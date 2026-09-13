# 36_gra2pes_county_downscale.R ------------------------------------------------
# RUN ON YOUR OWN MACHINE (needs the large GRA2PES all-species tree; see script 18).
#
# CAUTION: the all-species and methane-only GRA2PES archives expand to the SAME
# 202307/{weekdy,satdy,sundy}/ member filenames, so one overwrites the other.
# Untar them into separate directories and pass the full month-dir path here.
# A methane-only tree exposes just HC01 (Methane) and every other species will
# fail with 'No emission variable for ...'.
#
# WHY. The manuscript anchors CH4:CO to GRA2PES CO summed over the Denver-metro
# BOX, and carries the EPA 2020 NEI seven-county CO total only as an upper-bound
# sensitivity case. Comparing a seven-county total against a box sum is not a
# like-for-like comparison: the county footprint is ~4x the box, so the NEI
# anchor over-scales a box-scale enhancement ratio. This script removes that
# mismatch by computing GRA2PES CO over BOTH footprints and using their ratio to
# downscale the NEI county total to the box:
#
#   E_CO_NEI_box = E_CO_NEI_7county * (GRA2PES CO in box / GRA2PES CO in counties)
#
# GRA2PES and the NEI use similar spatial surrogates, so the GRA2PES box/county
# ratio is a defensible spatial redistribution of the NEI total. The result is a
# SECOND box-consistent CO anchor rather than an upper bound.
#
# It also serves as the box/county sum tool for any other species (CO2, ethane),
# and a LIST mode that dumps every variable with its long name so that species
# codes (GRA2PES names species by code, e.g. methane = HC01) can be identified.
#
# Run:  Rscript scripts/36_gra2pes_county_downscale.R <path-to>/allspec/202307 LIST
#       Rscript scripts/36_gra2pes_county_downscale.R <path-to>/allspec/202307 CO
#       Rscript scripts/36_gra2pes_county_downscale.R <path-to>/allspec/202307 CO2
# Out:  <OUT_DIR>/gra2pes_<SPEC>_<month>_county_box.csv
#
# Inherits every unit and structural guard from script 18 (molar units, vertical
# level summing, DX/DY cell area, 24-hour day-type weighting).
# -----------------------------------------------------------------------------
suppressMessages(library(ncdf4))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))

args <- commandArgs(trailingOnly = TRUE)
mdir <- if (length(args) >= 1) args[1] else "202307"
spec <- if (length(args) >= 2) args[2] else "CO"

# CELL AREA. ncatt_get() returns list(hasatt=FALSE, value=0) for a MISSING
# attribute rather than erroring, so a bare tryCatch()$value silently yields 0
# and every emission total collapses to 0.000 t/hr. Check hasatt, and otherwise
# derive the spacing from the lat/lon centres, which are always present.
.cell_km2 <- function(nc, lat, lon) {
  ax <- ncatt_get(nc, 0, "DX"); ay <- ncatt_get(nc, 0, "DY")
  if (isTRUE(ax$hasatt) && isTRUE(ay$hasatt) &&
      is.finite(ax$value) && is.finite(ay$value) &&
      ax$value > 0 && ay$value > 0)
    return(list(km2 = ax$value * ay$value / 1e6, src = "DX/DY global attributes"))
  hav <- function(lo1, la1, lo2, la2) {           # great-circle km
    R <- 6371.0088; p <- pi / 180
    a <- sin((la2 - la1) * p / 2)^2 +
         cos(la1 * p) * cos(la2 * p) * sin((lo2 - lo1) * p / 2)^2
    2 * R * asin(pmin(1, sqrt(a)))
  }
  if (length(dim(lat)) != 2L)
    stop("lat/lon are not 2-D, cannot derive the grid spacing.")
  i <- max(1L, floor(nrow(lat) / 2)); j <- max(1L, floor(ncol(lat) / 2))
  dxk <- hav(lon[i, j], lat[i, j], lon[i + 1, j], lat[i + 1, j])
  dyk <- hav(lon[i, j], lat[i, j], lon[i, j + 1], lat[i, j + 1])
  list(km2 = dxk * dyk,
       src = sprintf("lat/lon centre spacing (%.3f x %.3f km)", dxk, dyk))
}
NOMINAL_KM <- 4   # GRA2PES v1.1 Lambert grid spacing, by definition
.set_cell <- function(nc, lat, lon) {
  ca <- .cell_km2(nc, lat, lon)
  edge <- sqrt(ca$km2)
  # HARD STOP, not a warning: every emission total scales linearly with this,
  # so a wrong area silently rescales the published anchor.
  if (!is.finite(ca$km2) || ca$km2 <= 0 || abs(edge - NOMINAL_KM) > 0.2)
    stop("Grid cell edge is ", signif(edge, 4), " km, not ~", NOMINAL_KM,
         " km. Refusing to continue, since every emission total scales ",
         "linearly with the cell area.")
  if (grepl("attributes", ca$src, fixed = TRUE)) {      # exact, projected metres
    message("  cell = ", signif(ca$km2, 6), " km2  [", ca$src, "]")
    return(ca$km2)
  }
  # The grid is exactly NOMINAL_KM square in PROJECTED space. Great-circle
  # spacing between cell CENTRES is inflated by the Lambert map scale factor
  # (~1.005 away from the standard parallels), so the derived value VALIDATES
  # the nominal spacing rather than replacing it. Adopting it directly would
  # rescale every published anchor by that scale factor squared (~1.1%).
  message("  cell = ", NOMINAL_KM^2, " km2  [nominal ", NOMINAL_KM, " km grid; ",
          ca$src, " agrees to ", sprintf("%.2f%%", 100 * abs(edge / NOMINAL_KM - 1)),
          ", the Lambert map scale factor]")
  NOMINAL_KM^2
}

# HC02 is GRA2PES's "Ethane+Alkanes with k(OH)<500" lumped class. Propane
# (HC45), butanes (HC39) and pentanes (HC40) are carried SEPARATELY, so HC02 is
# plausibly ethane-dominated, but it is NOT pure ethane. Treat any HC02-derived
# number as an UPPER BOUND on GRA2PES ethane, never as an ethane measurement.
# INVENTORY VERSION TAG. GRA2PES filenames carry the version, e.g.
# GRA2PESv1.1_total_202307_weekdy_00to11Z.nc or GRA2PESv2.0beta_WASTE_...
# Output files and tables are tagged with it so a v2 run can never silently
# overwrite a v1 result, and so every CSV records which inventory produced it.
.gra_version <- function(mdir, daytype = "weekdy") {
  f <- list.files(file.path(mdir, daytype), pattern = "\\.nc$")
  if (!length(f)) return("unknown")
  m <- regmatches(f[1], regexpr("GRA2PES(v[0-9][0-9.]*[A-Za-z]*)", f[1]))
  if (!length(m)) return("unknown")
  sub("^GRA2PES", "", m)
}

MW <- c(CO = 28.01, CH4 = 16.04, CO2 = 44.01, ETHANE = 30.07, HC02 = 30.07)
CELL_KM2 <- 4 * 4

## ---- seven-county outline ---------------------------------------------------
outl <- NULL
for (cand in c(file.path(proj, "denver7_metro_outline.csv"),
               file.path(INV_DIR, "denver7_metro_outline.csv"),
               normalizePath(file.path(proj, "..", "inventories",
                                       "denver7_metro_outline.csv"), mustWork = FALSE),
               file.path(Sys.getenv("HOME"), "MethaneData", "EmissionsInventory",
                         "denver7_metro_outline.csv"))) {
  if (file.exists(cand)) { outl <- read.csv(cand, stringsAsFactors = FALSE); break }
}
if (is.null(outl)) stop("denver7_metro_outline.csv not found (needed for the county footprint).")
if (!"part" %in% names(outl)) outl$part <- 1L
parts <- split(outl, outl$part)
message("county outline: ", length(parts), " part(s), ", nrow(outl), " vertices")

# Ray-casting point-in-polygon, vectorised over POINTS and looped over vertices.
# A point counts as inside if it falls inside ANY part (the parts are separate
# county polygons, not holes).
.pip_part <- function(px, py, vx, vy) {
  n <- length(vx); inside <- rep(FALSE, length(px)); j <- n
  for (i in seq_len(n)) {
    dy <- vy[j] - vy[i]
    cond <- ((vy[i] > py) != (vy[j] > py)) &
            (px < ifelse(dy == 0, -Inf, (vx[j] - vx[i]) * (py - vy[i]) / dy + vx[i]))
    cond[is.na(cond)] <- FALSE
    inside <- xor(inside, cond)
    j <- i
  }
  inside
}
in_counties <- function(px, py) {
  keep <- rep(FALSE, length(px))
  for (p in parts) keep <- keep | .pip_part(px, py, p$lon, p$lat)
  keep
}

## ---- locate the month tree (same resolution logic as script 18) -------------
mm <- basename(mdir)
# PATH RESOLUTION. If the caller passed an explicit path (anything containing a
# separator, or an existing directory), that path is BINDING: it must contain a
# weekdy/ subdirectory or we stop. Silently falling back to a different month
# tree is how a run can report numbers from the wrong archive, for example the
# methane-only tree when the all-species tree was intended.
explicit <- grepl("[/\\\\]", mdir) || dir.exists(mdir)
if (explicit) {
  if (!dir.exists(file.path(mdir, "weekdy")))
    stop("You passed an explicit month dir but it has no weekdy/ subdirectory:\n  ",
         normalizePath(mdir, mustWork = FALSE),
         "\nExtract the archive there first, or pass a bare month name (e.g. 202307)",
         "\nto use the search path. Refusing to fall back to another tree.")
} else {
  cands <- c(mdir, file.path(INV_DIR, mm), file.path("../EmissionsInventory", mm),
             file.path(Sys.getenv("HOME"), "MethaneData", "EmissionsInventory", mm))
  hit <- cands[dir.exists(file.path(cands, "weekdy"))]
  if (!length(hit)) stop("Could not find <month>/weekdy in:\n  ", paste(cands, collapse = "\n  "))
  mdir <- hit[1]
}
message("using month dir: ", normalizePath(mdir, mustWork = FALSE))
# PROVENANCE GUARD. A methane-only tree carries a single emission variable
# (HC01). Anything else requested against it would otherwise fail deep in the
# read loop with a bare 'No emission variable' message; say why up front.
.probe <- list.files(file.path(mdir, "weekdy"), pattern = "\\.nc$", full.names = TRUE)
if (length(.probe)) {
  .nc <- nc_open(.probe[1])
  .nsp <- sum(grepl("mole", tolower(sapply(names(.nc$var),
            function(v) .nc$var[[v]]$units %||% ""))))
  nc_close(.nc)
  if (.nsp <= 1L && !toupper(spec) %in% c("LIST", "CH4"))
    stop("This tree exposes only ", .nsp, " emission species, so it is the ",
         "methane-only archive. '", spec, "' is not in it. Extract ",
         "GRA2PESv1.1_total_<month>.tar.gz into a separate directory and pass ",
         "that path.")
}

yr <- as.integer(substr(mm, 1, 4)); mo <- as.integer(substr(mm, 5, 6))
dts <- seq(as.Date(sprintf("%04d-%02d-01", yr, mo)), by = "day", length.out = 31)
dts <- dts[as.integer(format(dts, "%m")) == mo]
wdi <- as.integer(format(dts, "%u"))
ndays <- c(weekdy = sum(wdi <= 5), satdy = sum(wdi == 6), sundy = sum(wdi == 7))

pick <- function(nc, cs) { for (c in cs) if (c %in% names(nc$var) || c %in% names(nc$dim)) return(c)
                           stop("none of ", paste(cs, collapse = ",")) }
LONGNM <- list(CO = "carbon monoxide", CH4 = "methane", CO2 = "carbon dioxide", ETHANE = "ethane")
# Match a species to its emission variable, preferring the PUREST match. A future
# GRA2PES release may carry both a pure "Ethane" variable and a lumped
# "Ethane+Alkanes..." class; picking whichever came first would silently anchor
# to the wrong one. Preference order among molar variables whose long_name
# contains the pattern: (1) exact long_name == pattern, (2) a non-lumped match
# (no "+", "alkanes", or "lumped"), (3) any match, which is then flagged lumped.
.is_lumped_ln <- function(ln) grepl("\\+|alkanes|lumped", ln)
find_emis_var <- function(nc, spec) {
  if (spec %in% names(nc$var)) return(spec)
  pat <- LONGNM[[spec]] %||% tolower(spec)
  exact <- NA_character_; pure <- NA_character_; any_hit <- NA_character_
  for (vn in names(nc$var)) {
    u <- tolower(nc$var[[vn]]$units %||% ""); ln <- tolower(nc$var[[vn]]$longname %||% "")
    if (!grepl("mole", u) || !grepl(pat, ln)) next
    if (is.na(any_hit)) any_hit <- vn
    if (identical(ln, pat) && is.na(exact)) exact <- vn
    if (!.is_lumped_ln(ln) && is.na(pure)) pure <- vn
  }
  vn <- if (!is.na(exact)) exact else if (!is.na(pure)) pure else any_hit
  if (is.na(vn)) stop("No emission variable for '", spec, "'.")
  vn
}

f1 <- list.files(file.path(mdir, "weekdy"), pattern = "\\.nc$", full.names = TRUE)[1]
if (is.na(f1)) stop("no .nc files under ", file.path(mdir, "weekdy"))

## ---- LIST mode: dump every variable so species codes can be identified ------
if (toupper(spec) == "LIST") {
  nc <- nc_open(f1)
  cat(sprintf("\nVariables in %s\n\n", basename(f1)))
  for (vn in names(nc$var))
    cat(sprintf("  %-12s %-34s %s\n", vn,
                substr(nc$var[[vn]]$longname %||% "", 1, 34),
                nc$var[[vn]]$units %||% ""))
  nc_close(nc)
  cat("\nLook for an ethane long_name to enable the ethane anchor.\n")
  quit(save = "no")
}

## ---- accumulate over both footprints ---------------------------------------
LUMPED <- FALSE
box_mask <- NULL; cty_mask <- NULL
tot_box <- 0; tot_cty <- 0; wt <- 0
emis_units <- NA_character_; nlev <- NA_integer_; dim_checked <- FALSE

# Tree-wide reference size for the truncation check below.
REF_NC_BYTES <- max(file.info(list.files(mdir, pattern = "\\.nc$",
                                         full.names = TRUE, recursive = TRUE))$size,
                    na.rm = TRUE)

DAYTYPE_36 <- "weekdy"
slices_seen <- list()
for (dt in names(ndays)) {
  nd <- ndays[[dt]]
  files <- list.files(file.path(mdir, dt), pattern = "\\.nc$", full.names = TRUE)
  # Skip obviously truncated members (an interrupted untar leaves short files).
  # The reference size is the largest member ACROSS THE WHOLE TREE, not within
  # this one day-type folder: a folder holding a single partial file would
  # otherwise measure it against itself and pass it through.
  if (length(files)) {
    .bad <- file.info(files)$size < 0.5 * REF_NC_BYTES
    .bad[is.na(.bad)] <- TRUE
    if (any(.bad)) {
      warning("skipping ", sum(.bad), " truncated file(s) in ", dt, ": ",
              paste(basename(files[.bad]), collapse = ", "),
              " (re-extract the archive to include them)")
      files <- files[!.bad]
    }
  }
  if (!length(files)) next
  for (fp in files) {
    nc <- nc_open(fp)
    vn <- find_emis_var(nc, spec)
    if (is.null(box_mask)) {
      emis_units <- nc$var[[vn]]$units %||% NA_character_
      message("  species '", spec, "' -> '", vn, "' (", nc$var[[vn]]$longname %||% "", ", ", emis_units, ")")
      .ln <- nc$var[[vn]]$longname %||% ""
      LUMPED <<- grepl("\\+|alkanes|lumped", tolower(.ln))
      if (LUMPED) {
        message("")
        message("  *** LUMPED SPECIES WARNING ***")
        message("  '", vn, "' is '", .ln, "', a lumped mechanism class, not a pure species.")
        message("  Every number below is an UPPER BOUND on the pure species, not a measurement of it.")
        message("  The output CSV records this as lumped_class = TRUE.")
        message("")
      }
      if (!grepl("mole", tolower(emis_units)))
        warning("units '", emis_units, "' do not look molar; MW conversion assumes moles.")
      lat <- ncvar_get(nc, pick(nc, c("XLAT","lat","latitude","LAT","XLAT_M")))
      lon <- ncvar_get(nc, pick(nc, c("XLONG","lon","longitude","LON","XLONG_M")))
      if (max(lon, na.rm = TRUE) > 180) lon <- lon - 360
      box_mask <- lat >= URBAN_BOX$lat_s & lat <= URBAN_BOX$lat_n &
                  lon >= URBAN_BOX$lon_w & lon <= URBAN_BOX$lon_e
      # county mask: restrict to the outline's bounding box FIRST, then ray-cast,
      # so point-in-polygon runs over ~1e3 candidate cells, not the whole CONUS grid.
      bb <- c(min(outl$lon), max(outl$lon), min(outl$lat), max(outl$lat))
      cand <- lon >= bb[1] & lon <= bb[2] & lat >= bb[3] & lat <= bb[4]
      cand[is.na(cand)] <- FALSE
      cty_mask <- array(FALSE, dim = dim(lat))
      idx <- which(cand)
      message("  candidate cells in county bbox: ", length(idx))
      cty_mask[idx] <- in_counties(lon[idx], lat[idx])
      message("  box cells: ", sum(box_mask, na.rm = TRUE),
              " | county cells: ", sum(cty_mask, na.rm = TRUE))
      stopifnot(sum(box_mask, na.rm = TRUE) > 0, sum(cty_mask, na.rm = TRUE) > 0)
      CELL_KM2 <- .set_cell(nc, lat, lon)
    }
    e <- ncvar_get(nc, vn); dd <- dim(e)
    if (length(dd) == 4) { nlev <- dd[3]; es <- e[, , 1, ]; for (l in 2:dd[3]) es <- es + e[, , l, ]; e <- es }
    else if (is.na(nlev)) nlev <- 1L
    nt <- if (length(dim(e)) == 3) dim(e)[3] else 1
    if (!dim_checked) {
      layer0 <- if (nt > 1) e[, , 1] else e
      stopifnot(all(dim(layer0) == dim(box_mask)))
      dim_checked <- TRUE
    }
    for (h in seq_len(nt)) {
      layer <- if (nt > 1) e[, , h] else e
      tot_box <- tot_box + sum(layer[box_mask], na.rm = TRUE) * CELL_KM2 * nd
      tot_cty <- tot_cty + sum(layer[cty_mask], na.rm = TRUE) * CELL_KM2 * nd
      wt <- wt + nd
    }
    slices_seen[[dt]] <- (slices_seen[[dt]] %||% 0L) + nt
    nc_close(nc)
  }
}

# Each day-type is archived as 00to11Z + 12to23Z, so 12 slices per file is
# correct; what must hold is 24 slices per day-type in total.
for (.dt in names(slices_seen)) {
  .n <- slices_seen[[.dt]]
  if (!identical(as.integer(.n), 24L))
    warning(sprintf("%s has %d hourly slices, not 24; the day-count weighting assumes a full diurnal cycle.",
                    .dt, .n))
}

mw <- MW[[spec]] %||% NA_real_
box_t_hr <- (tot_box / wt) * mw / 1e6
cty_t_hr <- (tot_cty / wt) * mw / 1e6
frac <- box_t_hr / cty_t_hr

cat(sprintf("\n=== GRA2PES %s, %s ===\n", spec, mm))
cat(sprintf("  Denver-metro box   : %8.3f t/hr  (%.1f Gg/yr)\n", box_t_hr, box_t_hr * 8766 / 1000))
cat(sprintf("  seven-county metro : %8.3f t/hr  (%.1f Gg/yr)\n", cty_t_hr, cty_t_hr * 8766 / 1000))
cat(sprintf("  box / county       : %8.3f   (box holds %.0f%% of the county total)\n", frac, 100 * frac))

out <- data.frame(species = spec, month = mm, emis_units = emis_units,
                  n_levels_summed = nlev, cell_km2 = CELL_KM2, lumped_class = LUMPED,
                  box_cells = sum(box_mask, na.rm = TRUE),
                  county_cells = sum(cty_mask, na.rm = TRUE),
                  box_t_per_hr = round(box_t_hr, 4),
                  county_t_per_hr = round(cty_t_hr, 4),
                  box_over_county = round(frac, 4),
                  stringsAsFactors = FALSE)

if (spec == "CO" && exists("E_CO_NEI")) {
  nei_box_Gg <- E_CO_NEI * frac
  out$NEI_7county_Gg_yr <- E_CO_NEI
  out$NEI_downscaled_box_Gg_yr <- round(nei_box_Gg, 2)
  out$GRA2PES_box_Gg_yr <- round(box_t_hr * 8766 / 1000, 2)
  cat(sprintf("\n  NEI seven-county CO      : %.1f Gg/yr\n", E_CO_NEI))
  cat(sprintf("  NEI downscaled to box    : %.1f Gg/yr   <-- box-consistent second CO anchor\n", nei_box_Gg))
  cat(sprintf("  GRA2PES box CO (primary) : %.1f Gg/yr\n", box_t_hr * 8766 / 1000))
  cat(sprintf("  NEI/GRA2PES at box scale : %.2f  (a like-for-like inventory spread)\n",
              nei_box_Gg / (box_t_hr * 8766 / 1000)))
  cat("\n  Set E_CO_NEI_BOX in config.R to the downscaled value to use it as an anchor.\n")
}

VER <- .gra_version(mdir, DAYTYPE_36)
out$inventory_version <- VER
.fn <- sprintf("gra2pes_%s_%s_%s_county_box.csv", spec, mm, VER)
write.csv(out, file.path(OUT_DIR, .fn), row.names = FALSE)
message("\nWrote ", file.path(OUT_DIR, .fn), "   [inventory version: ", VER, "]")
