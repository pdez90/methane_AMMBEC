# 18_gra2pes_boxsum_LOCAL.R ---------------------------------------------------
# RUN ON YOUR OWN MACHINE (the 'total' GRA2PES archives are ~7-20 GB, too big for
# the Cowork sandbox). Box-consistent GRA2PES emission rate (t/hr) over the
# Denver-metro box for a species: CO for the CH4:CO ratio anchor, CH4 for the
# beta-methane inventory comparison. Uses only base R + ncdf4 (a toolkit dep).
#
# First untar the tree. CAUTION: the all-species and methane-only archives BOTH
# expand to 202307/{weekdy,satdy,sundy}/ using IDENTICAL member filenames, so
# untarring one over the other silently overwrites it and the missing species
# then fail with 'No emission variable for ...'. Untar them into SEPARATE
# directories and pass the full month-dir path as argument 1:
#   mkdir -p allspec ch4only
#   tar xzf GRA2PESv1.1_total_202307.tar.gz         -C allspec   # all species (has CO)
#   tar xzf GRA2PESv1.1_total_202307_methane.tar.gz -C ch4only   # CH4 beta
# giving  <dir>/202307/{weekdy,satdy,sundy}/GRA2PESv1.1_total_202307_*_*Z.nc
# The all-species archive is ~21 GB compressed. To extract weekdays only (enough
# for a box/county spatial ratio, and far faster):
#   tar xzf GRA2PESv1.1_total_202307.tar.gz -C allspec '*weekdy*'
#
# Run: Rscript scripts/18_gra2pes_boxsum_LOCAL.R <path-to>/allspec/202307 CO
#      Rscript scripts/18_gra2pes_boxsum_LOCAL.R <path-to>/ch4only/202307 CH4
# Out: gra2pes_<SPECIES>_<month>_boxsum.csv  (hand this small file back)
#
# NOTE: GRA2PES stores emissions as moles km^-2 hr^-1 on a 4-km Lambert grid with
# vertical levels. If ncvar names differ, run once:
#   Rscript -e 'library(ncdf4); print(names(nc_open("202307/weekdy/<file>.nc")$var))'
# and set VAR/LATN/LONN below.
#
# VALIDATION CHECKLIST (E_CO_DENVER / E_CH4_GRA2PES scale linearly with these):
#   (a) CELL AREA: CELL_KM2 = 4*4 assumes exactly 4-km spacing; confirm from the
#       grid attributes (Lambert dx/dy), not the filename.
#   (b) DIMENSION ORDER: confirm the emission array's first two dims align with the
#       lat/lon arrays and the box mask (dim(layer) == dim(box_mask)).
#   (c) VERTICAL: confirm whether emission levels must be SUMMED (surface + elevated
#       stacks) or only the surface level is used.
#   (d) TIME WEIGHTING: each hourly slice is weighted by weekday/Sat/Sun day-counts.
#       This is a weighted MEAN across the 24-hour diurnal profile, valid only if
#       every day-type folder holds a complete nt == 24 cycle. Confirm nt == 24.
# Recommended guards once the variable names are confirmed on your machine:
#   stopifnot(nt == 24L); stopifnot(all(dim(layer) == dim(box_mask)))
# and write the units string + summed cell count into the output CSV.
# -----------------------------------------------------------------------------
suppressMessages(library(ncdf4))
# Write the result where the rest of the pipeline's output goes. This script is run
# by hand from the project root, so without this the CSV lands in the current
# directory and scripts/45_sync_anchors.R never finds it.
proj <- if (file.exists("config.R")) "." else ".."
if (file.exists(file.path(proj, "config.R"))) source(file.path(proj, "config.R"))
OUT <- if (exists("OUT_DIR")) OUT_DIR else "."
if (!nzchar(Sys.getenv("METHANE_OUT_DIR"))) {
  sib <- normalizePath(file.path(proj, "..", "outputs"), mustWork = FALSE)
  if (dir.exists(sib)) OUT <- sib
}
args <- commandArgs(trailingOnly = TRUE)
mdir <- if (length(args) >= 1) args[1] else "202307"
spec <- if (length(args) >= 2) args[2] else "CO"

LAT_S <- 39.50; LAT_N <- 39.95; LON_W <- -105.20; LON_E <- -104.55   # == URBAN_BOX
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

MW <- c(CO = 28.01, CH4 = 16.04, CO2 = 44.01)
CELL_KM2 <- 4 * 4

# Resolve the month directory no matter where the script is launched from: the
# untarred tree lives next to the archives (…/EmissionsInventory/<month>/).
mm <- basename(mdir)
cands <- c(mdir,
           # config.R already resolves INV_DIR (METHANE_INV_DIR, else DATA_DIR/EmissionsInventory).
           # Rebuilding it from the raw env var with "." as the fallback meant that with only
           # METHANE_DATA_DIR set, this script looked in ./<month> while script 36, doing the
           # same job, looked in the right place -- the bug class fixed in 16/17/37/38/39.
           if (exists("INV_DIR")) file.path(INV_DIR, mm),
           if (exists("INV_DIR")) file.path(INV_DIR, "GRA2PES", mm),
           file.path(Sys.getenv("METHANE_INV_DIR", unset = "."), mm),
           file.path("../EmissionsInventory", mm),
           file.path(Sys.getenv("HOME"), "MethaneData", "EmissionsInventory", mm))
# An explicit path is BINDING: never silently fall back to another month tree.
if (grepl("[/\\\\]", mdir) || dir.exists(mdir)) {
  if (!dir.exists(file.path(mdir, "weekdy")))
    stop("You passed an explicit month dir but it has no weekdy/ subdirectory:\n  ",
         normalizePath(mdir, mustWork = FALSE),
         "\nExtract the archive there first. Refusing to fall back to another tree.")
} else {
  hit <- cands[dir.exists(file.path(cands, "weekdy"))]
  if (!length(hit)) stop("Could not find <month>/weekdy in any of:\n  ",
                         paste(cands, collapse = "\n  "),
                         "\nPass the full path to the month dir, e.g. Rscript ... /full/path/202307 CO")
  mdir <- hit[1]
}
message("using month dir: ", normalizePath(mdir, mustWork = FALSE))
yr <- as.integer(substr(mm, 1, 4)); mo <- as.integer(substr(mm, 5, 6))
dts <- seq(as.Date(sprintf("%04d-%02d-01", yr, mo)), by = "day", length.out = 31)
dts <- dts[as.integer(format(dts, "%m")) == mo]
wdi <- as.integer(format(dts, "%u"))                      # 1=Mon .. 7=Sun
ndays <- c(weekdy = sum(wdi <= 5), satdy = sum(wdi == 6), sundy = sum(wdi == 7))

pick <- function(nc, cands) { for (c in cands) if (c %in% names(nc$var) || c %in% names(nc$dim)) return(c)
                              stop(paste("none of", paste(cands, collapse=","), "in", paste(names(nc$var), collapse=","))) }
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
# GRA2PES names species by code (e.g. methane = HC01), so we match on long_name.
LONGNM <- list(CO = "carbon monoxide", CH4 = "methane", CO2 = "carbon dioxide")
find_emis_var <- function(nc, spec) {
  if (spec %in% names(nc$var)) return(spec)
  pat <- LONGNM[[spec]] %||% tolower(spec)
  for (vn in names(nc$var)) {
    u  <- tolower(nc$var[[vn]]$units %||% "")
    ln <- tolower(nc$var[[vn]]$longname %||% "")
    if (grepl("mole", u) && grepl(pat, ln)) return(vn)
  }
  stop("No emission variable for '", spec, "' (long_name ~ '", pat, "'). Available:\n  ",
       paste(sapply(names(nc$var), function(v) paste0(v, " [", nc$var[[v]]$longname %||% "", "]")),
             collapse = "\n  "))
}
box_mask <- NULL; tot_moles_hr <- 0; total_weight <- 0
hours_by_dt <- list()                    # hourly slices seen per day type
emis_units <- NA_character_; emis_long <- NA_character_; nlev <- NA_integer_
dim_checked <- FALSE
for (dt in names(ndays)) {
  nd <- ndays[[dt]]
  files <- list.files(file.path(mdir, dt), pattern = "\\.nc$", full.names = TRUE)
  if (!length(files)) { message("  (no files for ", dt, ")"); next }
  for (fp in files) {
    nc <- nc_open(fp)
    vn <- find_emis_var(nc, spec)
    if (is.null(box_mask)) {                        # diagnostics on first file
      emis_units <- nc$var[[vn]]$units %||% NA_character_
      emis_long  <- nc$var[[vn]]$longname %||% NA_character_
      message("  matched species '", spec, "' -> variable '", vn,
              "' (", emis_long, ", ", emis_units, ")")
      message("  dims: ", paste(sapply(nc$var[[vn]]$dim, function(x) paste0(x$name, "=", x$len)), collapse = ", "))
      # UNIT GUARD: this script assumes a molar flux per area (moles km-2 hr-1).
      if (!grepl("mole", tolower(emis_units)))
        warning("emission units '", emis_units, "' do not look molar; the MW conversion assumes moles.")
      lat <- ncvar_get(nc, pick(nc, c("XLAT","lat","latitude","LAT","XLAT_M")))
      lon <- ncvar_get(nc, pick(nc, c("XLONG","lon","longitude","LON","XLONG_M")))
      if (max(lon, na.rm=TRUE) > 180) lon <- lon - 360
      box_mask <- lat >= LAT_S & lat <= LAT_N & lon >= LON_W & lon <= LON_E
      message("  box cells: ", sum(box_mask, na.rm = TRUE),
              if (sum(box_mask, na.rm=TRUE) == 0) "  <-- WARNING: box empty, check lon/lat orientation" else "")
      stopifnot(sum(box_mask, na.rm = TRUE) > 0)    # box must intersect the grid
      # CELL AREA from the grid metadata, not the filename: read the DX/DY global
      # attributes (meters) and require ~4 km, rather than assuming 16 km2 from
      # the filename.
      CELL_KM2 <- .set_cell(nc, lat, lon)
    }
    e <- ncvar_get(nc, vn)                 # (x, y, level, time), moles km-2 hr-1
    dd <- dim(e)
    if (length(dd) == 4) {                 # sum over the vertical levels (surface + stacks)
      nlev <- dd[3]
      es <- e[, , 1, ]; for (l in 2:dd[3]) es <- es + e[, , l, ]; e <- es
    } else if (is.na(nlev)) nlev <- 1L
    nt <- if (length(dim(e)) == 3) dim(e)[3] else 1
    if (!dim_checked) {                    # one-time structural guards
      layer0 <- if (nt > 1) e[, , 1] else e
      stopifnot(all(dim(layer0) == dim(box_mask)))     # grid aligns with lat/lon mask
      # GRA2PES splits each day type into 00to11Z and 12to23Z members, so 12 slices
      # per file and 24 per day type is the normal layout; a lone 24-slice file is
      # also fine. Anything else means the diurnal cycle is incomplete, and the
      # day-count weighting below would be wrong, so say so. The per-day-type total
      # is checked after the loop.
      if (!nt %in% c(12L, 24L))
        warning(sprintf("time slices per file = %d, expected 12 (half-day member) or 24.", nt))
      dim_checked <- TRUE
    }
    hours_by_dt[[dt]] <- (hours_by_dt[[dt]] %||% 0L) + nt
    for (h in seq_len(nt)) {
      layer <- if (nt > 1) e[, , h] else e
      tot_moles_hr <- tot_moles_hr + sum(layer[box_mask], na.rm = TRUE) * CELL_KM2 * nd
      total_weight <- total_weight + nd
    }
    nc_close(nc)
  }
}
# Each day type must contribute a complete 24-hour cycle, or the day-count weighting
# is averaging over a partial diurnal profile. A missing or truncated member shows up
# here rather than as a quietly wrong total.
for (dt in names(ndays)) {
  h <- hours_by_dt[[dt]] %||% 0L
  if (h == 0L) message("  NOTE: no files for ", dt, " (that day type is excluded)")
  else if (h != 24L)
    warning(sprintf("%s has %d hourly slices, not 24; the diurnal cycle is incomplete.", dt, h))
}

mean_moles_hr <- tot_moles_hr / total_weight
t_hr  <- mean_moles_hr * MW[[spec]] / 1e6
Gg_yr <- t_hr * 8766 / 1000
cat(sprintf("\nGRA2PES %s over Denver box, %s: %.2f t/hr  (%.1f Gg/yr)\n", spec, mdir, t_hr, Gg_yr))
cat(sprintf("  (units read from file: %s ; vertical levels summed: %s ; cell = %g km2)\n",
            emis_units, nlev, CELL_KM2))
write.csv(data.frame(species = spec, month = mm, emis_units = emis_units,
                     n_levels_summed = nlev, cell_km2 = CELL_KM2,
                     box_cells = sum(box_mask, na.rm = TRUE),
                     t_per_hr = round(t_hr, 3), Gg_per_yr = round(Gg_yr, 1)),
          file.path(OUT, sprintf("gra2pes_%s_%s_boxsum.csv", spec, mm)), row.names = FALSE)
cat("wrote", file.path(OUT, sprintf("gra2pes_%s_%s_boxsum.csv", spec, mm)), "\n")
