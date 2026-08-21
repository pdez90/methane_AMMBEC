# 18_gra2pes_boxsum_LOCAL.R ---------------------------------------------------
# RUN ON YOUR OWN MACHINE (the 'total' GRA2PES archives are ~7-20 GB, too big for
# the Cowork sandbox). Box-consistent GRA2PES emission rate (t/hr) over the
# Denver-metro box for a species: CO for the CH4:CO ratio anchor, CH4 for the
# beta-methane inventory comparison. Uses only base R + ncdf4 (a toolkit dep).
#
# First untar the 'total' tree, e.g.:
#   tar xzf GRA2PESv1.1_total_202307.tar.gz            # all species (has CO)
#   tar xzf GRA2PESv1.1_total_202307_methane.tar.gz    # CH4 beta
# giving  202307/{weekdy,satdy,sundy}/GRA2PESv1.1_total_202307_*_*Z.nc
#
# Run: Rscript scripts/18_gra2pes_boxsum_LOCAL.R 202307 CO
#      Rscript scripts/18_gra2pes_boxsum_LOCAL.R 202307 CH4
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
args <- commandArgs(trailingOnly = TRUE)
mdir <- if (length(args) >= 1) args[1] else "202307"
spec <- if (length(args) >= 2) args[2] else "CO"

LAT_S <- 39.50; LAT_N <- 39.95; LON_W <- -105.20; LON_E <- -104.55   # == URBAN_BOX
MW <- c(CO = 28.01, CH4 = 16.04, CO2 = 44.01)
CELL_KM2 <- 4 * 4

# Resolve the month directory no matter where the script is launched from: the
# untarred tree lives next to the archives (…/EmissionsInventory/<month>/).
mm <- basename(mdir)
cands <- c(mdir,
           file.path(Sys.getenv("METHANE_INV_DIR", unset = "."), mm),
           file.path("../EmissionsInventory", mm),
           file.path(Sys.getenv("HOME"), "MethaneData", "EmissionsInventory", mm))
hit <- cands[dir.exists(file.path(cands, "weekdy"))]
if (!length(hit)) stop("Could not find <month>/weekdy in any of:\n  ",
                       paste(cands, collapse = "\n  "),
                       "\nPass the full path to the month dir, e.g. Rscript ... /full/path/202307 CO")
mdir <- hit[1]; message("using month dir: ", mdir)
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
      # attributes (metres) and require ~4 km, rather than assuming 16 km2 from
      # the filename.
      dx <- tryCatch(ncatt_get(nc, 0, "DX")$value, error = function(e) NA_real_)
      dy <- tryCatch(ncatt_get(nc, 0, "DY")$value, error = function(e) NA_real_)
      if (is.finite(dx) && is.finite(dy)) {
        CELL_KM2 <- dx * dy / 1e6
        message(sprintf("  grid spacing from file: DX=%.0f m, DY=%.0f m -> cell = %.3f km2", dx, dy, CELL_KM2))
        if (abs(sqrt(CELL_KM2) - 4) > 0.2)
          warning(sprintf("GRA2PES cell edge %.2f km is not ~4 km; using the file value %.2f km2.", sqrt(CELL_KM2), CELL_KM2))
      } else message("  DX/DY global attributes not found; using assumed CELL_KM2 = ", CELL_KM2,
                     " km2 (VERIFY against the grid definition).")
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
      if (nt != 24L)
        warning(sprintf("time slices per file = %d, not 24; the day-count weighting assumes a full 24-h diurnal cycle.", nt))
      dim_checked <- TRUE
    }
    for (h in seq_len(nt)) {
      layer <- if (nt > 1) e[, , h] else e
      tot_moles_hr <- tot_moles_hr + sum(layer[box_mask], na.rm = TRUE) * CELL_KM2 * nd
      total_weight <- total_weight + nd
    }
    nc_close(nc)
  }
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
          sprintf("gra2pes_%s_%s_boxsum.csv", spec, mm), row.names = FALSE)
