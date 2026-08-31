# 16_vulcan_co2_boxsum.R ------------------------------------------------------
# Box-consistent fossil-fuel CO2 emission rate over the Denver-metro box from the
# Vulcan v4.0 gridded product (Gurney et al.). Feeds E_CO2_DENVER in config.R for
# the CH4:CO2 enhancement-ratio method (script 15). R port of the extraction;
# needs the 'terra' package (install.packages("terra")) to read the GeoTIFF.
#
# Input : v4.tot.co2.usa.1km.lcc.mn.<YEAR>.tif  (1-km Lambert grid, tonnes C/km2/yr)
# Out   : <OUT_DIR>/vulcan_co2_boxsum.csv ; prints E_CO2_DENVER (Gg CO2/yr)
# Reproduces E_CO2_DENVER = 23,478 Gg CO2/yr (2022 file).
# Run   : Rscript scripts/16_vulcan_co2_boxsum.R /path/to/v4.tot.co2.usa.1km.lcc.mn.2022.tif
#
# VALIDATION CHECKLIST (confirm against Vulcan v4.0 docs before treating
# E_CO2_DENVER as calibrated; the emission magnitude scales linearly with each):
#   (a) UNITS: is the raster tonnes CARBON or tonnes CO2, and per km2 or per cell?
#       If carbon, the 44/12 factor must be applied; verify it is in the code below.
#   (b) TIME BASE: confirm the ".mn." filename token is an ANNUAL total (yr-1),
#       not a monthly or hourly mean. If not, the annual scaling is wrong.
#   (c) BOUNDARY: cells are summed as full 1-km squares; fractional overlap along
#       the projected box edge is neglected (small at 1 km, but noted).
# The layer name, units, and cell count are printed into the output CSV so a
# reader can audit these without re-opening the GeoTIFF.
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
suppressMessages(library(terra))

args <- commandArgs(trailingOnly = TRUE)
tif <- if (length(args)) args[1] else file.path(DATA_DIR, "EmissionsInventory",
                                                "v4.tot.co2.usa.1km.lcc.mn.2022.tif")
C_TO_CO2 <- 44.01 / 12.011

r    <- rast(tif)
# --- audit the raster's own metadata so units are not assumed silently ---
layer_name <- tryCatch(names(r)[1], error = function(e) NA_character_)
layer_unit <- tryCatch({ u <- units(r); if (length(u)) u[1] else NA_character_ },
                       error = function(e) NA_character_)
res_km     <- tryCatch(round(mean(res(r)) / 1000, 3), error = function(e) NA_real_)
cat(sprintf("layer name / unit (from file): %s / %s ; res ~ %s km\n",
            layer_name, layer_unit, res_km))
message("VALIDATION: confirm the above unit is tonnes C per km2 per YEAR. If it is ",
        "CO2 (not C), drop the 44/12 factor; if it is a monthly/mean value, rescale to annual.")

poly <- as.polygons(ext(URBAN_BOX$lon_w, URBAN_BOX$lon_e,
                        URBAN_BOX$lat_s, URBAN_BOX$lat_n), crs = "EPSG:4326")
poly <- project(poly, crs(r))                 # box -> Vulcan's Lambert grid
rc   <- crop(r, poly, mask = TRUE)
v    <- as.numeric(values(rc)); v <- v[is.finite(v) & v > 0]

stopifnot(length(v) > 0)                       # box must actually intersect the grid
EXP_CELLS <- 2755                              # ~box area in km2 (1-km grid); sanity only
if (abs(length(v) - EXP_CELLS) > 0.5 * EXP_CELLS)
  warning(sprintf("box cell count %d is far from the expected ~%d 1-km cells; check the box/projection.",
                  length(v), EXP_CELLS))

tC   <- sum(v)                                 # 1-km cells => tC/km2/yr * 1km2 = tC/yr
tCO2 <- tC * C_TO_CO2
cat(sprintf("file          : %s\n", basename(tif)))
cat(sprintf("box cells (km2): %d\n", length(v)))
cat(sprintf("total tCO2/yr  : %s\n", format(round(tCO2), big.mark = ",")))
cat(sprintf("E_CO2_DENVER   : %.1f Gg CO2/yr  (%.1f t/hr)\n", tCO2/1e3, tCO2/8766))
write.csv(data.frame(file = basename(tif), box_cells_km2 = length(v),
                     layer_name = layer_name, layer_unit = layer_unit, res_km = res_km,
                     C_to_CO2_applied = TRUE,
                     tCO2_yr = round(tCO2), Gg_CO2_yr = round(tCO2/1e3, 1)),
          file.path(OUT_DIR, "vulcan_co2_boxsum.csv"), row.names = FALSE)
