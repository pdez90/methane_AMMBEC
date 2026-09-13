# 16_vulcan_co2_boxsum.R ------------------------------------------------------
# Box-consistent fossil-fuel CO2 emission rate over the Denver-metro box from the
# Vulcan v4.0 gridded product (Gurney et al.). Feeds E_CO2_DENVER in config.R for
# the CH4:CO2 enhancement-ratio method (script 15). R port of the extraction;
# needs the 'terra' package (install.packages("terra")) to read the GeoTIFF.
#
# Input : v4.tot.co2.usa.1km.lcc.mn.<YEAR>.tif  (1-km Lambert grid, tonnes C/km2/yr)
# Out   : <OUT_DIR>/vulcan_co2_boxsum.csv ; prints E_CO2_DENVER (Gg CO2/yr)
# Reproduces E_CO2_DENVER = 23,622.3 Gg CO2/yr over 2,856 1-km cells (2022 file).
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
# VULCAN_FILE is config.R's resolved path (METHANE_VULCAN, else INV_DIR/<name>), which is
# what run_local.sh exports. This script used to rebuild the path itself from DATA_DIR,
# which ignored both the environment variable and METHANE_INV_DIR — so it looked in
# instruments/EmissionsInventory and failed even with the GeoTIFF present under
# inventories/Vulcan. An explicit command-line argument still wins.
tif <- if (length(args)) args[1] else VULCAN_FILE
C_TO_CO2 <- 44.01 / 12.011

if (!file.exists(tif))
  stop("Vulcan GeoTIFF not found: ", tif,
       "\nThe 2022 file lives inside v4.tot.co2.usa.1km.lcc.mn.allyrs.zip; run_local.sh",
       "\nextracts it. Point METHANE_VULCAN at it, or pass the path as an argument:",
       "\n  Rscript scripts/16_vulcan_co2_boxsum.R /path/to/v4.tot.co2.usa.1km.lcc.mn.2022.tif")

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
# TWO DIFFERENT COUNTS, previously conflated. The masked crop holds every 1-km cell
# inside the box, including cells with zero emissions; that count is the FOOTPRINT the
# sum covers, and it is what the manuscript means by "over N km2". Dropping the zeros
# first (which the old code did before counting) gives the number of EMITTING cells,
# a smaller and different quantity that was being published as an area. The emission
# total is identical either way, since the dropped cells contribute zero.
vall <- as.numeric(values(rc))
v_in <- vall[is.finite(vall)]                  # footprint: all cells in the masked box
v    <- v_in[v_in > 0]                         # emitting subset

stopifnot(length(v) > 0)                       # box must actually intersect the grid
# Expected footprint from the box geometry itself, rather than a hardcoded constant:
# 0.45 deg lat x 0.65 deg lon at the box's mid-latitude, on a 1-km grid.
EXP_CELLS <- round((URBAN_BOX$lat_n - URBAN_BOX$lat_s) * 110.95 *
                   (URBAN_BOX$lon_e - URBAN_BOX$lon_w) * 111.32 *
                   cos(mean(c(URBAN_BOX$lat_s, URBAN_BOX$lat_n)) * pi / 180))
if (abs(length(v_in) - EXP_CELLS) > 0.5 * EXP_CELLS)
  warning(sprintf("box footprint %d cells is far from the expected ~%d 1-km cells; check the box/projection.",
                  length(v_in), EXP_CELLS))

tC   <- sum(v)                                 # 1-km cells => tC/km2/yr * 1km2 = tC/yr
tCO2 <- tC * C_TO_CO2
cat(sprintf("file          : %s\n", basename(tif)))
cat(sprintf("box footprint  : %d cells (1 km each); expected ~%d from the box geometry\n",
            length(v_in), EXP_CELLS))
cat(sprintf("  of which emitting: %d cells (%.1f%%)\n",
            length(v), 100 * length(v) / length(v_in)))
cat(sprintf("total tCO2/yr  : %s\n", format(round(tCO2), big.mark = ",")))
cat(sprintf("E_CO2_DENVER   : %.1f Gg CO2/yr  (%.1f t/hr)\n", tCO2/1e3, tCO2/8766))
write.csv(data.frame(file = basename(tif),
                     box_cells_km2 = length(v_in),        # footprint (what "over N km2" means)
                     box_cells_emitting = length(v),      # subset with positive emissions
                     box_cells_expected = EXP_CELLS,
                     layer_name = layer_name, layer_unit = layer_unit, res_km = res_km,
                     C_to_CO2_applied = TRUE,
                     tCO2_yr = round(tCO2), Gg_CO2_yr = round(tCO2/1e3, 1)),
          file.path(OUT_DIR, "vulcan_co2_boxsum.csv"), row.names = FALSE)
