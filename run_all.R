# run_all.R ------------------------------------------------------------------
# Reproducible end-to-end run. From the project root:
#   Rscript run_all.R
# Set METHANE_DATA_DIR / METHANE_OUT_DIR env vars, or edit config.R, first.
#
# Required stages STOP the run on failure (so the pipeline never claims success
# when a core analysis errored). Optional stages (ancillary datasets, figures)
# only warn and continue. A final summary lists any stages that failed.
# -----------------------------------------------------------------------------
if (!file.exists("config.R"))
  stop("run_all.R must be run from the project root (where config.R lives).")
source("config.R")                       # make config constants available up front

.fail <- character(0)
run_step <- function(label, script, required = TRUE) {
  message("== ", label, " ==")
  tryCatch(
    source(script, local = new.env(parent = globalenv())),
    error = function(e) {
      msg <- sprintf("%s FAILED: %s", label, conditionMessage(e))
      .fail[[length(.fail) + 1L]] <<- label
      if (required) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
    })
}

run_step("01 inventory",                 "scripts/01_inventory.R")
run_step("02 flight ethane:methane",     "scripts/02_flight_ethane_methane.R")
run_step("03 mobile hotspots",           "scripts/03_mobile_hotspots.R", required = FALSE)
run_step("04 aircraft/mobile overlap",   "scripts/04_aircraft_mobile_overlap.R", required = FALSE)
# Scripts 05 (curtain "mass-balance" flux) and 09 (campaign summary that merges
# those fluxes) are NOT part of the manuscript pipeline. No flight supports a valid
# closed-loop/curtain flux (script 13), so their numerical fluxes are not reported
# and nothing downstream reads them. They are retained under scripts/ for reference
# but excluded from the reproducible run to avoid regenerating rejected results.
message("== 05 mass-balance flux == SKIPPED (curtain flux not used in the manuscript; see scripts/05 header)")
run_step("06 mobile trends",             "scripts/06_mobile_trends.R", required = FALSE)
if (dir.exists(MOBILELAB_DIR)) run_step("07 mobile lab", "scripts/07_mobilelab.R", required = FALSE) else message("== 07 mobile lab == skipped (no MobileLab dir)")
if (file.exists(VELSTATS_FILE)) run_step("08 lidar BLH", "scripts/08_lidar_blh.R", required = FALSE) else message("== 08 lidar BLH == skipped (no velStats file)")
message("== 09 combined summary == SKIPPED (merges curtain fluxes not used in the manuscript)")
run_step("11 aircraft urban flux",       "scripts/11_urban_flux.R")
run_step("12 urban figures",             "scripts/12_urban_figures.R", required = FALSE)
run_step("13 closed-loop diagnostic",    "scripts/13_closeloop_diagnostic.R")
if (file.exists(GHGI_FILE)) run_step("14 inventory comparison", "scripts/14_inventory_comparison.R", required = FALSE) else message("== 14 inventory comparison == skipped (no GHGI file)")
# Inventory anchors for script 15 come from prep scripts 16/17/18 (run once on the
# downloaded inventory files; the resulting numbers are baked into config.R).
run_step("15 CH4:CO/CO2 ratio-to-inventory", "scripts/15_ratio_method.R")
run_step("19 wind vs fossil fraction",   "scripts/19_wind_fossil.R", required = FALSE)
if (requireNamespace("terra", quietly = TRUE) && file.exists(VULCAN_FILE)) run_step("20 basemap figures", "scripts/20_basemap_figures.R", required = FALSE) else message("== 20 basemap figures == skipped (needs terra + Vulcan tif)")
run_step("21 manuscript Table 1 + emission-estimate table", "scripts/21_tables.R")
run_step("22 QC robustness",             "scripts/22_qc_robustness.R", required = FALSE)
run_step("23 source attribution",        "scripts/23_source_attribution.R", required = FALSE)
run_step("24 Suncor/Robert Hite facility-separation scan", "scripts/24_facility_separation_scan.R", required = FALSE)
run_step("25 fossil-signature map",      "scripts/25_fossil_signature_map.R", required = FALSE)
run_step("26 mobile CDPHE plume figure", "scripts/26_mobile_plume_figure.R", required = FALSE)
run_step("27 beta_source sensitivity",   "scripts/27_beta_sensitivity.R", required = FALSE)
run_step("28 loop-closure diagnostic figure", "scripts/28_loop_closure_figure.R", required = FALSE)
run_step("29 regression-estimator comparison", "scripts/29_regression_comparison.R", required = FALSE)
if (requireNamespace("png", quietly = TRUE)) run_step("30 combined source figure (Fig 4A/4B)", "scripts/30_combined_source_figure.R", required = FALSE) else message("== 30 combined source figure == skipped (needs png)")

# Record the exact environment for reproducibility.
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "sessionInfo.txt"))
if (length(.fail)) {
  message("\nDone WITH WARNINGS. Optional stages that failed: ", paste(.fail, collapse = ", "),
          "\nOutputs in: ", OUT_DIR)
} else {
  message("\nAll done (all required stages succeeded). Outputs in: ", OUT_DIR)
}
