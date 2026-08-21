# 22_qc_robustness.R ----------------------------------------------------------
# Robustness of the York fossil fractions to the Schafer/Peischl (2025) data
# filters (daytime 10-17 MDT, >200 m AGL, in-PBL, in-box). For each urban flight
# it recomputes the fossil fraction on the full in-box urban-leg data and on the
# QC-filtered subset, and reports the difference. In AMMBEC these agree to within
# ~3 percentage points, i.e. the biogenic-leaning result is robust to selection.
#
# Out: <OUT_DIR>/qc_robustness.csv ; prints the max |Δ fossil %|.
# Run: Rscript scripts/22_qc_robustness.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "qc.R"))
source(file.path(proj, "R", "lidar_blh.R"))

foss <- function(dd) {
  x <- dd$CH4_ppb_enh; y <- dd$C2H6_ppb_enh; k <- is.finite(x) & is.finite(y) & x > 20
  if (sum(k) < 10) return(NA_real_)
  100 * fossil_fraction(york_slope(x[k], y[k], 1, 0.2)$slope, SOURCE_C2H6_CH4)
}
rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL); if (is.null(ic)) next
  if (!all(c("CH4_ppb","C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  legs <- tag_region(leg_metrics(d))
  du <- d[d$leg_id %in% legs$leg_id[legs$region == "urban"], ]
  if (nrow(du) < 50) next
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  blh <- tryCatch(blh_flight(as.character(ic$meta$date), min(du$timestamp),
                             max(du$timestamp), LIDAR_DIR)$blh_m, error = function(e) NA_real_)
  # Record whether the in-PBL ceiling was actually applied: if the lidar gives no
  # BLH for this flight, qc_filter() silently skips that one filter, so the "QC"
  # value then reflects only the box + daytime + >200 m AGL filters.
  pbl_applied <- is.finite(blh)
  duq <- qc_filter(du, blh = blh, box = URBAN_BOX, agl_min = QC_AGL_MIN,
                   day_start = QC_DAY[1], day_end = QC_DAY[2], utc_off_hours = QC_UTC_OFF)
  ff <- foss(du); fq <- if (nrow(duq) >= 50) foss(duq) else NA_real_
  rows[[p]] <- data.frame(
    flight = sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub(".ict", "", basename(p))),
    date = as.character(ic$meta$date),
    n_full = nrow(du), n_qc = nrow(duq), pbl_filter_applied = pbl_applied,
    fossil_full = round(ff), fossil_QC = round(fq),
    delta = round(abs(fq - ff)), stringsAsFactors = FALSE)
}
res <- do.call(rbind, rows); res <- res[order(res$date), ]
write.csv(res, file.path(OUT_DIR, "qc_robustness.csv"), row.names = FALSE)
print(res, row.names = FALSE)
dmax <- if (any(is.finite(res$delta))) max(res$delta, na.rm = TRUE) else NA
message("\nMax |delta fossil %| from QC filtering: ", dmax, " percentage points")
message("In-PBL ceiling applied on ", sum(res$pbl_filter_applied), " of ", nrow(res),
        " flights (rest: box + daytime + >200 m AGL only).")
