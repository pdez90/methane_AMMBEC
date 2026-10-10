# 22_qc_robustness.R ----------------------------------------------------------
# Robustness of the York fossil fractions to the Schafer/Peischl (2025) data
# filters (daytime 10-17 MDT, >200 m AGL, in-PBL, in-box). For each urban flight
# it recomputes the fossil fraction on the full in-box urban-leg data and on the
# QC-filtered subset, and reports the difference. The boundary-layer ceiling is the
# airborne-lidar BLH along the track at each sample (R/airborne_blh.R; the flight's
# in-box airborne median where a sample has no nearby retrieval, the ground lidar for
# flights without an airborne file); the same filter with the ground-lidar mixing
# height is kept as columns *_ground for comparison.
#
# Out: <OUT_DIR>/qc_robustness.csv ; prints the max |Δ fossil %|.
# Run: Rscript scripts/22_qc_robustness.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "qc.R"))
source(file.path(proj, "R", "lidar_blh.R")); source(file.path(proj, "R", "airborne_blh.R"))

foss <- function(dd) {
  x <- dd$CH4_ppb_enh; y <- dd$C2H6_ppb_enh; k <- is.finite(x) & is.finite(y) & x > 20
  if (sum(k) < 10) return(NA_real_)
  100 * fossil_fraction(fossil_slope_fit(x[k], y[k], dd$leg_id[k], 1, 0.2)$slope, SOURCE_C2H6_CH4)
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
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub(".ict", "", basename(p)))
  t0 <- min(du$timestamp); t1 <- max(du$timestamp)
  fb <- tryCatch(flight_mixing_height(fl, as.character(ic$meta$date), t0, t1, box = URBAN_BOX), error = function(e) list(blh_m = NA_real_, src = "none"))
  loc <- if (identical(BLH_SOURCE, "airborne")) airborne_blh_at(fl, du$timestamp) else rep(NA_real_, nrow(du))
  blh <- ifelse(is.finite(loc), loc, fb$blh_m)
  blh_g <- tryCatch(ground_mixing_height(as.character(ic$meta$date), t0, t1)$blh_m, error = function(e) NA_real_)
  # Record whether the in-PBL ceiling was actually applied: with no BLH for this flight
  # qc_filter() skips that one filter, so the "QC" value then reflects only the box +
  # daytime + >200 m AGL filters.
  pbl_applied <- any(is.finite(blh))
  qcf <- function(b) qc_filter(du, blh = b, box = URBAN_BOX, agl_min = QC_AGL_MIN,
                               day_start = QC_DAY[1], day_end = QC_DAY[2], utc_off_hours = QC_UTC_OFF)
  duq <- qcf(blh); duqg <- qcf(blh_g)
  ff <- foss(du); fq <- if (nrow(duq) >= 50) foss(duq) else NA_real_; fqg <- if (nrow(duqg) >= 50) foss(duqg) else NA_real_
  above <- is.finite(du$ALTAGL) & is.finite(blh) & du$ALTAGL >= blh
  rows[[p]] <- data.frame(
    flight = sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub(".ict", "", basename(p))),
    date = as.character(ic$meta$date),
    n_full = nrow(du), n_qc = nrow(duq), pbl_filter_applied = pbl_applied, blh_src = fb$src,
    blh_inbox_m = round(fb$blh_m), share_above_blh = round(mean(above), 3),
    fossil_full = round(ff), fossil_QC = round(fq), delta = round(abs(fq - ff)),
    blh_ground_m = round(blh_g), n_qc_ground = nrow(duqg), fossil_QC_ground = round(fqg), delta_ground = round(abs(fqg - ff)),
    stringsAsFactors = FALSE)
}
res <- do.call(rbind, rows); res <- res[order(res$date), ]
write.csv(res, file.path(OUT_DIR, "qc_robustness.csv"), row.names = FALSE)
print(res, row.names = FALSE)
dmax <- if (any(is.finite(res$delta))) max(res$delta, na.rm = TRUE) else NA
message("\nMax |delta fossil %| from QC filtering: ", dmax, " percentage points")
message("In-PBL ceiling applied on ", sum(res$pbl_filter_applied), " of ", nrow(res),
        " flights (rest: box + daytime + >200 m AGL only).")
