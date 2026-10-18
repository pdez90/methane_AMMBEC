# 111_blh_export.R ------------------------------------------------------------
# Mixing-height data for the Shiny explorer's "Mixing height" tab: the airborne
# Doppler-lidar boundary-layer height along each flight track, the ground lidar's
# time series on the flight days, and the per-flight and per-transect summaries the
# paper's sensitivity tests use (scripts 13, 22, 90, 91).
#
#   blh_airborne_track.csv   every airborne retrieval (about every 30 s): flight, time,
#                            lat, lon, in_box, aircraft altitude, BLH (m AGL) with the
#                            retrieval's low/high bounds. The 3 July L2 file was provided
#                            by the lidar team and is not in the public archive: it is
#                            included only when METHANE_BLH_INCLUDE_0703L2=1.
#   blh_ground_days.csv      the ground lidar time series (blh_timeseries.csv of script 08)
#                            restricted to the flight days, with the lidar's name
#   plus copies of airborne_blh_flights.csv, transects_vs_blh.csv, qc_robustness.csv
#   and flight_summary.csv (script 86 copies the last two already).
#
# Run: Rscript scripts/111_blh_export.R      (writes straight into shiny/data/)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths", "airborne_blh")) source(file.path(proj, "R", paste0(f, ".R")))
DEST <- file.path(proj, "shiny", "data"); dir.create(DEST, recursive = TRUE, showWarnings = FALSE)
INCL_L2 <- Sys.getenv("METHANE_BLH_INCLUDE_0703L2", "0") == "1"
b <- URBAN_BOX
rows <- list(); skipped <- character(0)
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  x <- airborne_blh_data(fl)
  if (!is.data.frame(x)) next
  if (fl == "20240703_R0_L2" && !INCL_L2) { skipped <- c(skipped, fl); next }
  rows[[fl]] <- data.frame(flight = fl, time_utc = format(x$time, "%Y-%m-%d %H:%M:%S"), lat = round(x$lat, 4), lon = round(x$lon, 4),
    in_box = .in_box(x$lat, x$lon, b), alt_m = round(x$alt_m), ground_m = round(x$ground_m),
    blh_agl_m = round(x$blh_agl), blh_lo_m = round(x$blh_lo), blh_hi_m = round(x$blh_hi), stringsAsFactors = FALSE)
}
A <- do.call(rbind, rows); write.csv(A, file.path(DEST, "blh_airborne_track.csv"), row.names = FALSE)
if (length(skipped)) message("not exported (not in the public archive): ", paste(skipped, collapse = ", "), "  [METHANE_BLH_INCLUDE_0703L2=1 includes it]")
# ground lidar time series on the flight days
days <- unique(substr(names(rows), 1, 8)); days <- unique(c(days, substr(sub("AMMBEC-ARL-Suite_TwinOtter_", "", basename(list_flights(DATA_DIR))), 1, 8)))
ts <- file.path(OUT_DIR, "blh_timeseries.csv")
if (file.exists(ts)) {
  g <- read.csv(ts, stringsAsFactors = FALSE); g$day <- gsub("-", "", substr(g$time, 1, 10))
  g <- g[g$day %in% days, ]; g$lidar <- if (grepl("PUMAS", LIDAR_DIR, ignore.case = TRUE)) "ground lidar (PUMAS)" else "ground lidar (DSRC, Boulder)"
  write.csv(data.frame(day = g$day, time_utc = substr(g$time, 1, 19), blh_m = round(g$blh_m), lidar = g$lidar), file.path(DEST, "blh_ground_days.csv"), row.names = FALSE)
} else message("blh_timeseries.csv not found (script 08): no ground time series exported")
for (f in c("airborne_blh_flights.csv", "transects_vs_blh.csv", "qc_robustness.csv", "flight_summary.csv")) {
  src <- file.path(OUT_DIR, f); if (!file.exists(src)) src <- file.path(proj, "results", f)
  if (file.exists(src)) file.copy(src, file.path(DEST, f), overwrite = TRUE) else message("missing: ", f)
}
message(sprintf("blh_airborne_track.csv: %d retrievals on %d flights; blh_ground_days.csv: %d days", nrow(A), length(rows), length(days)))
