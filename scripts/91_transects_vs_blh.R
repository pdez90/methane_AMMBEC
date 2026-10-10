# 91_transects_vs_blh.R ---------------------------------------------------------------
# Where the urban transects sit relative to the boundary layer measured along the track
# by the airborne Doppler lidar (R/airborne_blh.R), and how the airborne mixing height
# compares with the ground lidar at the DSRC in Boulder.
#
# For every urban transect segment: its mean height above ground, the median airborne
# BLH over its samples (interpolated along the track; NA where the lidar has no
# retrieval within 3 min), and whether it was flown inside the boundary layer. Also the
# share of the ethane-fit samples (dCH4 > 20 ppb) inside the boundary layer.
#
# Out: <OUT_DIR>/transects_vs_blh.csv  one row per urban transect segment
#      <OUT_DIR>/airborne_blh_flights.csv  one row per flight: airborne (whole flight,
#        in box) and ground-lidar mixing heights
# Run: Rscript scripts/91_transects_vs_blh.R
# --------------------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths", "massbalance", "enhancements", "lidar_blh", "airborne_blh")) source(file.path(proj, "R", paste0(f, ".R")))

rows <- list(); fl_rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  ic <- read_icartt(p); d <- detect_level_legs(ic$data)
  tr <- range(d$timestamp, na.rm = TRUE)
  aw <- airborne_blh_window(fl, tr[1], tr[2]); ab <- airborne_blh_window(fl, tr[1], tr[2], box = URBAN_BOX)
  g <- tryCatch(ground_mixing_height(ic$meta$date, tr[1], tr[2]), error = function(e) list(blh_m = NA, src = "none"))
  fl_rows[[fl]] <- data.frame(flight = fl, airborne_file = !is.na(airborne_blh_file(fl)), n_airborne = aw$n,
                              blh_airborne_flight_m = round(aw$blh_m), blh_airborne_inbox_m = if (ab$in_box) round(ab$blh_m) else NA,
                              n_airborne_inbox = if (ab$in_box) ab$n else 0, blh_ground_m = round(g$blh_m), ground_src = g$src)
  if (!any(d$leg_id > 0)) next
  lg <- tag_region(leg_metrics(d)); urb <- lg[lg$region == "urban", ]
  if (!nrow(urb)) next
  du <- d[d$leg_id %in% urb$leg_id, ]
  if (nrow(du) >= 50) { du <- add_enhancements(du, "CH4_ppb"); gated <- is.finite(du$CH4_ppb_enh) & du$CH4_ppb_enh > 20 } else gated <- rep(FALSE, nrow(du))
  bl <- airborne_blh_at(fl, du$timestamp)
  for (g_ in urb$leg_id) {
    k <- du$leg_id == g_
    b <- if (any(is.finite(bl[k]))) stats::median(bl[k], na.rm = TRUE) else NA_real_
    agl <- mean(du$ALTAGL[k], na.rm = TRUE)
    rows[[length(rows) + 1]] <- data.frame(flight = fl, leg_id = g_, n = sum(k), n_gated = sum(gated[k]), agl_m = round(agl),
      blh_airborne_m = round(b), inside_bl = if (is.finite(b)) agl < b else NA,
      gated_inside_bl = sum(gated[k] & is.finite(bl[k]) & du$ALTAGL[k] < bl[k]),
      gated_with_blh = sum(gated[k] & is.finite(bl[k])))
  }
}
TB <- do.call(rbind, rows); FB <- do.call(rbind, fl_rows)
write.csv(TB, file.path(OUT_DIR, "transects_vs_blh.csv"), row.names = FALSE)
write.csv(FB, file.path(OUT_DIR, "airborne_blh_flights.csv"), row.names = FALSE)
print(FB, row.names = FALSE)
t1 <- tryCatch(read.csv(file.path(OUT_DIR, "table1.csv")), error = function(e) NULL)
an <- if (!is.null(t1)) t1$Flight[t1$Fossil_pct_CI != "NA" & !is.na(t1$Fossil_pct_CI)] else unique(TB$flight)
s <- TB[TB$flight %in% an & is.finite(TB$blh_airborne_m), ]
lo <- s$agl_m < 1000; hi <- s$agl_m >= 1500
cat(sprintf("\nAnalysis flights with airborne BLH: %d of %d; urban transects with a BLH: %d\n", length(unique(s$flight)), length(an), nrow(s)))
cat(sprintf("  transects below 1 km AGL inside the BL: %d of %d\n", sum(s$inside_bl[lo]), sum(lo)))
cat(sprintf("  transects above 1.5 km AGL above the BL: %d of %d\n", sum(!s$inside_bl[hi]), sum(hi)))
cat(sprintf("  gated (dCH4 > 20 ppb) samples inside the BL: %d of %d with a BLH (%.0f%%)\n",
            sum(s$gated_inside_bl), sum(s$gated_with_blh), 100 * sum(s$gated_inside_bl) / max(1, sum(s$gated_with_blh))))
