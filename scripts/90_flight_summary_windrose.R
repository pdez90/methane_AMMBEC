# 90_flight_summary_windrose.R ------------------------------------------------------
# Per-flight summary of the urban sampling (SI Table S1) and the wind roses added to
# Figure S13. Navigation and wind variables only, plus the counts already written by
# the analysis scripts.
#
# Table S1 (one row per flight with at least three urban transect segments):
#   UTC window of the flight file, UTC window of the in-box transect samples, number
#   of constant-altitude transects crossing the box and of urban transect segments,
#   seconds of transect flight inside the box, the range of the segments' mean height
#   above ground, the mean in-box boundary-layer wind (samples below BL_AGL_MAX_M:
#   scalar-mean speed, vector-mean direction, and the same for samples above), the
#   number of altitude levels and the lidar mixing height (closeloop_diagnostic.csv),
#   whether the flight encircles the box, and the gated sample count of the ethane fit
#   (regression_comparison.csv).
# Wind roses: (a) the aircraft's own winds at every in-box sample below BL_AGL_MAX_M
#   during the campaign flights; (b) the Boulder Doppler lidar winds (windProf files),
#   averaged over 200 to 1000 m above the lidar, for every profile in June and July
#   2024 (the lidar files cover only the campaign months; a full-year 2024 rose needs a
#   surface station record such as the Denver International Airport ASOS, which is not
#   among the repository inputs).
#
# Needs: transects (R/transects.R), closeloop_diagnostic.csv (script 13),
#        regression_comparison.csv (script 22), table1.csv (script 21),
#        figures/SI_wind_fossil.png (script 19), LIDAR_DIR windProf_2024{06,07}.nc
# Out:   <OUT_DIR>/flight_summary.csv, <OUT_DIR>/campaign_sampling_totals.csv,
#        <OUT_DIR>/figures/FigS13_wind_fossil_rose.png
# Run:   Rscript scripts/90_flight_summary_windrose.R
# -----------------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths", "massbalance")) source(file.path(proj, "R", paste0(f, ".R")))
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })

circmean <- function(d, w = rep(1, length(d))) { ok <- is.finite(d) & is.finite(w)
  (atan2(sum(w[ok] * sin(d[ok] * pi / 180)), sum(w[ok] * cos(d[ok] * pi / 180))) * 180 / pi) %% 360 }
vecmean <- function(ws, wd) { ok <- is.finite(ws) & is.finite(wd)
  u <- -ws[ok] * sin(wd[ok] * pi / 180); v <- -ws[ok] * cos(wd[ok] * pi / 180)
  c(dir = (atan2(-mean(u), -mean(v)) * 180 / pi) %% 360, spd = mean(ws[ok]), n = sum(ok)) }
inbox <- function(lat, lon) is.finite(lat) & lat >= URBAN_BOX$lat_s & lat <= URBAN_BOX$lat_n &
  lon >= URBAN_BOX$lon_w & lon <= URBAN_BOX$lon_e
hm <- function(t) format(t, "%H:%M")

rows <- list(); rose <- list(); tot <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  d <- read_icartt(p)$data
  d <- detect_level_legs(d)
  box <- inbox(d$Latitude, d$Longitude)
  bl <- box & is.finite(d$ALTAGL) & d$ALTAGL < BL_AGL_MAX_M
  if (any(bl & is.finite(d$WS) & is.finite(d$WD)))
    rose[[fl]] <- data.frame(flight = fl, ws = d$WS[bl], wd = d$WD[bl])
  tr_box <- box & d$transect_id > 0
  tot[[fl]] <- data.frame(flight = fl, n_transects = length(unique(d$transect_id[d$transect_id > 0])),
                          n_box_transects = length(unique(d$transect_id[tr_box])), inbox_transect_s = sum(tr_box))
  legs <- if (any(d$leg_id > 0)) tag_region(leg_metrics(d)) else NULL
  urb <- if (!is.null(legs)) legs[legs$region == "urban", ] else NULL
  if (is.null(urb) || nrow(urb) < 3) next
  uw <- d$leg_id %in% urb$leg_id
  ab <- box & is.finite(d$ALTAGL) & d$ALTAGL >= BL_AGL_MAX_M
  w_bl <- vecmean(d$WS[bl], d$WD[bl]); w_ab <- vecmean(d$WS[ab], d$WD[ab])
  rows[[fl]] <- data.frame(
    flight = fl,
    flight_utc = paste(hm(min(d$timestamp, na.rm = TRUE)), hm(max(d$timestamp, na.rm = TRUE)), sep = "-"),
    inbox_utc = paste(hm(min(d$timestamp[uw])), hm(max(d$timestamp[uw])), sep = "-"),
    n_box_transects = length(unique(d$transect_id[tr_box])), n_urban_segments = nrow(urb),
    inbox_transect_s = sum(tr_box), urban_segment_s = sum(uw),
    agl_min_m = min(urb$agl_m, na.rm = TRUE), agl_max_m = max(urb$agl_m, na.rm = TRUE),
    bl_ws_ms = round(w_bl[["spd"]], 1), bl_wd_deg = round(w_bl[["dir"]]), n_bl = w_bl[["n"]],
    above_ws_ms = if (w_ab[["n"]] >= 60) round(w_ab[["spd"]], 1) else NA,
    above_wd_deg = if (w_ab[["n"]] >= 60) round(w_ab[["dir"]]) else NA,
    below200_share = round(mean(d$ALTAGL[box] < 200, na.rm = TRUE), 3),
    local_mdt = paste(hm(min(d$timestamp[box], na.rm = TRUE) - 6 * 3600), hm(max(d$timestamp[box], na.rm = TRUE) - 6 * 3600), sep = "-"),
    stringsAsFactors = FALSE)
}
S <- do.call(rbind, rows)
cl <- read.csv(file.path(OUT_DIR, "closeloop_diagnostic.csv"), stringsAsFactors = FALSE)
rc <- read.csv(file.path(OUT_DIR, "regression_comparison.csv"), stringsAsFactors = FALSE)
t1 <- read.csv(file.path(OUT_DIR, "table1.csv"), stringsAsFactors = FALSE)
S$n_alt_levels <- cl$n_alt_levels[match(S$flight, cl$flight)]
S$blh_m <- cl$blh_m[match(S$flight, cl$flight)]
S$blh_src <- cl$blh_src[match(S$flight, cl$flight)]
S$blh_ground_m <- if ("blh_ground_m" %in% names(cl)) cl$blh_ground_m[match(S$flight, cl$flight)] else NA
S$encircles <- ifelse(cl$encloses_metro[match(S$flight, cl$flight)], "yes", "no")
S$n_gated <- rc$n[match(S$flight, rc$flight)]
S$fossil_pct_CI <- t1$Fossil_pct_CI[match(S$flight, t1$Flight)]
S$label <- sub("^2024(\\d\\d)(\\d\\d)_R0_?", "2024-\\1-\\2 ", S$flight)
S$label <- trimws(sub(" (L\\d)$", " (\\1)", S$label))
S$agl_range <- sprintf("%d-%d", S$agl_min_m, S$agl_max_m)
S$bl_wind <- sprintf("%.1f m/s from %d°", S$bl_ws_ms, as.integer(S$bl_wd_deg))
S$above_wind <- ifelse(is.na(S$above_ws_ms), "-", sprintf("%.1f m/s from %d°", S$above_ws_ms, as.integer(S$above_wd_deg)))
write.csv(S, file.path(OUT_DIR, "flight_summary.csv"), row.names = FALSE)
TT <- do.call(rbind, tot)
an <- S$flight[!is.na(S$fossil_pct_CI) & S$fossil_pct_CI != "NA"]
TOT <- data.frame(
  scope = c("all flights", "flights with a fossil fraction"),
  n_flights = c(nrow(TT), length(an)),
  n_transects = c(sum(TT$n_transects), sum(TT$n_transects[TT$flight %in% an])),
  n_box_transects = c(sum(TT$n_box_transects), sum(TT$n_box_transects[TT$flight %in% an])),
  inbox_transect_s = c(sum(TT$inbox_transect_s), sum(TT$inbox_transect_s[TT$flight %in% an])),
  n_gated = c(sum(rc$n, na.rm = TRUE), sum(rc$n[rc$flight %in% an], na.rm = TRUE)))
write.csv(TOT, file.path(OUT_DIR, "campaign_sampling_totals.csv"), row.names = FALSE)
print(S[, c("label", "flight_utc", "inbox_utc", "n_box_transects", "n_urban_segments", "inbox_transect_s",
            "agl_range", "bl_wind", "above_wind", "n_alt_levels", "blh_m", "blh_src", "blh_ground_m", "local_mdt", "below200_share", "n_gated")], row.names = FALSE)
print(TOT, row.names = FALSE)

# ---- wind roses ----------------------------------------------------------------------
SECT <- 16; brk <- c(0, 2, 4, 6, 8, Inf); lab <- c("0-2", "2-4", "4-6", "6-8", ">8")
rose_df <- function(ws, wd) {
  ok <- is.finite(ws) & is.finite(wd) & ws >= 0
  sec <- floor(((wd[ok] + 360 / SECT / 2) %% 360) / (360 / SECT))
  cls <- cut(ws[ok], brk, lab, right = FALSE)
  z <- as.data.frame(table(sec = factor(sec, 0:(SECT - 1)), cls = cls))
  z$pct <- 100 * z$Freq / sum(ok); z$dir <- as.numeric(as.character(z$sec)) * 360 / SECT
  attr(z, "n") <- sum(ok); z
}
rose_plot <- function(z, title) {
  ggplot(z, aes(dir, pct, fill = cls)) + geom_col(width = 360 / SECT * 0.92, colour = "white", linewidth = 0.1) +
    coord_polar(start = -pi / SECT) + scale_x_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(-360 / SECT / 2, 360 - 360 / SECT / 2)) +
    scale_fill_viridis_d(option = "mako", direction = -1, name = "m/s", end = 0.9) +
    annotate("rect", xmin = 22.5, xmax = 90, ymin = 0, ymax = Inf, alpha = 0, colour = "#c0392b", linetype = "22", linewidth = 0.3) +
    labs(x = NULL, y = "% of samples", title = title, subtitle = sprintf("n = %s", format(attr(z, "n"), big.mark = ","))) +
    theme_minimal(base_size = 9) + theme(plot.title = element_text(face = "bold", size = 9))
}
R1 <- do.call(rbind, rose)
za <- rose_df(R1$ws, R1$wd)
lid <- Sys.getenv("METHANE_LIDAR_DIR", unset = if (exists("LIDAR_DIR")) LIDAR_DIR else "")
pb <- NULL
fs <- file.path(lid, c("windProf_202406.nc", "windProf_202407.nc")); fs <- fs[file.exists(fs)]
if (length(fs)) {
  source(file.path(proj, "R", "read_windprof.R"))
  L <- lapply(fs, read_windprof); ws <- c(); wd <- c(); tt <- c()
  for (w in L) {
    k <- which(is.finite(w$height) & w$height >= 200 & w$height <= 1000)
    u <- -w$speed[, k, drop = FALSE] * sin(w$direction[, k, drop = FALSE] * pi / 180)
    v <- -w$speed[, k, drop = FALSE] * cos(w$direction[, k, drop = FALSE] * pi / 180)
    um <- rowMeans(u, na.rm = TRUE); vm <- rowMeans(v, na.rm = TRUE)
    ws <- c(ws, sqrt(um^2 + vm^2)); wd <- c(wd, (atan2(-um, -vm) * 180 / pi) %% 360); tt <- c(tt, as.numeric(w$time))
  }
  keep <- is.finite(ws) & tt >= as.numeric(as.POSIXct("2024-06-01", tz = "UTC")) & tt < as.numeric(as.POSIXct("2024-08-01", tz = "UTC"))
  zb <- rose_df(ws[keep], wd[keep])
  rng <- format(as.POSIXct(range(tt[keep]), origin = "1970-01-01", tz = "UTC"), "%d %b")
  pb <- rose_plot(zb, sprintf("(c) Boulder Doppler lidar, 200-1000 m AGL,\nall profiles %s to %s 2024", rng[1], rng[2]))
  cat(sprintf("lidar rose: %d profiles; NE (22.5-90 deg) share %.0f%%\n", sum(keep),
              100 * mean(wd[keep] >= 22.5 & wd[keep] < 90)))
}
cat(sprintf("aircraft rose: %d in-box BL samples on %d flights; NE share %.0f%%\n", nrow(R1), length(unique(R1$flight)),
            100 * mean(R1$wd >= 22.5 & R1$wd < 90, na.rm = TRUE)))
pa <- rose_plot(za, "(b) Aircraft winds, in-box samples below 1.5 km AGL,\nall campaign flights")
old <- png::readPNG(file.path(OUT_DIR, "figures", "SI_wind_fossil.png"))
nw <- which(apply(old[, , 1:3, drop = FALSE], c(1, 2), min) < 0.97, arr.ind = TRUE)   # crop the white margin
old <- old[max(1, min(nw[, 1]) - 10):min(dim(old)[1], max(nw[, 1]) + 10), max(1, min(nw[, 2]) - 10):min(dim(old)[2], max(nw[, 2]) + 10), , drop = FALSE]
pf <- ggplot() + annotation_raster(old, 0, 1, 0, 1) + coord_fixed(dim(old)[1] / dim(old)[2], xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  labs(title = "(a)") + theme_void(base_size = 9) + theme(plot.title = element_text(face = "bold", size = 9))
g <- if (is.null(pb)) pf | pa else pf | (pa / pb)
g <- g + plot_layout(widths = c(1.35, 1), guides = "collect")
ggsave(file.path(OUT_DIR, "figures", "FigS13_wind_fossil_rose.png"), g, width = 10, height = 6.2, dpi = 200, bg = "white")
message("wrote flight_summary.csv, campaign_sampling_totals.csv, figures/FigS13_wind_fossil_rose.png")
