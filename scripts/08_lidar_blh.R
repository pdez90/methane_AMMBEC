# 08_lidar_blh.R -------------------------------------------------------------
# Boundary-layer height from the Doppler-lidar velStats file, plus wind-profiler
# winds. Produces the clearest views of the mixing layer:
#   * wVar (turbulence) heatmap in time x height, with derived mixing height
#   * mean diurnal cycle of mixing height
#   * wind-profiler speed heatmap
# and fills blh_m in curtain_config.csv for any flights the lidar covers.
#
# Run:  Rscript scripts/08_lidar_blh.R
# Out:  <OUT_DIR>/blh_timeseries.csv, <OUT_DIR>/figures/lidar_blh.png,
#       lidar_winds.png; updates blh_m in curtain_config.csv where covered.
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R"))
source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "lidar_blh.R"))
source(file.path(proj, "R", "read_windprof.R"))

if (!file.exists(VELSTATS_FILE)) stop("velStats file not found: ", VELSTATS_FILE,
                                      " (set METHANE_VELSTATS)")
vs <- read_velstats(VELSTATS_FILE)
bt <- blh_timeseries(vs)
write.csv(bt, file.path(OUT_DIR, "blh_timeseries.csv"), row.names = FALSE)

# image() needs strictly increasing axes; sort time+height and drop dup times.
.heatmap_ready <- function(time, height, mat, clip = Inf) {
  ot <- order(time); oh <- order(height)
  th <- time[ot]; hh <- height[oh]; z <- mat[ot, oh, drop = FALSE]
  keep <- c(TRUE, diff(as.numeric(th)) > 0)
  hk <- c(TRUE, diff(hh) > 0)
  list(x = as.numeric(th[keep]), y = hh[hk], z = pmin(z[keep, hk, drop = FALSE], clip),
       t = th[keep])
}

# ---- Figure 1: wVar heatmap + mixing height ----
png(file.path(OUT_DIR, "figures", "lidar_blh.png"), 1300, 900, res = 120)
op <- par(mfrow = c(2,1), mar = c(4,4,3,5))
H <- .heatmap_ready(vs$time, vs$height/1000, vs$wVar, clip = 3)
pal <- colorRampPalette(c("#08306B","#4292C6","#FDBE85","#D94701"))(64)
image(x = H$x, y = H$y, z = H$z, col = pal,
      xlab = "", ylab = "height (km AGL)", axes = FALSE,
      main = "Doppler-lidar vertical-velocity variance wVar (turbulence) + mixing height")
axis.POSIXct(1, H$t, format = "%m-%d"); axis(2); box()
lines(as.numeric(bt$time), bt$blh_m/1000, col = "black", lwd = 1.4)
legend("topleft", c("mixing height"), lwd = 1.4, col = "black", bty = "n")

# ---- Figure 1b: mean diurnal cycle of mixing height ----
hr <- as.integer(format(bt$time, "%H"))
di <- tapply(bt$blh_m, hr, function(x) stats::median(x, na.rm = TRUE))
plot(as.integer(names(di)), as.numeric(di)/1000, type = "b", pch = 19, col = "firebrick",
     xlab = "hour of day (UTC;  MDT = UTC-6)", ylab = "median mixing height (km)",
     main = "Mean diurnal cycle of mixing height")
grid(); par(op); dev.off()

# ---- Figure 2: wind-profiler speed heatmap ----
if (file.exists(WINDPROF_FILE)) {
  wp <- read_windprof(WINDPROF_FILE)
  fin <- is.finite(wp$height)
  W <- .heatmap_ready(wp$time, wp$height[fin]/1000, wp$speed[, fin, drop=FALSE], clip = 25)
  png(file.path(OUT_DIR, "figures", "lidar_winds.png"), 1300, 560, res = 120)
  pal2 <- colorRampPalette(c("#F7FBFF","#6BAED6","#08306B"))(64)
  image(x = W$x, y = W$y, z = W$z, col = pal2,
        xlab = "", ylab = "height (km AGL)", axes = FALSE,
        main = "Wind-profiler wind speed (m/s)")
  axis.POSIXct(1, W$t, format = "%m-%d"); axis(2); box()
  dev.off()
}

# ---- Fill curtain_config.csv blh_m for covered flights ----
cfg_path <- CURTAIN_CONFIG
filled <- 0
if (file.exists(cfg_path)) {
  cfg <- utils::read.csv(cfg_path, colClasses = "character", check.names = FALSE)
  for (i in seq_len(nrow(cfg))) {
    fpath <- list.files(file.path(DATA_DIR, "Aircraft"), pattern = cfg$flight[i],
                        recursive = TRUE, full.names = TRUE)[1]
    if (is.na(fpath)) next
    tr <- range(read_icartt(fpath)$data$timestamp, na.rm = TRUE)
    w <- blh_for_window(bt, tr[1], tr[2])
    if (w$n > 0) { cfg$blh_m[i] <- round(w$blh_m); filled <- filled + 1
      message("  filled BLH ", round(w$blh_m), " m for ", cfg$flight[i],
              " (", w$n, " lidar profiles in window)") }
  }
  utils::write.csv(cfg, cfg_path, row.names = FALSE)
}

message("\nBLH: median ", round(stats::median(bt$blh_m, na.rm=TRUE)), " m; ",
        "midday peak ~", round(max(di, na.rm=TRUE)), " m.")
message("Coverage: ", format(min(vs$time),"%m-%d %H:%M"), " to ",
        format(max(vs$time),"%m-%d %H:%M"), " UTC. Filled BLH for ", filled, " flights.")
message("Wrote blh_timeseries.csv and figures lidar_blh.png / lidar_winds.png.")
