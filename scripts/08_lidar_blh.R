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

# CAMPAIGN COVERAGE. velStats files are MONTHLY. The campaign spans 28 June to
# 13 July 2024, so a single monthly file covers only part of it: reading just
# VELSTATS_FILE (June) would draw the mixing-layer figure for June alone while
# 13 of 15 flight days are in July. Read every monthly file whose month appears
# in the flight manifest (stage 01) and concatenate, so the diagnostic figure and
# blh_timeseries.csv cover the flights actually analysed. Falls back to
# VELSTATS_FILE if the manifest is absent (e.g. script run standalone).
# NOTE this affects only the SI diagnostic figure and blh_timeseries.csv. The
# per-flight BLH used by the analysis comes from blh_flight() in R/lidar_blh.R,
# which already auto-selects the correct month for each flight date.
.campaign_velstats <- function() {
  mf <- file.path(OUT_DIR, "manifest_flights.csv")
  if (!file.exists(mf)) return(character(0))
  d <- utils::read.csv(mf, stringsAsFactors = FALSE)
  if (!"date" %in% names(d)) return(character(0))
  dt <- suppressWarnings(as.Date(d$date))
  dt <- dt[!is.na(dt)]
  if (!length(dt)) return(character(0))
  cand <- file.path(LIDAR_DIR, paste0("velStats_", sort(unique(format(dt, "%Y%m"))), ".nc"))
  cand[file.exists(cand)]
}
vs_files <- .campaign_velstats()
if (!length(vs_files)) {
  if (!file.exists(VELSTATS_FILE))
    stop("No campaign velStats found in ", LIDAR_DIR, " and VELSTATS_FILE missing: ",
         VELSTATS_FILE, " (set METHANE_VELSTATS)")
  vs_files <- VELSTATS_FILE
}
message("velStats files for the campaign: ", paste(basename(vs_files), collapse = ", "))

vs_list <- lapply(vs_files, read_velstats)

# SITE CHECK. NOAA CSL publishes velStats_YYYYMM.nc under the SAME filename for
# two different Doppler lidars: Dalek 2, the stationary lidar at the DSRC in
# Boulder (39.99 N, -105.26), and PUMAS, the truck-mounted MicroDop that was
# parked in the DJ Basin during AMMBEC (40.18 N, -104.73). A folder holding one
# month from each therefore concatenates two instruments 50 km apart into one
# "campaign" mixing-height series without anything looking wrong. Refuse that.
# Set METHANE_ALLOW_MIXED_LIDAR=1 only if a mixed series is genuinely intended.
.sites <- do.call(rbind, lapply(vs_list, function(v) c(v$lat, v$lon)))
if (nrow(.sites) > 1 && all(is.finite(.sites))) {
  spread_km <- max(.haversine_km <- {
    R <- 6371.0088; d <- pi / 180
    la <- .sites[, 1] * d; lo <- .sites[, 2] * d
    outer(seq_len(nrow(.sites)), seq_len(nrow(.sites)), Vectorize(function(i, j)
      2 * R * asin(pmin(1, sqrt(sin((la[j] - la[i]) / 2)^2 +
        cos(la[i]) * cos(la[j]) * sin((lo[j] - lo[i]) / 2)^2)))))
  })
  if (spread_km > 5) {
    msg <- sprintf(paste0("velStats files are from different sites (%.0f km apart): %s. ",
                          "Dalek 2 (DSRC Boulder) and PUMAS (DJ Basin) share the filename ",
                          "velStats_YYYYMM.nc; download every month from the SAME instrument."),
                   spread_km,
                   paste(sprintf("%s at %.3f,%.3f", basename(vs_files),
                                 .sites[, 1], .sites[, 2]), collapse = "; "))
    if (identical(Sys.getenv("METHANE_ALLOW_MIXED_LIDAR"), "1")) warning(msg) else stop(msg)
  }
}

if (length(vs_list) > 1) {
  h1 <- vs_list[[1]]$height
  same <- vapply(vs_list, function(v) isTRUE(all.equal(v$height, h1)), logical(1))
  if (!all(same)) {
    warning("dropping ", sum(!same), " velStats file(s) whose height grid differs from ",
            basename(vs_files[1]), "; cannot concatenate a ragged grid.")
    vs_list <- vs_list[same]; vs_files <- vs_files[same]
  }
  vs <- list(time   = do.call(c, lapply(vs_list, `[[`, "time")),
             height = vs_list[[1]]$height,
             wVar   = do.call(rbind, lapply(vs_list, `[[`, "wVar")))
  o <- order(vs$time); vs$time <- vs$time[o]; vs$wVar <- vs$wVar[o, , drop = FALSE]
  stopifnot(length(vs$time) == nrow(vs$wVar), ncol(vs$wVar) == length(vs$height))
} else vs <- vs_list[[1]]

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
        format(max(vs$time),"%m-%d %H:%M"), " UTC, from ", length(vs_files),
        " monthly file(s). Filled BLH for ", filled, " flights.")
message("Wrote blh_timeseries.csv and figures lidar_blh.png / lidar_winds.png.")
