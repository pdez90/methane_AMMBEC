# 108_stilt_particle_gif.R ----------------------------------------------------
# A short animation (GIF, and MP4 when ffmpeg is available) of the HRRR-STILT
# particles of one urban transect played backward in time, from the export of
# script 107: the same picture as the Shiny explorer's STILT tab, for talks and
# as a supplementary movie. Frames every 15 min from release to 10 h back; the
# particles are the 25 per receptor exported by script 107 (orange when in the
# lower half of the mixed layer, i.e. contributing to the surface footprint, grey
# above it); the lines are the centroid trails of all 500 particles per receptor.
#
# Usage: Rscript scripts/108_stilt_particle_gif.R [flight leg_id]
#   default transect: the one with the largest share of its predicted enhancement
#   from basin oil and gas north of 40.05 N (stilt_exposure_legs.csv)
# Out: <OUT_DIR>/figures/stilt_particles_<flight>_leg<id>.gif (+ .mp4 with ffmpeg)
# Needs: ffmpeg on PATH (GIF and MP4) or the magick package (GIF only).
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
args <- commandArgs(trailingOnly = TRUE)
L <- read.csv(file.path(OUT_DIR, "stilt_exposure_legs.csv"), stringsAsFactors = FALSE)
P <- read.csv(file.path(OUT_DIR, "stilt_particles.csv.gz"), stringsAsFactors = FALSE)
C <- read.csv(file.path(OUT_DIR, "stilt_centroids.csv"), stringsAsFactors = FALSE)
R <- read.csv(file.path(OUT_DIR, "stilt_exposure_receptors.csv"), stringsAsFactors = FALSE)
if (length(args) >= 2) { fl <- args[1]; lg <- as.integer(args[2]) } else {
  k <- which.max(L$dC_djb_core / L$total); fl <- L$flight[k]; lg <- L$leg_id[k] }
pp <- P[P$flight == fl & P$leg_id == lg, ]; cc <- C[C$flight == fl & C$leg_id == lg, ]; rr <- R[R$flight == fl & R$leg_id == lg, ]
if (!nrow(pp)) stop("no particles for ", fl, " transect ", lg, " (run script 107)")
lab <- function(f) { f <- sub("_R0", "", f); paste(as.integer(substr(f, 7, 8)), month.abb[as.integer(substr(f, 5, 6))], sub("^[0-9]+_?", "", f)) }
t0 <- as.POSIXct(rr$run_time[1], tz = "UTC")
b <- URBAN_BOX; BASIN_LAT <- 40.05
qx <- quantile(pp$lon, c(0.02, 0.98)); qy <- quantile(pp$lat, c(0.02, 0.98))      # 2nd-98th percentile of the particle positions, so a few far outliers do not shrink the map
xl <- c(min(qx[1] - 0.1, -105.35), max(qx[2] + 0.1, -104.3)); yl <- c(min(qy[1] - 0.08, 39.45), max(qy[2] + 0.08, 40.75))
steps <- sort(unique(pp$min_back))
fd <- file.path(tempdir(), "stilt_frames"); dir.create(fd, showWarnings = FALSE); unlink(list.files(fd, full.names = TRUE))
for (i in seq_along(steps)) {
  mb <- steps[i]; now <- pp[pp$min_back == mb, ]; tr <- cc[cc$min_back <= mb, ]
  png(file.path(fd, sprintf("f%03d.png", i)), width = 1200, height = 900, res = 130)
  par(mar = c(3.5, 3.5, 3, 1))
  plot(NA, xlim = xl, ylim = yl, asp = 1 / cos(mean(yl) * pi / 180), xlab = "", ylab = "", las = 1)
  grid(col = "grey90"); rect(b$lon_w, b$lat_s, b$lon_e, b$lat_n, border = "black", lwd = 1.5); abline(h = BASIN_LAT, lty = 2, col = "grey40")
  for (r in unique(tr$rec)) { s <- tr[tr$rec == r, ]; s <- s[order(s$min_back), ]; lines(s$lon, s$lat, col = "grey35", lwd = 1.3) }
  points(now$lon[!now$in_foot], now$lat[!now$in_foot], pch = 16, col = adjustcolor("#8a8985", 0.7), cex = 0.8)
  points(now$lon[now$in_foot], now$lat[now$in_foot], pch = 16, col = adjustcolor("#d95926", 0.85), cex = 0.9)
  points(rr$long, rr$lati, pch = 3, lwd = 2, cex = 1.4)
  title(sprintf("%s, transect %d: %.2g h before sampling (%s UTC)", lab(fl), lg, mb / 60, format(t0 - mb * 60, "%d %b %H:%M")), cex.main = 1)
  legend("bottomleft", c("particle in the lower half of the mixed layer (adds to the footprint)", "particle above it", "centroid trail (all 500 particles)", "receptor (aircraft sample)", "Denver-metro box; dashed: 40.05 N (basin threshold)"),
         pch = c(16, 16, NA, 3, NA), lty = c(NA, NA, 1, NA, 1), col = c("#d95926", "#8a8985", "grey35", "black", "black"), bty = "n", cex = 0.75)
  dev.off()
}
dir.create(file.path(OUT_DIR, "figures"), showWarnings = FALSE)
stem <- file.path(OUT_DIR, "figures", sprintf("stilt_particles_%s_leg%d", fl, lg))
if (nzchar(Sys.which("ffmpeg"))) {
  system2("ffmpeg", c("-y", "-loglevel", "error", "-framerate", "4", "-i", shQuote(file.path(fd, "f%03d.png")), "-vf", shQuote("scale=900:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse"), shQuote(paste0(stem, ".gif"))))
  system2("ffmpeg", c("-y", "-loglevel", "error", "-framerate", "4", "-i", shQuote(file.path(fd, "f%03d.png")), "-pix_fmt", "yuv420p", "-vf", "scale=1200:-2", shQuote(paste0(stem, ".mp4"))))
  message("wrote ", stem, ".gif and .mp4 (", length(steps), " frames, 4 per second)")
} else if (requireNamespace("magick", quietly = TRUE)) {
  im <- magick::image_read(list.files(fd, full.names = TRUE)); im <- magick::image_scale(im, "900")
  magick::image_write(magick::image_animate(im, fps = 4), paste0(stem, ".gif"))
  message("wrote ", stem, ".gif (", length(steps), " frames; install ffmpeg for an MP4)")
} else stop("neither ffmpeg nor the magick package is available; frames are in ", fd)
