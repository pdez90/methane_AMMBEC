# 03_mobile_hotspots.R -------------------------------------------------------
# Mobile CAT survey analysis (AMMBEC memo section 4 — spatial hotspots):
#   * rolling background + CH4 enhancement along the drive track
#   * flag plume points and export a hotspot table (lat/lon/CH4/enhancement)
#   * track map coloured by CH4 enhancement.
#
# Run:  Rscript scripts/03_mobile_hotspots.R [optional: one *_CAT_Methane.csv]
# Out:  <OUT_DIR>/mobile_survey_summary.csv, <OUT_DIR>/mobile_hotspots.csv,
#       <OUT_DIR>/figures/<survey>_track.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_mobile.R"))
source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "enhancements.R"))

args <- commandArgs(trailingOnly = TRUE); args <- args[file.exists(args)]  # ignore stray args
surveys <- if (length(args)) args else list_mobile(DATA_DIR)
if (!length(surveys)) stop("No mobile surveys found under ", DATA_DIR)

.heat <- function(v) {
  rng <- range(v, na.rm = TRUE)
  s <- (v - rng[1]) / (diff(rng) + 1e-9)
  cols <- rep("#BBBBBB", length(v))
  ok <- is.finite(s)
  cols[ok] <- grDevices::rgb(colorRamp(c("#2166AC", "#F7F7F7", "#B2182B"))(s[ok]) / 255)
  cols
}

summ <- list(); hot <- list()
for (p in surveys) {
  d <- tryCatch(read_mobile(p), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 50) { message("skip (empty): ", basename(p)); next }
  # Mobile CH4 in ppmv; ~1 Hz. 120-sample (~2 min) window suits a drive.
  d <- add_enhancements(d, "CH4_ppmv", window = 120, q = 0.05, n_sigma = 3)
  tag <- tools::file_path_sans_ext(basename(p))

  summ[[p]] <- data.frame(
    file = basename(p), site = d$site[1], date = as.character(d$survey_date[1]),
    n = nrow(d), n_plume = sum(d$plume, na.rm = TRUE),
    ch4_bg_med = round(stats::median(d$CH4_ppmv_bg, na.rm = TRUE), 3),
    # p95 is the stable per-survey plume metric (max is a 1 s spike, sensitive to
    # survey duration); both are reported so the figure can use the robust one.
    ch4_enh_p95 = round(stats::quantile(d$CH4_ppmv_enh, 0.95, na.rm = TRUE, names = FALSE), 3),
    ch4_enh_max = round(max(d$CH4_ppmv_enh, na.rm = TRUE), 3),
    stringsAsFactors = FALSE)

  hp <- d[d$plume, c("site", "timestamp", "Latitude", "Longitude",
                     "CH4_ppmv", "CH4_ppmv_enh")]
  if (nrow(hp)) hot[[p]] <- hp

  png(file.path(OUT_DIR, "figures", paste0(tag, "_track.png")), 680, 640, res = 110)
  plot(d$Longitude, d$Latitude, col = .heat(d$CH4_ppmv_enh), pch = 20, cex = 0.6,
       xlab = "Longitude", ylab = "Latitude", asp = 1,
       main = paste0(d$site[1], " ", d$survey_date[1], "  (colour = ΔCH4)"))
  pts <- d[d$plume, ]
  if (nrow(pts)) points(pts$Longitude, pts$Latitude, col = "black", cex = 1.1, lwd = 0.6)
  dev.off()
  message("done: ", tag, "  plumes=", sum(d$plume, na.rm = TRUE),
          "  maxΔ=", round(max(d$CH4_ppmv_enh, na.rm = TRUE), 2), " ppmv")
}

write.csv(do.call(rbind, summ), file.path(OUT_DIR, "mobile_survey_summary.csv"),
          row.names = FALSE)
if (length(hot))
  write.csv(do.call(rbind, hot), file.path(OUT_DIR, "mobile_hotspots.csv"),
            row.names = FALSE)
message("\nWrote mobile_survey_summary.csv (", length(summ), " surveys) and hotspots.")
