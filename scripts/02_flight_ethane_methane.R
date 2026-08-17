# 02_flight_ethane_methane.R -------------------------------------------------
# Per-flight first-order analysis (AMMBEC memo sections 1-2):
#   * background + CH4 enhancement (plume detection)
#   * ethane:methane RMA slope  -> thermogenic vs biogenic signature
#   * crude fossil fraction from the ethane ratio
#   * three quicklook figures per flight: CH4 time series, C2H6-vs-CH4 scatter,
#     flight-track map coloured by CH4 enhancement.
#
# Run:  Rscript scripts/02_flight_ethane_methane.R [optional: one .ict path]
# Out:  <OUT_DIR>/flight_ethane_methane.csv  and  <OUT_DIR>/figures/*.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R"))
source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R"))

args <- commandArgs(trailingOnly = TRUE); args <- args[file.exists(args)]  # ignore stray args
flights <- if (length(args)) args else list_flights(DATA_DIR)
if (!length(flights)) stop("No flights found under ", file.path(DATA_DIR, "Aircraft"))

# Simple blue->red palette for map colouring (no package needed). NA-safe.
.heat <- function(v) {
  rng <- range(v, na.rm = TRUE)
  s <- (v - rng[1]) / (diff(rng) + 1e-9)
  cols <- rep("#BBBBBB", length(v))          # grey for missing
  ok <- is.finite(s)
  cols[ok] <- grDevices::rgb(colorRamp(c("#2166AC", "#F7F7F7", "#B2182B"))(s[ok]) / 255)
  cols
}

summ <- list()
for (p in flights) {
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb", "C2H6_ppb") %in% names(ic$data))) {
    message("skip (no CH4/C2H6): ", basename(p)); next
  }
  d <- ic$data
  d <- add_enhancements(d, "CH4_ppb", window = 300, q = 0.05, n_sigma = 3)
  d <- add_enhancements(d, "C2H6_ppb", window = 300, q = 0.05, n_sigma = 3)
  fit <- ethane_methane_ratio(d, "CH4_ppb", "C2H6_ppb", min_enh = 20)
  ff  <- fossil_fraction(fit$slope, SOURCE_C2H6_CH4)
  tag <- tools::file_path_sans_ext(basename(p))

  summ[[p]] <- data.frame(
    file = basename(p), date = as.character(ic$meta$date), n = nrow(d),
    n_plume = sum(d$plume, na.rm = TRUE),
    ch4_bg_med = round(stats::median(d$CH4_ppb_bg, na.rm = TRUE), 1),
    ch4_enh_max = round(max(d$CH4_ppb_enh, na.rm = TRUE), 1),
    # WHOLE-FLIGHT quicklook columns: these mix urban, DJB-basin, and biogenic air
    # and are NOT the Denver urban endmember or urban fossil fraction. Named to
    # prevent misuse; the manuscript's urban values come from the urban-leg
    # workflow (scripts 15/21), never from this file.
    wholeflight_c2h6_ch4_slope = round(fit$slope, 4),
    ratio_pct = round(fit$ratio_pct, 2), r = round(fit$r, 3), n_fit = fit$n,
    wholeflight_fossil_frac_diag = round(ff, 2), stringsAsFactors = FALSE)

  # --- figures ---
  fig <- function(name) file.path(OUT_DIR, "figures", paste0(tag, "_", name, ".png"))

  png(fig("ch4_timeseries"), 1100, 420, res = 110)
  plot(d$timestamp, d$CH4_ppb, type = "l", col = "grey40",
       xlab = "UTC", ylab = "CH4 (ppb)", main = paste("CH4 —", tag))
  lines(d$timestamp, d$CH4_ppb_bg, col = "steelblue")
  points(d$timestamp[d$plume], d$CH4_ppb[d$plume], col = "red", pch = 20, cex = 0.5)
  legend("topright", c("CH4", "background", "plume"), bty = "n",
         col = c("grey40", "steelblue", "red"), lty = c(1, 1, NA), pch = c(NA, NA, 20))
  dev.off()

  png(fig("c2h6_vs_ch4"), 620, 600, res = 110)
  keep <- is.finite(d$CH4_ppb_enh) & is.finite(d$C2H6_ppb_enh) & d$CH4_ppb_enh > 20
  plot(d$CH4_ppb_enh[keep], d$C2H6_ppb_enh[keep], pch = 20, cex = 0.5,
       col = "#00000055", xlab = "ΔCH4 (ppb)", ylab = "ΔC2H6 (ppb)",
       main = sprintf("%s\nslope=%.3f  r=%.2f  fossil~%.0f%%",
                      tag, fit$slope, fit$r, 100 * ff))
  if (is.finite(fit$slope)) abline(fit$intercept, fit$slope, col = "red", lwd = 2)
  dev.off()

  png(fig("track_map"), 640, 640, res = 110)
  ok <- is.finite(d$Longitude) & is.finite(d$Latitude)
  plot(d$Longitude[ok], d$Latitude[ok], col = .heat(d$CH4_ppb_enh[ok]),
       pch = 20, cex = 0.5, xlab = "Longitude", ylab = "Latitude",
       main = paste("Flight track (colour = ΔCH4) —", tag), asp = 1)
  dev.off()

  message("done: ", tag, "  slope=", round(fit$slope, 3), " fossil~", round(100 * ff), "%")
}

out <- do.call(rbind, summ)
write.csv(out, file.path(OUT_DIR, "flight_ethane_methane.csv"), row.names = FALSE)
message("\nWrote ", file.path(OUT_DIR, "flight_ethane_methane.csv"),
        " (", nrow(out), " flights) and figures in ", file.path(OUT_DIR, "figures"))
