# 06_mobile_trends.R ---------------------------------------------------------
# Trend of mobile CH4 enhancements 2023-2025 at the recurring monitoring sites
# (Suncor P66, HEP Terminal, Collins Aerospace, ...). For every survey it
# computes per-survey enhancement percentiles, then fits an EXPLORATORY ordinary
# least-squares (not robust-regression) linear trend of the per-site 95th-percentile
# enhancement over time and plots the time series.
#
# NOTE: these are fence-line/facility drives, so the "trend" is a signal in a route-
# specific enhancement percentile, NOT a facility-emissions or regional-flux trend.
# The fit is OLS with model-based p-values; with irregular, autocorrelated surveys a
# non-significant p-value means no DETECTABLE linear trend, not demonstrated stable
# emissions. Background is survey-relative (rolling 5th percentile). Read as
# exploratory only.
#
# Run:  Rscript scripts/06_mobile_trends.R
# Out:  <OUT_DIR>/mobile_trends.csv, <OUT_DIR>/mobile_trend_fits.csv,
#       <OUT_DIR>/figures/mobile_trend_<site>.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_mobile.R"))
source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "enhancements.R"))

surveys <- list_mobile(DATA_DIR)
rows <- list()
for (p in surveys) {
  d <- tryCatch(read_mobile(p), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 50 || is.na(d$survey_date[1])) next
  d <- add_enhancements(d, "CH4_ppmv", window = 120, q = 0.05, n_sigma = 3)
  enh <- d$CH4_ppmv_enh
  rows[[p]] <- data.frame(
    date = d$survey_date[1], site = d$site[1], n = nrow(d),
    ch4_bg_med   = stats::median(d$CH4_ppmv_bg, na.rm = TRUE),
    enh_mean     = mean(enh, na.rm = TRUE),
    enh_p95      = stats::quantile(enh, 0.95, na.rm = TRUE, names = FALSE),
    enh_max      = max(enh, na.rm = TRUE),
    plume_frac   = mean(d$plume, na.rm = TRUE),
    stringsAsFactors = FALSE)
}
trends <- do.call(rbind, rows)
trends <- trends[order(trends$site, trends$date), ]
write.csv(trends, file.path(OUT_DIR, "mobile_trends.csv"), row.names = FALSE)

# Per-site linear trend of the 95th-percentile enhancement vs. time.
fits <- list()
for (s in sort(unique(trends$site))) {
  ts <- trends[trends$site == s, ]
  if (nrow(ts) < 3) next
  yr <- as.numeric(ts$date - min(ts$date)) / 365.25     # years since first survey
  fit <- stats::lm(enh_p95 ~ yr, data = data.frame(enh_p95 = ts$enh_p95, yr = yr))
  co <- summary(fit)$coefficients
  fits[[s]] <- data.frame(site = s, n_surveys = nrow(ts),
    span_days = as.integer(max(ts$date) - min(ts$date)),
    slope_ppmv_per_yr = round(co[2,1], 4),
    p_value = round(co[2,4], 4),
    enh_p95_first = round(ts$enh_p95[1], 3),
    enh_p95_last  = round(ts$enh_p95[nrow(ts)], 3),
    stringsAsFactors = FALSE)

  png(file.path(OUT_DIR, "figures", paste0("mobile_trend_", s, ".png")), 900, 460, res = 110)
  plot(ts$date, ts$enh_p95, pch = 19, col = "grey30",
       xlab = "survey date", ylab = "CH4 enhancement p95 (ppmv)",
       main = paste0(s, "  —  trend ", round(co[2,1],3), " ppmv/yr (p=", round(co[2,4],3), ")"))
  abline(fit$coefficients[1] - fit$coefficients[2]*as.numeric(min(ts$date))/365.25,
         fit$coefficients[2]/365.25, col = "red", lwd = 2)
  dev.off()
}
fit_df <- do.call(rbind, fits)
write.csv(fit_df, file.path(OUT_DIR, "mobile_trend_fits.csv"), row.names = FALSE)

message("Wrote mobile_trends.csv (", if (is.null(trends)) 0 else nrow(trends), " surveys) and ",
        "mobile_trend_fits.csv (", if (is.null(fit_df)) 0 else nrow(fit_df), " sites).")
if (!is.null(fit_df)) print(fit_df)
