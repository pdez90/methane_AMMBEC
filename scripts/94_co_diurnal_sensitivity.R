# 94_co_diurnal_sensitivity.R --------------------------------------------------------
# Sensitivity of the CH4:CO emission estimates to the time of day of the CO anchor.
#
# The primary anchor (config.R e_co_anchor) is the 24-hour mean GRA2PES v1.1 box CO for
# the flight's day type. GRA2PES CO has a strong diurnal cycle (traffic), so the CO
# emitted while the urban air was sampled is higher than the daily mean. Two alternatives:
#   (a) "flight hours":      the box CO averaged over the in-box sampling window
#                            (Table S1 inbox_utc), weighted by minutes in each UTC hour;
#   (b) "flight + 3 h prior": the same, from 3 h before the window start, as a rough
#                            allowance for emissions accumulated in the mixed layer
#                            before sampling.
# Each flight's emission is rescaled by (window-mean CO) / (24-hour mean CO) of its day type.
#
# In:  <OUT_DIR>/gra2pes_CO_202307_daytype_hourly.json  (hourly box sums, mol km-2 h-1,
#        per day-type member file; extracted from GRA2PESv1.1_total_202307.tar.gz)
#      <OUT_DIR>/flight_summary.csv (script 90), ratio_method_flux.csv (script 15)
# Out: <OUT_DIR>/co_diurnal_sensitivity.csv
# Run: Rscript scripts/94_co_diurnal_sensitivity.R
# --------------------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("install.packages('jsonlite')")
J <- jsonlite::fromJSON(file.path(OUT_DIR, "gra2pes_CO_202307_daytype_hourly.json"))
hourly_t <- function(dt) {                               # 24 UTC hourly box totals, t CO / h
  a <- J[[sprintf("GRA2PESv1.1_total_202307_%s_00to11Z.nc", dt)]]; b <- J[[sprintf("GRA2PESv1.1_total_202307_%s_12to23Z.nc", dt)]]
  if (is.null(a) || is.null(b)) return(NULL)
  stopifnot(a$ncell == 171, b$ncell == 171)
  c(a$hourly_mol_km2_hr, b$hourly_mol_km2_hr) * 16 * 28.01 / 1e6
}
H <- list(weekdy = hourly_t("weekdy"), satdy = hourly_t("satdy"))
cat(sprintf("24-h mean box CO: weekday %.2f t/h (%.1f Gg/yr), Saturday %.2f t/h (%.1f Gg/yr)\n",
            mean(H$weekdy), mean(H$weekdy) * 8.766, mean(H$satdy), mean(H$satdy) * 8.766))

win_mean <- function(h, t0, t1) {                       # minute-weighted mean over [t0, t1) in UTC minutes (may wrap)
  m <- seq(t0, t1 - 1) %% 1440
  mean(h[m %/% 60 + 1])
}
hm <- function(s) { p <- as.integer(strsplit(s, ":")[[1]]); p[1] * 60 + p[2] }
FS <- read.csv(file.path(OUT_DIR, "flight_summary.csv"), stringsAsFactors = FALSE)
RM <- read.csv(file.path(OUT_DIR, "ratio_method_flux.csv"), stringsAsFactors = FALSE)
RM <- RM[RM$usable_co %in% c(TRUE, "TRUE"), ]
rows <- lapply(seq_len(nrow(RM)), function(i) {
  f <- RM$flight[i]; s <- FS[FS$flight == f, ]
  wd <- as.integer(format(as.Date(RM$date[i]), "%u")); dt <- if (wd <= 5) "weekdy" else if (wd == 6) "satdy" else NA
  h <- H[[dt]]; w <- strsplit(s$inbox_utc, "-")[[1]]; t0 <- hm(w[1]); t1 <- hm(w[2]); if (t1 <= t0) t1 <- t1 + 1440
  fa <- win_mean(h, t0, t1) / mean(h); fb <- win_mean(h, t0 - 180, t1) / mean(h)
  data.frame(flight = f, day_type = dt, inbox_utc = s$inbox_utc, E_daytype_t_hr = RM$E_CH4_from_CO_t_hr[i],
             co_24h_t_hr = round(mean(h), 2), co_window_t_hr = round(win_mean(h, t0, t1), 2), co_window3h_t_hr = round(win_mean(h, t0 - 180, t1), 2),
             factor_window = round(fa, 3), factor_window3h = round(fb, 3),
             E_window_t_hr = round(RM$E_CH4_from_CO_t_hr[i] * fa, 2), E_window3h_t_hr = round(RM$E_CH4_from_CO_t_hr[i] * fb, 2))
})
D <- do.call(rbind, rows)
write.csv(D, file.path(OUT_DIR, "co_diurnal_sensitivity.csv"), row.names = FALSE)
print(D, row.names = FALSE)
cat(sprintf("\nprimary (24-h day type): %.1f-%.1f t/h, median %.1f\n", min(D$E_daytype_t_hr), max(D$E_daytype_t_hr), median(D$E_daytype_t_hr)))
cat(sprintf("flight hours:            %.1f-%.1f t/h, median %.1f (factors %.2f-%.2f)\n", min(D$E_window_t_hr), max(D$E_window_t_hr), median(D$E_window_t_hr), min(D$factor_window), max(D$factor_window)))
cat(sprintf("flight + 3 h prior:      %.1f-%.1f t/h, median %.1f (factors %.2f-%.2f)\n", min(D$E_window3h_t_hr), max(D$E_window3h_t_hr), median(D$E_window3h_t_hr), min(D$factor_window3h), max(D$factor_window3h)))
