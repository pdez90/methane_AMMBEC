# 69_ethane_lag_sensitivity.R ---------------------------------------------------------
# Does the CH4/C2H6 timing offset, or the ethane precision given to York, move the
# fossil fraction?
#
# WHY THIS EXISTS. The York fits pair C2H6(t) with CH4(t). CH4 comes from the Picarro
# G2401-m and C2H6 from the Aeris MIRA, on separate inlets and clocks. If one stream
# lags the other by tau seconds, the pointwise slope is attenuated: at ~50 m/s a 2-s
# offset is ~100 m of track. The analysis never applied a lag correction, so this
# script asks what the correction would change. It also replaces the nominal ethane
# precision (0.2 ppb, the ICARTT value used as sy) with the in-flight scatter.
#
#   (1) LAG ESTIMATE. Per flight, from strong-variance 60-s windows on level legs
#       (estimate_lag() in R/plume_tracer.R): once over all level legs, once over the
#       DJB basin legs only (ethane-rich, so the co-enhancement is unmistakable).
#       Never per plume, which would fit noise.
#   (2) LAG SWEEP. C2H6 is shifted by k = -5..+5 s over the WHOLE record before leg
#       selection, background and gating, and the Table 1 fit (urban legs, dCH4 > 20
#       ppb, within-leg York, sx 1, sy 0.2, beta 0.102) is rerun at every k. The basin
#       ratio (pooled York, as script 31) is swept alongside. Bootstrap intervals at
#       k = 0 and at the estimated campaign lag only.
#   (3) ETHANE PRECISION. Empirical 1-Hz sigma = MAD of the 61-s high-passed ethane at
#       quiet level-leg samples (|hp CH4| < 5 ppb). It includes sub-minute atmospheric
#       variability, so it bounds instrument noise from above. Table 1 is refitted with
#       sy = that flight's sigma, and with sy = 0.5 ppb as a stress case.
#
# SELF-CHECK. k = 0 with sy = 0.2 must reproduce table1.csv (script 21) to 1 point.
#
# Needs: base R.  Run: Rscript scripts/69_ethane_lag_sensitivity.R
# Out:   <OUT_DIR>/lag_estimates.csv, lag_sensitivity_flights.csv,
#        lag_sensitivity_summary.csv, ethane_precision_sensitivity.csv,
#        figures/FigS_lag_sensitivity.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "plume_tracer.R"))

LAGS <- -5:5; GATE <- 20; MIN_PTS <- 10; B_BOOT <- 2000
SX <- 1.0; SY <- 0.2; SY_STRESS <- 0.5

fl_name <- function(p) sub("\\.ict$", "", sub("AMMBEC-ARL-Suite_TwinOtter_", "", basename(p)))
fit_region <- function(d, legs, reg, sy, boot = FALSE) {
  ids <- legs$leg_id[legs$region == reg]; dr <- d[d$leg_id %in% ids, ]
  if (nrow(dr) < 50) return(NULL)
  dr <- add_enhancements(dr, "CH4_ppb"); dr <- add_enhancements(dr, "C2H6_ppb")
  x <- dr$CH4_ppb_enh; y <- dr$C2H6_ppb_enh; g <- dr$leg_id
  k <- is.finite(x) & is.finite(y) & x > GATE
  if (sum(k) < MIN_PTS) return(data.frame(n = sum(k), slope = NA, lo = NA, hi = NA))
  within <- reg == "urban" && fossil_method() == "york_within"
  sl <- if (within) fossil_slope_fit(x[k], y[k], g[k], SX, sy)$slope else york_slope(x[k], y[k], SX, sy)$slope
  lo <- hi <- NA
  if (boot) { bo <- york_boot(x[k], y[k], SX, sy, blocks = g[k], B = B_BOOT, within = within); lo <- bo$lo; hi <- bo$hi }
  data.frame(n = sum(k), slope = sl, lo = lo, hi = hi)
}

flights <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb", "C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  flights[[fl_name(p)]] <- list(d = d, legs = tag_region(leg_metrics(d)))
}
if (!length(flights)) stop("no ARL-Suite flights under METHANE_DATA_DIR = ", DATA_DIR)

## ---- (1) lag estimates + (3) empirical ethane noise -------------------------------
est <- do.call(rbind, lapply(names(flights), function(fl) {
  d <- flights[[fl]]$d; legs <- flights[[fl]]$legs
  inleg <- d$leg_id > 0; inbas <- d$leg_id %in% legs$leg_id[legs$region == "basin"]
  a <- estimate_lag(d$CH4_ppb, d$C2H6_ppb, inleg); b <- estimate_lag(d$CH4_ppb, d$C2H6_ppb, inbas)
  hC <- highpass(d$CH4_ppb); hE <- highpass(d$C2H6_ppb)
  quiet <- inleg & is.finite(hC) & abs(hC) < 5
  ns <- noise_stats(hE, quiet)
  data.frame(flight = fl, lag_all = a["lag"], r_all = a["r"], contrast_all = a["contrast"], nwin_all = a["n_win"],
             lag_basin = b["lag"], r_basin = b["r"], contrast_basin = b["contrast"], nwin_basin = b["n_win"],
             sigma_C2H6_empirical = ns["sigma"], rho1_C2H6 = ns["rho1"], n_quiet = sum(quiet))
}))
rownames(est) <- NULL
wd <- function(l, r, c) is.finite(l) & is.finite(r) & r >= 0.3 & is.finite(c) & c >= 0.05
est$well_defined_all <- wd(est$lag_all, est$r_all, est$contrast_all)
est$well_defined_basin <- wd(est$lag_basin, est$r_basin, est$contrast_basin)
LAG_CAMP <- as.integer(round(stats::median(est$lag_all[est$well_defined_all])))   # round, as script 66 does
LAG_BASIN <- stats::median(est$lag_basin[est$well_defined_basin])
write.csv(est, file.path(OUT_DIR, "lag_estimates.csv"), row.names = FALSE)

## ---- (2) lag sweep --------------------------------------------------------------------
rows <- list()
for (fl in names(flights)) for (k in LAGS) {
  d <- flights[[fl]]$d; legs <- flights[[fl]]$legs
  d$C2H6_ppb <- shift_series(d$C2H6_ppb, k)          # value at t+k aligned to t
  boot <- k %in% c(0, LAG_CAMP)
  for (reg in c("urban", "basin")) {
    f <- fit_region(d, legs, reg, SY, boot = boot && reg == "urban"); if (is.null(f)) next
    rows[[length(rows) + 1]] <- data.frame(flight = fl, region = reg, lag_s = k, n_points = f$n, slope = f$slope,
      fossil_pct = if (reg == "urban") 100 * fossil_fraction(f$slope, SOURCE_C2H6_CH4) else NA,
      lo_pct = if (reg == "urban" && is.finite(f$lo)) 100 * fossil_fraction(f$lo, SOURCE_C2H6_CH4) else NA,
      hi_pct = if (reg == "urban" && is.finite(f$hi)) 100 * fossil_fraction(f$hi, SOURCE_C2H6_CH4) else NA)
  }
}
S <- do.call(rbind, rows)
write.csv(S, file.path(OUT_DIR, "lag_sensitivity_flights.csv"), row.names = FALSE)
U <- S[S$region == "urban", ]; BA <- S[S$region == "basin", ]
summ <- do.call(rbind, lapply(LAGS, function(k) {
  u <- U[U$lag_s == k & is.finite(U$fossil_pct), ]; b <- BA[BA$lag_s == k & is.finite(BA$slope) & BA$slope > 0, ]
  data.frame(lag_s = k, n_flights = nrow(u), median_fossil_pct = round(stats::median(u$fossil_pct), 1),
             min_fossil_pct = round(min(u$fossil_pct), 1), max_fossil_pct = round(max(u$fossil_pct), 1),
             n_majority_fossil = sum(u$fossil_pct > 50), median_urban_slope = round(stats::median(u$slope), 5),
             median_basin_ratio = round(stats::median(b$slope), 5), n_basin = nrow(b))
}))
write.csv(summ, file.path(OUT_DIR, "lag_sensitivity_summary.csv"), row.names = FALSE)

## ---- (3) ethane precision -----------------------------------------------------------
prec <- do.call(rbind, lapply(names(flights), function(fl) {
  d <- flights[[fl]]$d; legs <- flights[[fl]]$legs
  se <- est$sigma_C2H6_empirical[est$flight == fl]
  f0 <- fit_region(d, legs, "urban", SY); if (is.null(f0) || !is.finite(f0$slope)) return(NULL)
  fe <- fit_region(d, legs, "urban", se); fs <- fit_region(d, legs, "urban", SY_STRESS)
  data.frame(flight = fl, sigma_empirical = round(se, 3),
             fossil_pct_sy0.2 = round(100 * fossil_fraction(f0$slope, SOURCE_C2H6_CH4), 1),
             fossil_pct_sy_empirical = round(100 * fossil_fraction(fe$slope, SOURCE_C2H6_CH4), 1),
             fossil_pct_sy0.5 = round(100 * fossil_fraction(fs$slope, SOURCE_C2H6_CH4), 1))
}))
write.csv(prec, file.path(OUT_DIR, "ethane_precision_sensitivity.csv"), row.names = FALSE)

## ---- self-check against table1.csv ---------------------------------------------------
t1p <- file.path(OUT_DIR, "table1.csv"); selfcheck <- "table1.csv not found (run script 21)"
if (file.exists(t1p)) {
  t1 <- read.csv(t1p, stringsAsFactors = FALSE)
  m <- merge(U[U$lag_s == 0, c("flight", "fossil_pct")],
             data.frame(flight = t1$Flight, t1 = suppressWarnings(as.numeric(sub(" .*", "", t1$Fossil_pct_CI)))), by = "flight")
  m$diff <- round(m$fossil_pct) - m$t1
  bad <- m[is.finite(m$diff) & abs(m$diff) > 1, ]
  selfcheck <- if (nrow(bad)) paste("FAILED on", paste(bad$flight, collapse = ", ")) else
    sprintf("OK: lag 0 matches table1.csv on %d flights", sum(is.finite(m$diff)))
  if (nrow(bad)) { message("\n*** SELF-CHECK FAILED ***"); print(bad) }
}

## ---- figure ---------------------------------------------------------------------------
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "FigS_lag_sensitivity.png"), width = 1500, height = 700, res = 165)
par(mfrow = c(1, 2), mar = c(4.4, 5.2, 2.8, 1.0), las = 1, bty = "l")
plot(range(LAGS), c(0, 100), type = "n", xlab = expression("C"[2]*"H"[6]*" shift vs CH"[4]*" (s; <0 = ethane read earlier)"),
     ylab = "Urban fossil fraction (%)", main = "Fossil fraction vs ethane time shift", cex.main = 0.95)
abline(h = 50, col = "grey80", lty = 3)
rect(LAG_CAMP - 0.5, -5, LAG_CAMP + 0.5, 105, col = adjustcolor("#2a78d6", 0.10), border = NA)
for (f in unique(U$flight)) { s <- U[U$flight == f & is.finite(U$fossil_pct), ]
  if (nrow(s) > 1) lines(s$lag_s, s$fossil_pct, col = "#9aa6b2", lwd = 1) }
lines(summ$lag_s, summ$median_fossil_pct, col = "#eb6834", lwd = 3); points(summ$lag_s, summ$median_fossil_pct, col = "#eb6834", pch = 19, cex = 0.8)
abline(v = 0, col = "#3d3d3a", lty = 2)
legend("topright", c("campaign median", "individual flights", sprintf("estimated lag (%+d s)", LAG_CAMP)),
       col = c("#eb6834", "#9aa6b2", adjustcolor("#2a78d6", 0.35)), lwd = c(3, 1, 8), bty = "n", cex = 0.62)
plot(summ$lag_s, summ$median_basin_ratio, type = "b", pch = 19, col = "#1c5cab",
     xlab = expression("C"[2]*"H"[6]*" shift vs CH"[4]*" (s)"), ylab = "",
     main = "Median DJB basin ratio vs shift", cex.main = 0.95, ylim = c(0, max(summ$median_basin_ratio, na.rm = TRUE) * 1.1))
title(ylab = expression(Delta*"C"[2]*"H"[6]*" / "*Delta*"CH"[4]*" (mol mol"^-1*")"), line = 3.9)
abline(v = c(0, LAG_CAMP), col = c("#3d3d3a", "#2a78d6"), lty = c(2, 1))
dev.off()

## ---- report ----------------------------------------------------------------------------
cat("\n=== Ethane timing and precision audit ===\n")
cat(sprintf("Lag (C2H6 vs CH4; <0 = ethane recorded earlier): all level legs median %+d s (%d of %d flights well defined; range %s);\n",
            LAG_CAMP, sum(est$well_defined_all), nrow(est), paste(range(est$lag_all[est$well_defined_all]), collapse = " to ")))
cat(sprintf("  DJB basin legs only: median %+.1f s (%d flights well defined).\n", LAG_BASIN, sum(est$well_defined_basin)))
cat("\nCampaign fossil fraction vs shift (urban, within-leg York, gate 20 ppb, beta 0.102):\n")
print(summ, row.names = FALSE)
b0 <- summ$median_fossil_pct[summ$lag_s == 0]; bL <- summ$median_fossil_pct[summ$lag_s == LAG_CAMP]
cat(sprintf("\nMedian at 0 s: %.0f%%; at the estimated %+d s: %.0f%%; across -2..+2 s: %.0f-%.0f%%; across -5..+5 s: %.0f-%.0f%%\n",
            b0, LAG_CAMP, bL, min(summ$median_fossil_pct[abs(summ$lag_s) <= 2]), max(summ$median_fossil_pct[abs(summ$lag_s) <= 2]),
            min(summ$median_fossil_pct), max(summ$median_fossil_pct)))
cat("\nPer flight at 0 s and at the estimated lag [leg-block 95%]:\n")
pf <- merge(U[U$lag_s == 0, c("flight", "fossil_pct", "lo_pct", "hi_pct")], U[U$lag_s == LAG_CAMP, c("flight", "fossil_pct", "lo_pct", "hi_pct")],
            by = "flight", suffixes = c("_0s", "_lag"))
print(pf, row.names = FALSE, digits = 3)
cat(sprintf("\nEmpirical 1-Hz ethane scatter: median %.3f ppb (range %.3f-%.3f) vs the 0.2 ppb given to York.\n",
            stats::median(est$sigma_C2H6_empirical), min(est$sigma_C2H6_empirical), max(est$sigma_C2H6_empirical)))
print(prec, row.names = FALSE)
cat(sprintf("Campaign median: sy 0.2 -> %.0f%%, empirical sy -> %.0f%%, sy 0.5 -> %.0f%%\n",
            stats::median(prec$fossil_pct_sy0.2), stats::median(prec$fossil_pct_sy_empirical), stats::median(prec$fossil_pct_sy0.5)))
cat("\nSelf-check:", selfcheck, "\n")
