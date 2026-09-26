# 68_plume_detection_power.R -----------------------------------------------------------
# EXPLORATORY (plume-level tracer analysis, step 3 of 3; needs 66 and 67).
# Nothing here changes a published number.
#
# How often does the script-67 classifier call a plume "fossil detected" (ethane
# slope CI lower bound > 0) when the plume truly carries ethane at ratio r?
#     P(detect | peak dCH4 = A, plume width, r)
# and the matching power for a CO/CH4 slope, so that "no CO signal" can be read as
# "absent" only where the test could have seen it.
#
# The classifier is slope_ci() (bootstrap U AR(1) interval), exactly as in 67.
# A. SYNTHETIC plumes. Gaussian in time, peak A, FWHM F (s; x ~50 m/s for km).
#    CH4 = signal held at the Picarro's 2.5-s update + AR(1) noise; C2H6 = r x signal
#    (Aeris, 1 Hz) + AR(1) noise; CO = r_CO x signal, held + AR(1) noise. Noise sigma
#    and lag-1 correlation = campaign medians from 66. Window and admission exactly as
#    67 (core where dCH4 > 20 ppb, +/-10 s; core >= 10 s; p95-p5 >= 25 ppb; >= 20 samples).
#    r = 0 gives the false-positive rate. A 1-s residual ethane lag is run as a
#    sensitivity (lag_err = 1).
# B. EMPIRICAL plumes. Each admitted plume's own dCH4 series (shape, width, sampling)
#    is the truth proxy; synthetic ethane/CO = r x dCH4 + AR(1) noise at that flight's
#    measured sigma, rho. The proxy carries CH4 noise (~1 ppb), negligible next to
#    >= 25 ppb plume ranges.
#
#   eval "$(bash run_local.sh --print-env)"; Rscript scripts/68_plume_detection_power.R
#   Out: <OUT_DIR>/plume_tracer/power_synthetic.csv, power_thresholds.csv,
#        power_empirical.csv, figures/plume_detection_power.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R")); source(file.path(proj, "R", "plume_tracer.R"))
PT <- file.path(OUT_DIR, "plume_tracer"); FIG <- file.path(PT, "figures")
nzf <- file.path(PT, "plume_noise_by_flight.csv"); rdsf <- file.path(PT, "plume_windows.rds")
if (!file.exists(nzf) || !file.exists(rdsf)) stop("run scripts/66 and 67 first")
NZ <- read.csv(nzf); PW <- readRDS(rdsf)

NSIM <- as.integer(Sys.getenv("METHANE_POWER_NSIM", 200)); BBOOT <- 300; BLOCK <- 5
ENH_MIN <- 20; CORE_MIN <- 10; PAD_S <- 10; RANGE_MIN <- 25; N_MIN <- 20; HOLD_S <- 2.5
BETA <- SOURCE_C2H6_CH4
med <- function(v) stats::median(v, na.rm = TRUE)
sig <- list(CH4 = med(NZ$sigma_CH4_ppb), C2H6 = med(NZ$sigma_C2H6_ppb), CO = med(NZ$sigma_CO_ppb))
rho <- list(CH4 = med(NZ$rho1_CH4_ppb), C2H6 = med(NZ$rho1_C2H6_ppb), CO = med(NZ$rho1_CO_ppb))
R_ETH <- c(r0 = 0, f10 = 0.1 * BETA, f25 = 0.25 * BETA, f50 = 0.5 * BETA, DJB_low = 0.061, adopted = BETA)
R_CO  <- c(0, 0.02, 0.05, 0.1, 0.2, 0.5)
AMP   <- c(25, 30, 40, 50, 75, 100, 150, 200, 300)
FWHM  <- c(5, 10, 20, 40)
set.seed(68)

hold <- function(s, dt = HOLD_S) s[pmin(length(s), floor((seq_along(s) - 1) / dt) * dt + 1)]
power_cell <- function(A, F, r, sy, ry, lag_err = 0, hold_y = FALSE) {
  sd_t <- F / 2.355; half <- ceiling(sd_t * sqrt(2 * log(A / ENH_MIN))) + PAD_S + 5
  t <- -half:half; s <- A * exp(-t^2 / (2 * sd_t^2))
  core <- which(s > ENH_MIN)
  if (length(core) < CORE_MIN) return(c(p_admit = 0, p_detect = 0, p_detect_admitted = NA, p_dom = 0))
  w <- max(1, min(core) - PAD_S):min(length(t), max(core) + PAD_S); n <- length(w)
  sx_sig <- hold(s)[w]
  ys <- r * shift_series(s, lag_err); ys[!is.finite(ys)] <- 0
  if (hold_y) ys <- hold(ys)
  X <- matrix(sx_sig, NSIM, n, byrow = TRUE) + ar1_noise(n, sig$CH4, rho$CH4, NSIM)
  Y <- matrix(ys[w], NSIM, n, byrow = TRUE) + ar1_noise(n, sy, ry, NSIM)
  adm <- apply(X, 1, function(v) diff(stats::quantile(v, c(.05, .95), names = FALSE)) >= RANGE_MIN) & n >= N_MIN
  lo <- vapply(seq_len(NSIM), function(i) slope_ci(X[i, ], Y[i, ], sig$CH4, sy, BLOCK, BBOOT, seed = i)["lo"], numeric(1))
  det <- adm & lo > 0; dom <- adm & lo >= 0.5 * BETA
  c(p_admit = mean(adm), p_detect = mean(det), p_detect_admitted = if (any(adm)) mean(det[adm]) else NA, p_dom = mean(dom))
}

## ---- A. synthetic ------------------------------------------------------------------
grid <- rbind(
  expand.grid(tracer = "C2H6", A = AMP, F = FWHM, r_name = names(R_ETH), lag_err = 0, stringsAsFactors = FALSE),
  expand.grid(tracer = "C2H6", A = AMP, F = FWHM, r_name = c("r0", "f25", "adopted"), lag_err = 1, stringsAsFactors = FALSE),
  expand.grid(tracer = "CO", A = AMP, F = FWHM, r_name = as.character(R_CO), lag_err = 0, stringsAsFactors = FALSE))
grid$r <- ifelse(grid$tracer == "C2H6", R_ETH[grid$r_name], as.numeric(grid$r_name))
res <- t(vapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  if (g$tracer == "C2H6") power_cell(g$A, g$F, g$r, sig$C2H6, rho$C2H6, g$lag_err, hold_y = FALSE)
  else power_cell(g$A, g$F, g$r, sig$CO, rho$CO, 0, hold_y = TRUE)
}, numeric(4)))
SY <- cbind(grid, res)
write.csv(SY, file.path(PT, "power_synthetic.csv"), row.names = FALSE)

thr <- do.call(rbind, lapply(split(SY, list(SY$tracer, SY$F, SY$r_name, SY$lag_err), drop = TRUE), function(g) {
  g <- g[order(g$A), ]; ok <- which(g$p_detect >= 0.8); oka <- which(g$p_detect_admitted >= 0.8 & g$p_admit >= 0.5)
  data.frame(tracer = g$tracer[1], FWHM_s = g$F[1], r_name = g$r_name[1], r = g$r[1], lag_err = g$lag_err[1],
             A80_ppb = if (length(ok)) g$A[min(ok)] else NA,
             A80_given_admitted_ppb = if (length(oka)) g$A[min(oka)] else NA,
             A_admit50_ppb = if (any(g$p_admit >= 0.5)) g$A[min(which(g$p_admit >= 0.5))] else NA,
             false_pos_mean = if (g$r[1] == 0) mean(g$p_detect_admitted, na.rm = TRUE) else NA,
             false_pos_max = if (g$r[1] == 0 && any(is.finite(g$p_detect_admitted))) max(g$p_detect_admitted, na.rm = TRUE) else NA)
}))
thr <- thr[order(thr$tracer, thr$lag_err, thr$r, thr$FWHM_s), ]
write.csv(thr, file.path(PT, "power_thresholds.csv"), row.names = FALSE)

## ---- B. empirical ------------------------------------------------------------------
tab <- PW$table; win <- PW$windows
emp <- list()
for (pid in names(win)) {
  w <- win[[pid]]; x <- w$dCH4; ok <- is.finite(x); x <- x[ok]; n <- length(x)
  fl <- tab$flight[tab$plume_id == pid]; nz <- NZ[NZ$flight == fl, ]
  for (rn in c("f10", "f25", "f50", "adopted")) {
    Y <- matrix(R_ETH[rn] * x, NSIM, n, byrow = TRUE) + ar1_noise(n, nz$sigma_C2H6_ppb, nz$rho1_C2H6_ppb, NSIM)
    lo <- vapply(seq_len(NSIM), function(i) slope_ci(x, Y[i, ], nz$sigma_CH4_ppb, nz$sigma_C2H6_ppb, BLOCK, BBOOT, seed = i)["lo"], numeric(1))
    emp[[length(emp) + 1]] <- data.frame(plume_id = pid, tracer = "C2H6", r_name = rn, r = R_ETH[rn],
                                         p_detect = mean(lo > 0), n = n, peak = max(x))
  }
  for (rc in c(0.05, 0.1, 0.2)) {
    Y <- matrix(rc * x, NSIM, n, byrow = TRUE) + ar1_noise(n, nz$sigma_CO_ppb, nz$rho1_CO_ppb, NSIM)
    lo <- vapply(seq_len(NSIM), function(i) slope_ci(x, Y[i, ], nz$sigma_CH4_ppb, nz$sigma_CO_ppb, BLOCK, BBOOT, seed = i)["lo"], numeric(1))
    emp[[length(emp) + 1]] <- data.frame(plume_id = pid, tracer = "CO", r_name = as.character(rc), r = rc,
                                         p_detect = mean(lo > 0), n = n, peak = max(x))
  }
}
EM <- do.call(rbind, emp); rownames(EM) <- NULL
write.csv(EM, file.path(PT, "power_empirical.csv"), row.names = FALSE)

## ---- report ------------------------------------------------------------------------
cat(sprintf("Noise used (campaign median): CH4 %.2f ppb (rho %.2f), C2H6 %.3f ppb (rho %.2f), CO %.1f ppb (rho %.2f); NSIM %d\n",
            sig$CH4, rho$CH4, sig$C2H6, rho$C2H6, sig$CO, rho$CO, NSIM))
cat("\nSmallest peak dCH4 (ppb) at which a plume is admitted by the 67 rules in >= 50% of draws (FWHM 5 s: core never >= 10 s):\n")
print(stats::reshape(thr[thr$tracer == "C2H6" & thr$r_name == "adopted" & thr$lag_err == 0, c("r_name", "FWHM_s", "A_admit50_ppb")],
                     idvar = "r_name", timevar = "FWHM_s", direction = "wide"), row.names = FALSE)
cat("\nSmallest peak dCH4 (ppb) with P(detect | admitted) >= 0.8 [NA = not reached by 300 ppb]:\n")
print(stats::reshape(thr[thr$r > 0, c("tracer", "r_name", "lag_err", "FWHM_s", "A80_given_admitted_ppb")],
                     idvar = c("tracer", "r_name", "lag_err"), timevar = "FWHM_s", direction = "wide"), row.names = FALSE)
cat("\nFalse-positive rate among admitted plumes (r = 0; nominal one-sided 2.5%), mean and max over amplitudes:\n")
print(thr[thr$r == 0, c("tracer", "lag_err", "FWHM_s", "false_pos_mean", "false_pos_max")], row.names = FALSE, digits = 2)
cat("\nEmpirical plumes: median P(detect) across the admitted plumes, and share with P >= 0.8:\n")
print(aggregate(p_detect ~ tracer + r_name, EM, function(v) c(median = round(median(v), 2), share80 = round(mean(v >= 0.8), 2))))

## ---- figure -------------------------------------------------------------------------
COLR <- c(f10 = "#86b6ef", f25 = "#3987e5", f50 = "#1c5cab", DJB_low = "#eda100", adopted = "#eb6834")
png(file.path(FIG, "plume_detection_power.png"), 1800, 1150, res = 150)
par(mfrow = c(2, 3), mar = c(4.2, 4.3, 2.6, 1), las = 1, bty = "l")
for (F in c(5, 10, 20)) {
  plot(NA, xlim = range(AMP), ylim = c(0, 1), log = "x", xlab = "peak dCH4 (ppb)", ylab = "P(fossil detected)",
       main = sprintf("Ethane, synthetic plume FWHM %d s (~%.1f km)", F, F * 0.05), cex.main = 0.9)
  abline(h = c(0.05, 0.8), col = "grey85", lty = 2)
  for (rn in names(COLR)) { g <- SY[SY$tracer == "C2H6" & SY$F == F & SY$r_name == rn & SY$lag_err == 0, ]
    lines(g$A, g$p_detect, col = COLR[rn], lwd = 2) }
  g <- SY[SY$tracer == "C2H6" & SY$F == F & SY$r_name == "r0" & SY$lag_err == 0, ]; lines(g$A, g$p_detect, col = "#3d3d3a", lty = 3, lwd = 2)
  g <- SY[SY$tracer == "C2H6" & SY$F == F & SY$r_name == "f25" & SY$lag_err == 1, ]; lines(g$A, g$p_detect, col = COLR["f25"], lty = 2, lwd = 1.5)
  if (F == 5) text(40, 0.55, "never admitted:\ncore > 20 ppb lasts < 10 s", cex = 0.8, col = "#3d3d3a")
  if (F == 5) legend("bottomright", c("10% fossil", "25% fossil", "50% fossil", "0.061 (DJB low)", "0.102 (adopted)",
                                      "25% fossil, 1-s lag error", "r = 0 (false positive)"),
                     col = c(COLR, COLR["f25"], "#3d3d3a"), lty = c(1, 1, 1, 1, 1, 2, 3), lwd = 2, bty = "n", cex = 0.7)
}
COLC <- c("0.02" = "#b7d3f6", "0.05" = "#6da7ec", "0.1" = "#2a78d6", "0.2" = "#184f95", "0.5" = "#0d366b")
for (F in c(10, 20)) {
  plot(NA, xlim = range(AMP), ylim = c(0, 1), log = "x", xlab = "peak dCH4 (ppb)", ylab = "P(CO/CH4 slope detected)",
       main = sprintf("CO, synthetic plume FWHM %d s", F), cex.main = 0.9)
  abline(h = c(0.05, 0.8), col = "grey85", lty = 2)
  for (rc in names(COLC)) { g <- SY[SY$tracer == "CO" & SY$F == F & SY$r_name == rc, ]; lines(g$A, g$p_detect, col = COLC[rc], lwd = 2) }
  g <- SY[SY$tracer == "CO" & SY$F == F & SY$r_name == "0", ]; lines(g$A, g$p_detect, col = "#3d3d3a", lty = 3, lwd = 2)
  if (F == 10) legend("topleft", paste("dCO/dCH4 =", names(COLC)), col = COLC, lwd = 2, bty = "n", cex = 0.7)
}
e <- EM[EM$tracer == "C2H6" & EM$r_name == "f10", ]; ec <- EM[EM$tracer == "CO" & EM$r_name == "0.1", ]
plot(e$peak, e$p_detect, log = "x", pch = 19, col = adjustcolor(COLR["f10"], 0.8), ylim = c(0, 1),
     xlab = "peak dCH4 of the observed plume (ppb)", ylab = "P(detect)", main = "Observed plume shapes", cex.main = 0.9)
points(ec$peak, ec$p_detect, pch = 17, col = adjustcolor("#2a78d6", 0.8))
legend("bottomright", c("ethane at 10% fossil", "CO at dCO/dCH4 = 0.1"), pch = c(19, 17), col = c(COLR["f10"], "#2a78d6"), bty = "n", cex = 0.75)
dev.off()
cat("\nWrote power_synthetic.csv, power_thresholds.csv, power_empirical.csv, figures/plume_detection_power.png\n")
