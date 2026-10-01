# 66_plume_lags.R ----------------------------------------------------------------
# EXPLORATORY (plume-level tracer analysis, step 1 of 3; see 67 and 68). Nothing
# here changes a published number.
#
# Puts CH4, C2H6, CO and CO2 on a common time base before any plume-scale slope
# is fitted, and measures the effective 1-Hz noise each later step needs.
#
# Instruments (ICARTT header): Picarro G2401-m -> CH4, CO, CO2 (one cavity,
# ~2.5-s update, so ~60% of 1-Hz values repeat); Aeris MIRA -> C2H6.
# At ~50 m/s a 2-s offset is ~100 m of track, enough to decorrelate a narrow plume.
#
# LAG METHOD. Lags are estimated per flight from strong-variance 60-s windows
# only (top 15% of high-passed CH4 variance on level legs; >= 5 windows), with a
# single pooled correlation per trial lag (-15..+15 s). They are NOT estimated per
# plume: a per-plume lag would be tuned on the very covariance tested later.
# Adopted C2H6 lag = the flight estimate when it is well defined (r >= 0.3 and a
# peak contrast >= 0.05 against lags 3-6 s away), else the campaign median of the
# well-defined flights. CO and CO2 share the CH4 cavity, so their lag is 0 by
# construction; their estimates are reported as a check only.
# STABILITY CHECK (reported, not used): the C2H6 lag re-estimated inside each
# large, unmistakable plume (high-passed CH4 peak >= 100 ppb, ethane r >= 0.7).
#
# NOISE. Robust sigma (MAD) and lag-1 autocorrelation of the 61-s high-passed
# series at quiet level-leg samples (|hp CH4| < 5 ppb), after alignment. This
# includes sub-minute atmospheric variability, so it bounds instrument noise from
# above (ICARTT stated: CH4 1 ppb, C2H6 0.2 ppb, CO 5 ppb, CO2 0.1 ppm).
#
#   eval "$(bash run_local.sh --print-env)"; Rscript scripts/66_plume_lags.R
#   Out: <OUT_DIR>/plume_tracer/plume_lags_by_flight.csv, plume_lag_stability.csv,
#        plume_noise_by_flight.csv, figures/plume_lag_xcorr.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "plume_tracer.R"))

PT <- file.path(OUT_DIR, "plume_tracer"); dir.create(file.path(PT, "figures"), showWarnings = FALSE, recursive = TRUE)
TRACERS <- c("C2H6_ppb", "CO_ppb", "CO2_ppm")
R_MIN <- 0.3; CONTRAST_MIN <- 0.05; BIG_PEAK <- 100

fl_files <- list_flights(DATA_DIR); fl_files <- fl_files[grepl("ARL-Suite", fl_files)]
if (!length(fl_files)) stop("no ARL-Suite flight files under METHANE_DATA_DIR = ", DATA_DIR)
fl_name <- function(p) sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))

lag_rows <- list(); stab_rows <- list(); xc_store <- list(); flights <- list()
for (p in fl_files) {
  fl <- fl_name(p)
  d <- detect_level_legs(read_icartt(p)$data)
  if (!all(c("CH4_ppb", TRACERS) %in% names(d)) || !any(d$leg_id > 0)) next
  inleg <- d$leg_id > 0; flights[[fl]] <- d
  for (tr in TRACERS) {
    e <- estimate_lag(d$CH4_ppb, d[[tr]], inleg)
    lag_rows[[length(lag_rows) + 1]] <- data.frame(flight = fl, tracer = tr, t(e))
  }
  # full cross-correlation curve for C2H6, for the figure
  hx <- highpass(d$CH4_ppb); hy <- highpass(d$C2H6_ppb)
  xc_store[[fl]] <- sapply(-15:15, function(k) { ys <- shift_series(hy, k); ok <- inleg & is.finite(hx) & is.finite(ys)
                                                 stats::cor(hx[ok], ys[ok]) })
  # stability: per large plume
  ev <- find_plume_events(hx, d$leg_id, thr = BIG_PEAK, gap = 3, core_min = 5)
  for (i in seq_len(nrow(ev))) {
    a <- max(1, ev$i0[i] - 30); b <- min(nrow(d), ev$i1[i] + 30); w <- a:b
    rr <- sapply(-10:10, function(k) { ys <- shift_series(hy, k)[w]; ok <- is.finite(hx[w]) & is.finite(ys)
                                        if (sum(ok) < 20) NA else stats::cor(hx[w][ok], ys[ok]) })
    if (all(!is.finite(rr))) next
    stab_rows[[length(stab_rows) + 1]] <- data.frame(flight = fl, t0 = d$StartTime_UTsec[ev$i0[i]],
      peak_hp_ch4 = max(hx[ev$i0[i]:ev$i1[i]], na.rm = TRUE), lag = (-10:10)[which.max(rr)], r = max(rr, na.rm = TRUE))
  }
}
L <- do.call(rbind, lag_rows)
S <- if (length(stab_rows)) do.call(rbind, stab_rows) else data.frame()

# adopted lags
eth <- L[L$tracer == "C2H6_ppb", ]
eth$well_defined <- is.finite(eth$r) & eth$r >= R_MIN & is.finite(eth$contrast) & eth$contrast >= CONTRAST_MIN
camp <- round(stats::median(eth$lag[eth$well_defined]))   # a whole-second lag (an even count can give x.5)
L$well_defined <- NA; L$well_defined[L$tracer == "C2H6_ppb"] <- eth$well_defined
L$adopted_lag <- ifelse(L$tracer != "C2H6_ppb", 0L,
                  ifelse(L$well_defined %in% TRUE, L$lag, camp))
L$adopted_rule <- ifelse(L$tracer != "C2H6_ppb", "same Picarro cavity (0 by construction)",
                   ifelse(L$well_defined %in% TRUE, "flight estimate", "campaign median"))
write.csv(L, file.path(PT, "plume_lags_by_flight.csv"), row.names = FALSE)
if (nrow(S)) write.csv(S, file.path(PT, "plume_lag_stability.csv"), row.names = FALSE)

# noise after alignment
nz <- list()
for (fl in names(flights)) {
  d <- flights[[fl]]; inleg <- d$leg_id > 0
  lg <- L$adopted_lag[L$flight == fl & L$tracer == "C2H6_ppb"]
  h <- list(CH4_ppb = highpass(d$CH4_ppb), C2H6_ppb = highpass(shift_series(d$C2H6_ppb, lg)),
            CO_ppb = highpass(d$CO_ppb), CO2_ppm = highpass(d$CO2_ppm))
  quiet <- inleg & is.finite(h$CH4_ppb) & abs(h$CH4_ppb) < 5
  row <- data.frame(flight = fl, n_quiet = sum(quiet))
  for (s in names(h)) { ns <- noise_stats(h[[s]], quiet)
    row[[paste0("sigma_", s)]] <- ns["sigma"]; row[[paste0("rho1_", s)]] <- ns["rho1"] }
  nz[[fl]] <- row
}
N <- do.call(rbind, nz); write.csv(N, file.path(PT, "plume_noise_by_flight.csv"), row.names = FALSE)

# figure: C2H6 cross-correlation curves + stability histogram
png(file.path(PT, "figures", "plume_lag_xcorr.png"), 1500, 620, res = 150)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.5, 1), las = 1, bty = "l")
M <- do.call(cbind, xc_store)
matplot(-15:15, M, type = "l", lty = 1, col = adjustcolor("#2a78d6", 0.45), lwd = 1.2,
        xlab = "trial lag of C2H6 vs CH4 (s; >0 = ethane recorded later)", ylab = "r (61-s high-passed, level legs)",
        main = "C2H6 vs CH4 cross-correlation, one line per flight", cex.main = 0.9)
abline(v = camp, lty = 2, col = "#3d3d3a"); abline(h = 0, col = "grey80")
text(camp, max(M, na.rm = TRUE), sprintf(" campaign median %+d s", camp), adj = c(0, 1), cex = 0.8)
if (nrow(S)) {
  Sg <- S[S$r >= 0.7, ]
  hist(Sg$lag, breaks = seq(-10.5, 10.5, 1), col = "#2a78d6", border = "white",
       xlab = "C2H6 lag within one large plume (s)", ylab = "plumes",
       main = sprintf("Stability: %d large plumes (hp CH4 >= %d ppb, r >= 0.7)", nrow(Sg), BIG_PEAK), cex.main = 0.9)
  abline(v = camp, lty = 2, col = "#3d3d3a")
}
dev.off()

cat("\nC2H6 lag by flight (s; >0 = ethane recorded later than CH4):\n")
print(eth[, c("flight", "lag", "r", "r0", "n_win", "contrast", "well_defined")], row.names = FALSE, digits = 3)
cat(sprintf("\nCampaign median C2H6 lag: %+d s (%d of %d flights well defined)\n", camp, sum(eth$well_defined), nrow(eth)))
if (nrow(S)) { Sg <- S[S$r >= 0.7, ]
  cat(sprintf("Per-plume stability (n = %d): lag %s; share within 1 s of campaign median %.0f%%\n",
              nrow(Sg), paste(names(table(Sg$lag)), table(Sg$lag), sep = ":", collapse = " "),
              100 * mean(abs(Sg$lag - camp) <= 1))) }
cat("\nCO and CO2 lag estimates (check only; 0 adopted):\n")
print(L[L$tracer != "C2H6_ppb", c("flight", "tracer", "lag", "r", "r0")], row.names = FALSE, digits = 2)
cat("\nEffective 1-Hz noise, campaign median:\n")
print(round(sapply(N[, grepl("sigma|rho1", names(N))], stats::median), 3))
