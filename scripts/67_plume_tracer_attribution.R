# 67_plume_tracer_attribution.R ------------------------------------------------------
# EXPLORATORY (plume-level tracer analysis, step 2 of 3; needs 66, feeds 68).
# Nothing here changes a published number.
#
# Question (deliberately narrow): is any of the summertime fossil methane the
# aircraft sampled MEASURABLY combustion-associated? Not: how much Denver methane
# comes from incomplete combustion. Everything is conditional on the late
# June - mid July 2024 sampling; a null here says nothing about heating season.
#
#  1. Lags from 66 (C2H6 shifted; CO/CO2 share the CH4 cavity).
#  2. Plume events: contiguous dCH4 > 20 ppb inside one level leg (gaps <= 3 s
#     bridged, core >= 10 s), fitted over the core +/- 10 s inside the leg.
#     Background = rolling 5th percentile / 300 s on the concatenated legs of each
#     region, as add_enhancements() does for the urban analysis.
#  3. Admission: mean AGL >= QC_AGL_MIN (config; airfield approaches), >= 20 valid
#     samples, CH4 dynamic range (p95 - p5 of dCH4 in the window) >= 25 ppb.
#  4. Ethane: errors-in-variables (York/Deming, constant per-flight precisions from
#     66) slope of dC2H6 on dCH4. 95% interval = union of a 5-s moving-block
#     bootstrap and an AR(1)-inflated analytic interval (slope_ci(); the bootstrap
#     alone was anticonservative in the script-68 null simulations).
#     Classes:  fossil detected  = CI lower > 0;
#               fossil dominant  = CI lower >= 0.5 x 0.102 (>= half fossil at the
#                                  adopted endmember);
#               not detected     = CI includes 0.
#     Also: whether the CI overlaps the pure-fossil endmember range 0.061-0.110.
#     R^2 is reported as a DESCRIPTIVE number only.
#  5. Detectability vs plume size (Spearman; binned detection rate; CI width).
#     The formal power analysis is script 68.
#  6. Within ethane-detected plumes vs the rest: dCO/dCH4, dCH4/dCO2, dCO/dCO2
#     (same fits). "Combustion-coherent" = CO/CH4 and CO2/CH4 and CO/CO2 CIs all
#     above 0. Null: CO and CO2 shifted together by +/-90 s (breaks co-location
#     with CH4, keeps their autocorrelation and their mutual relation).
#     Summer CO2 carries respiration/uptake, so CO2 CORROBORATES; it is not required
#     to be clean (negative CO2/CH4 slopes are reported, not discarded).
#  7. Map with plume-mean wind (arrow points upwind) and landmarks.
#
#   eval "$(bash run_local.sh --print-env)"; Rscript scripts/67_plume_tracer_attribution.R
#   Out: <OUT_DIR>/plume_tracer/plume_table.csv, plume_summary.txt, plume_windows.rds,
#        figures/plume_ethane_vs_size.png, plume_combustion_tracers.png, plume_map.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "plume_tracer.R"))

PT <- file.path(OUT_DIR, "plume_tracer"); FIG <- file.path(PT, "figures")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
lagf <- file.path(PT, "plume_lags_by_flight.csv"); nzf <- file.path(PT, "plume_noise_by_flight.csv")
if (!file.exists(lagf) || !file.exists(nzf)) stop("run scripts/66_plume_lags.R first")
LAGS <- read.csv(lagf, stringsAsFactors = FALSE); NZ <- read.csv(nzf, stringsAsFactors = FALSE)

ENH_MIN <- 20; GAP_S <- 3; CORE_MIN <- 10; PAD_S <- 10
N_MIN <- 20; RANGE_MIN <- 25
BETA <- SOURCE_C2H6_CH4                                   # 0.102 (config)
FOSSIL_RANGE <- c(0.061, SOURCE_C2H6_CH4_DENVER_DELIVERED)  # DJB lower case .. delivered gas 0.110
BLOCK <- 5; B <- 1000; NULL_SHIFT <- 90; VIF_MAX <- 5
LANDMARKS <- data.frame(   # approximate; confirm on a map before quoting
  name = c("Suncor / Cherokee", "Tower Rd landfill", "DADS landfill", "Boulder airport"),
  lat  = c(39.802, 39.852, 39.662, 40.039), lon = c(-104.950, -104.758, -104.690, -105.226))

fl_files <- list_flights(DATA_DIR); fl_files <- fl_files[grepl("ARL-Suite", fl_files)]
fl_name <- function(p) sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))

fit_row <- function(x, y, sx, sy, key, seed) {
  f <- slope_ci(x, y, sx, sy, L = BLOCK, B = B, seed = seed)
  ok <- is.finite(x) & is.finite(y)
  r2 <- if (sum(ok) > 5) suppressWarnings(stats::cor(x[ok], y[ok])^2) else NA
  stats::setNames(c(f[c("slope", "lo", "hi")], r2), paste0(key, c("", "_lo", "_hi", "_r2")))
}

rows <- list(); windows <- list(); drop_log <- c(agl = 0, n = 0, range = 0)
for (p in fl_files) {
  fl <- fl_name(p); nz <- NZ[NZ$flight == fl, ]
  if (!nrow(nz)) next
  d <- detect_level_legs(read_icartt(p)$data); if (!any(d$leg_id > 0)) next
  lg <- tag_region(leg_metrics(d)); d$region <- lg$region[match(d$leg_id, lg$leg_id)]
  k_eth <- LAGS$adopted_lag[LAGS$flight == fl & LAGS$tracer == "C2H6_ppb"]
  d$C2H6_al  <- shift_series(d$C2H6_ppb, k_eth)
  d$C2H6_pm1 <- shift_series(d$C2H6_ppb, k_eth + 1); d$C2H6_mm1 <- shift_series(d$C2H6_ppb, k_eth - 1)
  d$CO2_ppbv <- d$CO2_ppm * 1000
  d$CO_null_p <- shift_series(d$CO_ppb, NULL_SHIFT);  d$CO2_null_p <- shift_series(d$CO2_ppbv, NULL_SHIFT)
  d$CO_null_m <- shift_series(d$CO_ppb, -NULL_SHIFT); d$CO2_null_m <- shift_series(d$CO2_ppbv, -NULL_SHIFT)
  sCH4 <- nz$sigma_CH4_ppb; sC2 <- nz$sigma_C2H6_ppb; sCO <- nz$sigma_CO_ppb; sCO2 <- nz$sigma_CO2_ppm * 1000

  for (reg in c("urban", "edge", "basin")) {
    sub <- d[d$leg_id > 0 & d$region %in% reg, ]; if (nrow(sub) < 100) next
    sub <- add_enhancements(sub, "CH4_ppb")
    for (s in c("C2H6_al", "CO_ppb", "CO2_ppbv")) sub[[paste0(s, "_enh")]] <- sub[[s]] - rolling_baseline(sub[[s]])
    ev <- find_plume_events(sub$CH4_ppb_enh, sub$leg_id, ENH_MIN, GAP_S, CORE_MIN)
    for (e in seq_len(nrow(ev))) {
      i0 <- ev$i0[e]; i1 <- ev$i1[e]; legrows <- which(sub$leg_id == sub$leg_id[i0])
      w <- max(min(legrows), i0 - PAD_S):min(max(legrows), i1 + PAD_S); core <- i0:i1
      W <- sub[w, ]; x <- W$CH4_ppb_enh
      pid <- sprintf("%s_%s_L%02d_%05d", fl, reg, sub$leg_id[i0], sub$StartTime_UTsec[i0])
      agl <- mean(sub$ALTAGL[core], na.rm = TRUE)
      nval <- sum(is.finite(x) & is.finite(W$C2H6_al))
      rng <- diff(stats::quantile(x, c(0.05, 0.95), na.rm = TRUE, names = FALSE))
      if (is.finite(agl) && agl < QC_AGL_MIN) { drop_log["agl"] <- drop_log["agl"] + 1; next }
      if (nval < N_MIN) { drop_log["n"] <- drop_log["n"] + 1; next }
      if (!is.finite(rng) || rng < RANGE_MIN) { drop_log["range"] <- drop_log["range"] + 1; next }
      seed <- sum(utf8ToInt(pid))
      r <- data.frame(plume_id = pid, flight = fl, region = reg, leg_id = sub$leg_id[i0],
        t0_utsec = sub$StartTime_UTsec[i0], dur_s = length(core), n_win = length(w), n_valid = nval,
        lat = mean(sub$Latitude[core], na.rm = TRUE), lon = mean(sub$Longitude[core], na.rm = TRUE),
        agl_m = agl, ws = mean(sub$WS[core], na.rm = TRUE), wd = wd_mean(sub$WD[core], sub$WS[core]),
        peak_dCH4 = max(sub$CH4_ppb_enh[core], na.rm = TRUE), range_dCH4 = rng,
        int_dCH4_ppb_km = sum(sub$CH4_ppb_enh[core] * sub$GroundSpeed[core] / 1000, na.rm = TRUE),
        sigma_CH4 = sCH4, sigma_C2H6 = sC2, sigma_CO = sCO, sigma_CO2_ppm = sCO2 / 1000)
      r <- cbind(r, t(fit_row(x, W$C2H6_al_enh, sCH4, sC2, "eth", seed)))
      for (sh in c("pm1", "mm1")) {
        f <- slope_ci(x, W[[paste0("C2H6_", sh)]], sCH4, sC2, BLOCK, B, seed)
        r[[paste0("eth_lo_lag", sh)]] <- f["lo"]
      }
      r <- cbind(r, t(fit_row(x, W$CO_ppb_enh, sCH4, sCO, "co_ch4", seed + 1)),
                    t(fit_row(W$CO2_ppbv_enh, x, sCO2, sCH4, "ch4_co2", seed + 2)),
                    t(fit_row(W$CO2_ppbv_enh / 1000, W$CO_ppb_enh, sCO2 / 1000, sCO, "co_co2", seed + 3)))
      for (sh in c("p", "m")) {
        cN <- W[[paste0("CO_null_", sh)]]; c2N <- W[[paste0("CO2_null_", sh)]]
        r[[paste0("null_", sh, "_co_ch4_lo")]]  <- slope_ci(x, cN, sCH4, sCO, BLOCK, 300, seed + 4)["lo"]
        r[[paste0("null_", sh, "_ch4_co2_lo")]] <- slope_ci(c2N, x, sCO2, sCH4, BLOCK, 300, seed + 5)["lo"]
        r[[paste0("null_", sh, "_co_co2_lo")]]  <- slope_ci(c2N / 1000, cN, sCO2 / 1000, sCO, BLOCK, 300, seed + 6)["lo"]
      }
      pc <- partition_boot(x, W$C2H6_al_enh, W$CO_ppb_enh, BETA, BLOCK, B, seed + 7)
      names(pc) <- paste0("part_co_", names(pc)); r <- cbind(r, t(pc))
      for (sh in c("p", "m")) {
        pn <- partition_boot(x, W$C2H6_al_enh, W[[paste0("CO_null_", sh)]], BETA, BLOCK, 300, seed + 8)
        r[[paste0("null_", sh, "_part_fossil")]] <- pn["a_lo"] > 0 & pn["d_lo"] > 0
        r[[paste0("null_", sh, "_part_micro")]]  <- pn["b_lo"] > 0 & pn["d_hi"] < 0
      }
      rows[[pid]] <- r
      windows[[pid]] <- data.frame(dCH4 = x, dC2H6 = W$C2H6_al_enh, dCO = W$CO_ppb_enh,
                                   in_core = w %in% core)
    }
  }
  cat(sprintf("%s: %d plumes so far\n", fl, length(rows)))
}
P <- do.call(rbind, rows); rownames(P) <- NULL

## ---- classification -------------------------------------------------------------
P$f_fossil <- P$eth / BETA; P$f_lo <- P$eth_lo / BETA; P$f_hi <- P$eth_hi / BETA
P$eth_class <- ifelse(P$eth_lo >= 0.5 * BETA, "fossil dominant",
               ifelse(P$eth_lo > 0, "fossil detected", "not detected"))
P$fossil_detected <- P$eth_lo > 0
P$ci_overlaps_pure_fossil <- P$eth_hi >= FOSSIL_RANGE[1] & P$eth_lo <= FOSSIL_RANGE[2]
P$lag_sensitive <- (P$eth_lo_lagpm1 > 0) != P$fossil_detected | (P$eth_lo_lagmm1 > 0) != P$fossil_detected
P$co_pos <- P$co_ch4_lo > 0; P$co2_pos <- P$ch4_co2_lo > 0; P$coco2_pos <- P$co_co2_lo > 0
P$combustion_coherent <- P$co_pos & P$co2_pos & P$coco2_pos
for (sh in c("p", "m")) P[[paste0("null_", sh, "_coherent")]] <-
  P[[paste0("null_", sh, "_co_ch4_lo")]] > 0 & P[[paste0("null_", sh, "_ch4_co2_lo")]] > 0 & P[[paste0("null_", sh, "_co_co2_lo")]] > 0
P$co_co2_ratio_ppb_per_ppm <- P$co_co2
# 6b. within-plume partition of CO between ethane-carrying and ethane-free methane
P$part_resolvable <- is.finite(P$part_co_vif) & P$part_co_vif < VIF_MAX
P$part_co_verdict <- ifelse(!P$part_resolvable, "not resolvable (collinear)",
                     ifelse(P$part_co_a_lo > 0 & P$part_co_d_lo > 0, "CO tracks fossil CH4",
                     ifelse(P$part_co_b_lo > 0 & P$part_co_d_hi < 0, "CO tracks ethane-free CH4",
                     ifelse(P$part_co_a_lo > 0 & P$part_co_b_lo > 0, "CO tracks both", "unresolved"))))
P$near_landmark <- sapply(seq_len(nrow(P)), function(i) {
  dk <- hav_km(P$lat[i], P$lon[i], LANDMARKS$lat, LANDMARKS$lon)
  if (min(dk) <= 5) LANDMARKS$name[which.min(dk)] else "" })
write.csv(P, file.path(PT, "plume_table.csv"), row.names = FALSE)
saveRDS(list(windows = windows, table = P), file.path(PT, "plume_windows.rds"))

## ---- summary --------------------------------------------------------------------
sink(file.path(PT, "plume_summary.txt"), split = TRUE)
cat(sprintf("Plumes admitted: %d in %d flights (dropped: %d below %d m AGL, %d < %d samples, %d CH4 range < %d ppb)\n",
            nrow(P), length(unique(P$flight)), drop_log["agl"], QC_AGL_MIN, drop_log["n"], N_MIN, drop_log["range"], RANGE_MIN))
cat("\nEthane class by region:\n"); print(table(P$region, P$eth_class))
cat(sprintf("\nMedian plume f_fossil (slope/0.102): %.2f (urban %.2f); CI overlaps pure-fossil range 0.061-0.110 in %d plumes\n",
            median(P$f_fossil), median(P$f_fossil[P$region == "urban"]), sum(P$ci_overlaps_pure_fossil)))
cat(sprintf("Ethane lag +/-1 s changes 'fossil detected' for %d of %d plumes\n", sum(P$lag_sensitive, na.rm = TRUE), nrow(P)))

cat("\n-- Step 5: detectability vs plume size --\n")
cat(sprintf("Spearman(peak dCH4, fossil detected) = %.2f; Spearman(peak, eth R2) = %.2f; Spearman(peak, CI width) = %.2f\n",
            stats::cor(P$peak_dCH4, P$fossil_detected, method = "spearman"),
            stats::cor(P$peak_dCH4, P$eth_r2, method = "spearman"),
            stats::cor(P$peak_dCH4, P$eth_hi - P$eth_lo, method = "spearman")))
P$size_bin <- cut(P$peak_dCH4, c(20, 40, 70, 120, Inf), right = FALSE)
print(aggregate(cbind(fossil_detected, ci_width = eth_hi - eth_lo, eth_r2) ~ size_bin, P,
                function(v) round(mean(v), 3)))

cat("\n-- Step 6: combustion tracers, ethane-detected vs not --\n")
grp <- ifelse(P$fossil_detected, "ethane detected", "ethane not detected")
tab <- aggregate(cbind(co_pos, co2_pos, coco2_pos, combustion_coherent, null_p_coherent, null_m_coherent) ~ grp,
                 data.frame(grp, P), function(v) round(mean(v, na.rm = TRUE), 3))
tab$n <- as.vector(table(grp)[tab$grp]); print(tab, row.names = FALSE)
ft <- stats::fisher.test(table(P$fossil_detected, P$combustion_coherent))
cat(sprintf("Fisher test, coherent combustion signature vs ethane detection: OR %.2f, p = %.3f\n", ft$estimate, ft$p.value))
fd <- P[P$fossil_detected, ]
cat(sprintf("\nEthane-detected plumes: %d; combustion-coherent: %d (null expectation %.1f)\n",
            nrow(fd), sum(fd$combustion_coherent), nrow(fd) * mean(c(P$null_p_coherent, P$null_m_coherent), na.rm = TRUE)))
if (any(fd$combustion_coherent)) {
  cat("Combustion-coherent, ethane-detected plumes:\n")
  print(fd[fd$combustion_coherent, c("plume_id", "wd", "peak_dCH4", "f_fossil", "co_ch4", "ch4_co2", "co_co2",
                                     "part_co_verdict", "near_landmark")], row.names = FALSE, digits = 3)
}
cat("\nCombustion-coherent rate by fossil fraction and by region:\n")
P$f_bin <- cut(P$f_fossil, c(-Inf, 0.2, 0.3, 0.4, 0.5, Inf))
print(aggregate(cbind(combustion_coherent, co_pos) ~ f_bin, P, function(v) round(mean(v), 2)))
print(aggregate(cbind(combustion_coherent, co_pos) ~ region, P, function(v) round(mean(v), 2)))
cat("Flights contributing combustion-coherent plumes:\n"); print(sort(table(P$flight[P$combustion_coherent]), decreasing = TRUE))
cat(sprintf("\n-- Step 6b: within-plume partition of dCO (VIF < %d: ethane ratio varies inside the plume) --\n", VIF_MAX))
print(table(P$part_co_verdict, P$eth_class))
rs <- P[P$part_resolvable, ]
cat(sprintf("Null (CO shifted +/-90 s), resolvable plumes: 'CO tracks fossil' %.1f%%, 'CO tracks ethane-free' %.1f%% (observed %.1f%%, %.1f%%)\n",
    100 * mean(c(rs$null_p_part_fossil, rs$null_m_part_fossil), na.rm = TRUE),
    100 * mean(c(rs$null_p_part_micro, rs$null_m_part_micro), na.rm = TRUE),
    100 * mean(rs$part_co_verdict == "CO tracks fossil CH4"), 100 * mean(rs$part_co_verdict == "CO tracks ethane-free CH4")))
tf <- P[P$part_co_verdict == "CO tracks fossil CH4", ]
cat(sprintf("'CO tracks fossil CH4' plumes: %d; of these %d have no detected ethane:methane slope, %d have no positive CO/CH4 slope, %d come from one flight (%s)\n",
    nrow(tf), sum(!tf$fossil_detected), sum(!tf$co_pos), max(table(tf$flight)), names(which.max(table(tf$flight)))))
cat("  -> CO co-varying with ETHANE structure even where the CH4 slope carries none points to air-mass-scale\n     co-variation (e.g. advected urban outflow), not to a methane emission process.\n")
cat("Resolvable plumes by region:\n"); print(table(P$region[P$part_resolvable], P$part_co_verdict[P$part_resolvable]))
cat(sprintf("\nCO/CO2 (ppb/ppm) in combustion-coherent plumes (all classes): median %.1f, IQR %.1f-%.1f, n = %d\n",
            median(P$co_co2[P$combustion_coherent]), stats::quantile(P$co_co2[P$combustion_coherent], .25),
            stats::quantile(P$co_co2[P$combustion_coherent], .75), sum(P$combustion_coherent)))
cat(sprintf("Negative CH4/CO2 slope (CI < 0; CO2 uptake or unrelated CO2 structure): %d plumes\n", sum(P$ch4_co2_hi < 0)))
cat("\nAll interpretation is conditional on summer (late June - mid July 2024) sampling.\n")
sink()

## ---- figures --------------------------------------------------------------------
COL <- c("fossil dominant" = "#eb6834", "fossil detected" = "#eda100", "not detected" = "#8a8a85")
INK <- "#3d3d3a"

png(file.path(FIG, "plume_ethane_vs_size.png"), 1600, 700, res = 150)
par(mfrow = c(1, 2), mar = c(4.3, 4.5, 2.5, 1), las = 1, bty = "l")
o <- order(P$peak_dCH4)
plot(P$peak_dCH4, P$eth, log = "x", pch = 19, cex = 0.8, col = COL[P$eth_class],
     ylim = range(c(P$eth_lo, P$eth_hi), na.rm = TRUE), xlab = "peak dCH4 (ppb)",
     ylab = "dC2H6/dCH4 (errors-in-variables slope)", main = "Ethane slope and 95% CI vs plume size", cex.main = 0.9)
segments(P$peak_dCH4, P$eth_lo, P$peak_dCH4, P$eth_hi, col = adjustcolor(COL[P$eth_class], 0.5))
rect(10, FOSSIL_RANGE[1], 1e4, FOSSIL_RANGE[2], col = adjustcolor("#eb6834", 0.1), border = NA)
abline(h = 0, col = INK); text(par("usr")[1] * 0, FOSSIL_RANGE[2], "", cex = 0.7)
legend("topleft", c(names(COL), "pure-fossil endmembers 0.061-0.110"), pch = c(19, 19, 19, 15),
       col = c(COL, adjustcolor("#eb6834", 0.25)), bty = "n", cex = 0.75)
plot(P$peak_dCH4, P$eth_r2, log = "x", pch = 19, cex = 0.8, col = COL[P$eth_class], ylim = c(0, 1),
     xlab = "peak dCH4 (ppb)", ylab = "R^2 (C2H6 vs CH4), descriptive", main = "R^2 vs plume size (descriptive only)", cex.main = 0.9)
dev.off()

png(file.path(FIG, "plume_combustion_tracers.png"), 1800, 650, res = 150)
par(mfrow = c(1, 3), mar = c(4.3, 4.5, 2.5, 1), las = 1, bty = "l")
spec <- list(c("co_ch4", "dCO/dCH4 (ppb/ppb)"), c("ch4_co2", "dCH4/dCO2 (ppb/ppb)"), c("co_co2", "dCO/dCO2 (ppb/ppm)"))
for (s in spec) {
  v <- P[[s[1]]]; lo <- P[[paste0(s[1], "_lo")]]; hi <- P[[paste0(s[1], "_hi")]]
  xx <- P$f_fossil
  lim <- range(stats::quantile(v, c(0.02, 0.98), na.rm = TRUE), 0) * 1.3
  plot(xx, v, pch = 19, cex = 0.8, col = COL[P$eth_class], ylim = lim, xlab = "fossil fraction (ethane slope / 0.102)",
       ylab = s[2], main = s[2], cex.main = 0.9)
  segments(xx, lo, xx, hi, col = adjustcolor(COL[P$eth_class], 0.35)); abline(h = 0, col = INK)
  points(xx[P$combustion_coherent], v[P$combustion_coherent], pch = 1, cex = 1.4, col = INK)
}
legend("topright", c(names(COL), "combustion-coherent (ring)"), pch = c(19, 19, 19, 1), col = c(COL, INK), bty = "n", cex = 0.75)
dev.off()

png(file.path(FIG, "plume_map.png"), 1100, 1200, res = 150)
par(mar = c(4.2, 4.2, 2.5, 1), las = 1)
plot(P$lon, P$lat, type = "n", asp = 1 / cos(40 * pi / 180), xlab = "Longitude", ylab = "Latitude",
     main = "Plumes by ethane class (filled = combustion-coherent); arrows point upwind", cex.main = 0.85)
rect(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n, border = "grey60", lty = 2)
abline(h = BASIN_LAT, col = "grey70", lty = 3)
ar <- 0.03
arrows(P$lon, P$lat, P$lon + ar * sin(P$wd * pi / 180) / cos(40 * pi / 180), P$lat + ar * cos(P$wd * pi / 180),
       length = 0.04, col = adjustcolor(COL[P$eth_class], 0.6))
cx <- 0.6 + 1.6 * sqrt(P$peak_dCH4 / max(P$peak_dCH4))
points(P$lon, P$lat, pch = ifelse(P$combustion_coherent, 19, 1), col = COL[P$eth_class], cex = cx, lwd = 1.3)
points(LANDMARKS$lon, LANDMARKS$lat, pch = 4, col = INK); text(LANDMARKS$lon, LANDMARKS$lat, LANDMARKS$name, pos = 4, cex = 0.65, col = INK)
legend("bottomright", names(COL), pch = 19, col = COL, bty = "n", cex = 0.75)
dev.off()
cat("\nWrote plume_table.csv, plume_summary.txt, plume_windows.rds and three figures to ", PT, "\n")
