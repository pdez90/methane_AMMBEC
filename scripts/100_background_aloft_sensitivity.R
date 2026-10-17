# 100_background_aloft_sensitivity.R -----------------------------------------
# Sensitivity of the urban fossil fraction to an ALOFT background (S. Baidar, 16 Oct 2026):
# could the rolling 5th-percentile background remove part of a spread-out urban fossil
# (area-source) enhancement? Here the background is taken instead from the air above the
# boundary layer sampled on the same flight.
#
# For each flight of Table 1:
#   aloft samples  constant-altitude transect samples at least 100 m above the boundary-layer
#                  height at that sample (airborne Doppler lidar, interpolated along the track;
#                  the flight's ground-lidar value where there is no airborne retrieval) and at
#                  least 1 km above ground. Transects over the analysis box are used if they
#                  give >= 60 samples, otherwise aloft transects anywhere on the flight.
#   background     the median of the aloft samples (one constant per species and flight), and
#                  as a variant their 5th percentile.
#   enhancement    urban-transect sample minus that constant (no rolling window), then the
#                  paper's 20 ppb gate and within-transect York fit (fossil fraction at
#                  SOURCE_C2H6_CH4) and the OLS CH4:CO slope with the day-type GRA2PES anchor.
# A constant background only changes the within-transect fit through which samples pass the
# gate and through along-transect structure the rolling window used to remove.
#
# Out: <OUT_DIR>/background_aloft_sensitivity.csv
# Run: Rscript scripts/100_background_aloft_sensitivity.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths", "massbalance", "enhancements", "ratios", "lidar_blh", "airborne_blh"))
  source(file.path(proj, "R", paste0(f, ".R")))

MW_CH4 <- 16.04; MW_CO <- 28.01; GGYR_TO_THR <- 1e3 / (365.25 * 24)
GATE_PPB <- 20; CO_GATE_PPB <- 5; MARGIN_M <- 100; MIN_AGL_M <- 1000; MIN_N <- 60
T1 <- read.csv(file.path(OUT_DIR, "table1.csv"), stringsAsFactors = FALSE)
FL9 <- T1$Flight[T1$Fossil_pct_CI != "NA"]
FS <- read.csv(file.path(OUT_DIR, "flight_summary.csv"), stringsAsFactors = FALSE)

fit <- function(z, e_co) {
  k <- is.finite(z$dch4) & is.finite(z$dc2h6) & z$dch4 > GATE_PPB
  s <- if (sum(k) >= 10) york_within_leg(z$dch4[k], z$dc2h6[k], z$leg_id[k])$slope else NA_real_
  nl <- length(unique(z$leg_id[k]))
  kco <- is.finite(z$dch4) & is.finite(z$dco) & z$dco > CO_GATE_PPB
  rco <- if (sum(kco) >= 10) suppressWarnings(stats::cor(z$dco[kco], z$dch4[kco])) else NA_real_
  bco <- if (sum(kco) >= 10) unname(stats::coef(stats::lm(z$dch4[kco] ~ z$dco[kco]))[2]) else NA_real_
  c(n_gated = sum(k), n_legs = nl, slope = s, fossil_pct = round(100 * fossil_fraction(s, SOURCE_C2H6_CH4)),
    r_co = round(rco, 2), E_co_t_hr = round(bco * (MW_CH4 / MW_CO) * e_co * GGYR_TO_THR, 2))
}

rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  if (!fl %in% FL9) next
  ic <- read_icartt(p); d <- detect_level_legs(ic$data)
  legs <- tag_region(leg_metrics(d)); urb <- legs$leg_id[legs$region == "urban"]
  # boundary-layer height at every sample (airborne lidar; ground-lidar flight value otherwise)
  blh <- airborne_blh_at(fl, d$timestamp)
  src <- "airborne lidar"
  if (all(!is.finite(blh))) { blh[] <- as.numeric(FS$blh_m[FS$flight == fl]); src <- "ground lidar (flight value)" }
  aloft <- d$leg_id > 0 & is.finite(d$ALTAGL) & is.finite(blh) & d$ALTAGL >= pmax(blh + MARGIN_M, MIN_AGL_M)
  inbox <- aloft & d$leg_id %in% legs$leg_id[legs$region == "urban"]
  use <- if (sum(inbox & is.finite(d$CH4_ppb)) >= MIN_N) inbox else aloft
  where <- if (identical(use, inbox)) "over the box" else "anywhere on the flight"
  du <- d[d$leg_id %in% urb, ]
  eco <- e_co_anchor(ic$meta$date)
  out <- list()
  for (stat in c("median", "p05")) {
    f_bg <- if (stat == "median") function(x) stats::median(x, na.rm = TRUE) else function(x) stats::quantile(x, 0.05, na.rm = TRUE, names = FALSE)
    bg <- sapply(c("CH4_ppb", "C2H6_ppb", "CO_ppb"), function(v) f_bg(d[[v]][use]))
    z <- data.frame(leg_id = du$leg_id, dch4 = du$CH4_ppb - bg[["CH4_ppb"]], dc2h6 = du$C2H6_ppb - bg[["C2H6_ppb"]], dco = du$CO_ppb - bg[["CO_ppb"]])
    out[[stat]] <- c(fit(z, eco), bg_ch4 = round(bg[["CH4_ppb"]], 1), bg_c2h6 = round(bg[["C2H6_ppb"]], 2), bg_co = round(bg[["CO_ppb"]], 1))
  }
  # the paper's background, for reference
  a <- du; for (v in c("CH4_ppb", "C2H6_ppb", "CO_ppb")) a <- add_enhancements(a, v)
  ref <- fit(data.frame(leg_id = a$leg_id, dch4 = a$CH4_ppb_enh, dc2h6 = a$C2H6_ppb_enh, dco = a$CO_ppb_enh), eco)
  n_aloft <- sum(use & is.finite(d$CH4_ppb))
  rows[[fl]] <- data.frame(flight = fl, blh_source = src, aloft_from = where, n_aloft = n_aloft,
    aloft_agl_median_m = round(stats::median(d$ALTAGL[use], na.rm = TRUE)),
    urban_ch4_p05 = round(stats::quantile(du$CH4_ppb, 0.05, na.rm = TRUE, names = FALSE), 1),
    bg_ch4_aloft = out$median[["bg_ch4"]], bg_c2h6_aloft = out$median[["bg_c2h6"]], bg_co_aloft = out$median[["bg_co"]],
    n_gated_paper = ref[["n_gated"]], fossil_pct_paper = ref[["fossil_pct"]], E_co_paper = ref[["E_co_t_hr"]], r_co_paper = ref[["r_co"]],
    n_gated_aloft = out$median[["n_gated"]], n_legs_aloft = out$median[["n_legs"]], slope_aloft = signif(out$median[["slope"]], 4),
    fossil_pct_aloft = out$median[["fossil_pct"]], r_co_aloft = out$median[["r_co"]], E_co_aloft = out$median[["E_co_t_hr"]],
    n_gated_aloft_p05 = out$p05[["n_gated"]], fossil_pct_aloft_p05 = out$p05[["fossil_pct"]], E_co_aloft_p05 = out$p05[["E_co_t_hr"]],
    stringsAsFactors = FALSE)
  message(sprintf("%-16s aloft n %5d (%s, %s; median %4.0f m AGL) bg CH4 %.1f vs urban p05 %.1f | ff paper %3s%% -> aloft %3s%% (p05 %3s%%) | n gated %4d -> %4d",
                  fl, n_aloft, where, src, stats::median(d$ALTAGL[use], na.rm = TRUE), out$median[["bg_ch4"]],
                  stats::quantile(du$CH4_ppb, 0.05, na.rm = TRUE), ref[["fossil_pct"]], out$median[["fossil_pct"]], out$p05[["fossil_pct"]],
                  ref[["n_gated"]], out$median[["n_gated"]]))
}
res <- do.call(rbind, rows)
write.csv(res, file.path(OUT_DIR, "background_aloft_sensitivity.csv"), row.names = FALSE)
message(sprintf("\ncampaign median fossil fraction: paper %.0f%%, aloft median background %.0f%%, aloft 5th percentile %.0f%%",
                median(res$fossil_pct_paper, na.rm = TRUE), median(res$fossil_pct_aloft, na.rm = TRUE), median(res$fossil_pct_aloft_p05, na.rm = TRUE)))
