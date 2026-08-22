# 31_basin_ethane_trend.R -----------------------------------------------------
# DJB BASIN ethane:methane enhancement ratio from the 2024 AMMBEC flights.
#
# WHY THIS EXISTS. Whether the fossil (source-gas) ethane:methane ratio has kept
# falling after 2021 bears directly on the endmember this analysis adopts.
# Published Front Range values compiled in Kille et al. (2019) Table 2 are about
# 0.10 to 0.19 mol/mol for the mid-2010s, and ethane fluxes are reported to have
# fallen by a factor of ~3.3 between 2015 and 2021 while methane fluxes stayed
# flat. Nothing is published for 2024. The AMMBEC flights sampled the basin
# heavily (legs north of BASIN_LAT), so they can supply a 2024 point measured on
# the same basis as the urban analysis in the manuscript.
#
# WHAT THIS DOES AND DOES NOT GIVE YOU. This returns an AMBIENT enhancement ratio,
# not a source-gas composition. Basin air blends ethane-rich oil-and-gas emissions
# with ethane-free methane from CAFOs, landfills and wastewater, so
#
#     basin ambient ratio  ~=  beta_source * (oil-and-gas share of basin methane)
#
# The implied source ratio therefore requires an assumed O&G share, which carries
# its own uncertainty; we report it across a range rather than as a single number.
# This is the same construction used in the AMMBEC report to CDPHE, where the ARC
# ground ratio (0.0813) is divided into the aircraft ratio to obtain the O&G share.
#
# SECOND PURPOSE. This is an independent check on the manuscript's ethane
# machinery. The AMMBEC report obtained an aircraft basin ratio of about 0.042 by a
# separate analysis of the same flights. Recovering a similar value here, with
# different code, validates the background, enhancement and York-fit chain that
# produces the urban fossil fractions.
#
# Out: <OUT_DIR>/basin_ethane_methane.csv
#      <OUT_DIR>/basin_implied_source_ratio.csv
#      <OUT_DIR>/figures/FigS8_basin_ethane_trend.png
# Run: Rscript scripts/31_basin_ethane_trend.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R"))

# Sampling thresholds, matching the urban analysis (scripts 12/15) so the basin
# and urban ratios are computed on the same basis.
MIN_LEGS    <- 3     # basin legs required for a flight to contribute
MIN_SAMPLES <- 50    # in-basin 1 Hz samples required
MIN_ENH     <- 20    # ppb; dCH4 gate, so the fit sees plumes not baseline scatter
SD_CH4      <- 1.0   # ppb, 1-sigma instrument precision (ICARTT header)
SD_C2H6     <- 0.2   # ppb, 1-sigma instrument precision (ICARTT header)

# RELIABILITY SCREEN -- and why the basin needs one when the urban analysis does not.
# The manuscript deliberately applies NO correlation screen to the urban fossil
# fraction, because over the city a weak ethane:methane correlation IS the biogenic
# signal, and screening on it would discard the biogenic-dominated flights and bias
# the source mix toward fossil. That rationale does NOT transfer to the basin. Over
# a producing gas field, ethane and methane are expected to co-vary; a weak or
# negative correlation there means the fit has failed, not that the basin is
# biogenic. Without a screen, unphysical negative slopes enter the campaign median
# and pull it down. We therefore require a positive slope, a real correlation, and
# at least two legs actually contributing to the fit (a one-leg leg-block bootstrap
# is degenerate and returns a zero-width interval).
R_MIN_BASIN   <- 0.5   # Pearson r between the gated dCH4 and dC2H6
MIN_LEGS_USED <- 2     # distinct legs among the points that survive the dCH4 gate

rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL); if (is.null(ic)) next
  if (!all(c("CH4_ppb", "C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  lg  <- tag_region(leg_metrics(d))
  bas <- lg$leg_id[lg$region == "basin"]
  db  <- d[d$leg_id %in% bas, ]
  if (length(unique(bas)) < MIN_LEGS || nrow(db) < MIN_SAMPLES) next
  db <- add_enhancements(db, "CH4_ppb"); db <- add_enhancements(db, "C2H6_ppb")
  x <- db$CH4_ppb_enh; y <- db$C2H6_ppb_enh
  k <- is.finite(x) & is.finite(y) & x > MIN_ENH
  if (sum(k) < 10) next
  fit <- york_slope(x[k], y[k], SD_CH4, SD_C2H6)
  bo  <- tryCatch(york_boot(x[k], y[k], SD_CH4, SD_C2H6, blocks = db$leg_id[k]),
                  error = function(e) list(lo = NA_real_, hi = NA_real_))
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  # legs that actually contribute to the fit, i.e. that survive the dCH4 gate. This
  # is NOT the same as the number of basin legs flown, and it is the quantity the
  # leg-block bootstrap depends on.
  n_used <- length(unique(db$leg_id[k]))
  rr     <- suppressWarnings(stats::cor(x[k], y[k]))
  rows[[fl]] <- data.frame(
    flight        = fl,
    date          = as.character(ic$meta$date),
    n_basin_legs  = length(unique(bas)),
    n_legs_used   = n_used,
    n_points      = sum(k),
    basin_ratio   = round(fit$slope, 5),
    ci_lo         = round(bo$lo, 5),
    ci_hi         = round(bo$hi, 5),
    r             = round(rr, 3),
    usable        = is.finite(fit$slope) && fit$slope > 0 &&
                    is.finite(rr) && rr >= R_MIN_BASIN && n_used >= MIN_LEGS_USED,
    stringsAsFactors = FALSE)
}

if (!length(rows)) {
  message("No flight had >= ", MIN_LEGS, " basin legs with usable ethane. Nothing written.")
  quit(save = "no")
}
B <- do.call(rbind, rows); rownames(B) <- NULL
B <- B[order(-B$basin_ratio), ]
write.csv(B, file.path(OUT_DIR, "basin_ethane_methane.csv"), row.names = FALSE)

all_fits    <- B$basin_ratio[is.finite(B$basin_ratio)]
all_med     <- stats::median(all_fits)
good        <- B$basin_ratio[B$usable & is.finite(B$basin_ratio)]
if (!length(good)) {
  message("No basin fit passed the reliability screen (r >= ", R_MIN_BASIN,
          ", positive slope, >= ", MIN_LEGS_USED, " contributing legs). ",
          "Reporting the unscreened median only.")
  good <- all_fits
}
basin_med   <- stats::median(good)
basin_range <- range(good)

# Leg-count sensitivity. A leg-block bootstrap over only two blocks has just three
# distinct resamples, so its interval is coarse. The flights carrying the highest
# ratios are also the most sparsely sampled, so we report what happens when at
# least three contributing legs are required, rather than letting a threshold
# choice sit unexamined.
sel3     <- B$usable & is.finite(B$basin_ratio) & B$n_legs_used >= 3
basin_med3 <- if (any(sel3)) stats::median(B$basin_ratio[sel3]) else NA_real_

# Implied oil-and-gas share of basin methane, taking the ARC ground ratio as the
# source endmember. This is the same two-endmember construction the AMMBEC report
# uses, so it is directly comparable to the O&G share reported there.
og_share_implied <- basin_med / SOURCE_C2H6_CH4_ARC

# ---- implied source ratio across a plausible oil-and-gas share --------------
# beta_source = basin_ambient_ratio / (O&G share of basin methane). The AMMBEC
# report to CDPHE puts the 2024 O&G share near 0.49; Kille et al. (2019) put the
# mid-2010s natural-gas share near 0.63. We span both rather than pick one.
shares <- seq(0.35, 0.80, by = 0.05)
IMP <- data.frame(og_share = shares,
                  implied_beta_source = round(basin_med / shares, 4))
IMP$note <- ""
IMP$note[which.min(abs(shares - 0.50))] <- "~AMMBEC report 2024 O&G share (0.49)"
IMP$note[which.min(abs(shares - 0.65))] <- "~Kille et al. 2019 natural-gas share (0.63)"
write.csv(IMP, file.path(OUT_DIR, "basin_implied_source_ratio.csv"), row.names = FALSE)
write.csv(data.frame(
    screened_median            = round(basin_med, 5),
    n_flights_screened         = sum(B$usable),
    median_min3_legs           = round(basin_med3, 5),
    n_flights_min3_legs        = sum(sel3),
    unscreened_median          = round(all_med, 5),
    implied_og_share_using_ARC = round(og_share_implied, 4),
    arc_endmember              = SOURCE_C2H6_CH4_ARC),
    file.path(OUT_DIR, "basin_summary.csv"), row.names = FALSE)

# ---- comparison with published Front Range values --------------------------
# Values live in literature_basin_ratios.csv (editable, with provenance) rather
# than hardcoded here, so they can be checked and corrected independently. The
# year_measured column ships EMPTY on purpose: the measurement years were not
# verified against the primary sources. Fill it in and this script additionally
# draws the time-series panel; leave it empty and the values are drawn as labeled
# reference lines instead, which asserts nothing about timing.
litp <- file.path(proj, "literature_basin_ratios.csv")
LIT  <- if (file.exists(litp)) read.csv(litp, stringsAsFactors = FALSE) else NULL
has_years <- !is.null(LIT) && any(is.finite(suppressWarnings(as.numeric(LIT$year_measured))))

FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "FigS8_basin_ethane_trend.png"), width = 1320, height = 820, res = 150)
par(mar = c(4.6, 4.8, 3.2, if (has_years) 1 else 9))

if (has_years) {
  LIT$yr <- suppressWarnings(as.numeric(LIT$year_measured))
  L <- LIT[is.finite(LIT$yr) & is.finite(LIT$ratio), ]
  xr <- range(c(L$yr, 2024)) + c(-1, 1)
  yr <- c(0, max(c(L$ratio + ifelse(is.finite(L$ratio_sd), L$ratio_sd, 0), basin_range, 0.2), na.rm = TRUE) * 1.1)
  plot(NA, xlim = xr, ylim = yr, xlab = "Year of measurement",
       ylab = expression(Delta*C[2]*H[6]/Delta*CH[4]~"(mol/mol)"),
       main = "DJB basin ethane:methane, published values and AMMBEC 2024")
  abline(h = pretty(yr), col = "gray93")
  sdok <- is.finite(L$ratio_sd) & L$ratio_sd > 0
  if (any(sdok))
    arrows(L$yr[sdok], L$ratio[sdok] - L$ratio_sd[sdok],
           L$yr[sdok], L$ratio[sdok] + L$ratio_sd[sdok],
           angle = 90, code = 3, length = 0.03, col = "gray45")
  points(L$yr, L$ratio, pch = 21, bg = "gray70", cex = 1.3)
  text(L$yr, L$ratio, L$study, pos = 4, cex = 0.6, col = "gray30")
  arrows(2024, basin_range[1], 2024, basin_range[2], angle = 90, code = 3,
         length = 0.04, col = "#c0392b", lwd = 2)
  points(2024, basin_med, pch = 19, col = "#c0392b", cex = 1.6)
  text(2024, basin_med, "AMMBEC 2024\n(this work, basin legs)", pos = 2, cex = 0.65, col = "#c0392b")
} else {
  n <- nrow(B)
  ymax <- max(c(good, if (!is.null(LIT)) LIT$ratio else NULL, SOURCE_C2H6_CH4), na.rm = TRUE) * 1.15
  plot(NA, xlim = c(0.5, n + 0.5), ylim = c(0, ymax), xaxt = "n", xlab = "",
       ylab = expression(Delta*C[2]*H[6]/Delta*CH[4]~"(mol/mol)"),
       main = "DJB basin ethane:methane per flight, AMMBEC 2024")
  abline(h = pretty(c(0, ymax)), col = "gray93")
  if (!is.null(LIT)) {
    for (i in seq_len(nrow(LIT))) {
      if (!is.finite(LIT$ratio[i])) next
      abline(h = LIT$ratio[i], lty = 3, col = "gray55")
      text(n + 0.6, LIT$ratio[i], sprintf("%s  %.3f", LIT$study[i], LIT$ratio[i]),
           pos = 4, cex = 0.58, col = "gray35", xpd = NA)
    }
  }
  # Error bars only for fits that pass the screen. Failed fits are shown as bare
  # markers: their bootstrap intervals are meaningless (one spans to 0.86) and
  # drawing them swamps the panel.
  ciok <- B$usable & is.finite(B$ci_lo) & is.finite(B$ci_hi) & (B$ci_hi - B$ci_lo) > 0
  if (any(ciok))
    arrows(which(ciok), B$ci_lo[ciok], which(ciok), B$ci_hi[ciok], angle = 90,
           code = 3, length = 0.035, col = "#2C7FB8", lwd = 1.6)
  points(seq_len(n), B$basin_ratio, pch = ifelse(B$usable, 19, 4),
         col = ifelse(B$usable, "#1f3864", "gray55"), cex = 1.2)
  axis(1, at = seq_len(n), labels = sub("_R0", "", B$flight), las = 2, cex.axis = 0.6)
  abline(h = basin_med, col = "#c0392b", lwd = 2)
  text(n + 0.6, basin_med, sprintf("AMMBEC 2024 median  %.4f", basin_med),
       pos = 4, cex = 0.62, col = "#c0392b", font = 2, xpd = NA)
  abline(h = SOURCE_C2H6_CH4_ARC, lty = 2, col = "#1b7837", lwd = 1.4)
  text(n + 0.6, SOURCE_C2H6_CH4_ARC, sprintf("ARC ground  %.4f", SOURCE_C2H6_CH4_ARC),
       pos = 4, cex = 0.58, col = "#1b7837", xpd = NA)
  mtext("filled = passes reliability screen; x = fails (unphysical or uncorrelated fit). Dotted gray: published Front Range values.",
        side = 3, line = 0.1, cex = 0.55, col = "grey40")
}
dev.off()

message(sprintf("Basin ethane:methane from %d flights (%d basin legs flown).",
                nrow(B), sum(B$n_basin_legs)))
message(sprintf("Reliability screen (slope > 0, r >= %.1f, >= %d contributing legs): %d of %d flights pass.",
                R_MIN_BASIN, MIN_LEGS_USED, sum(B$usable), nrow(B)))
message(sprintf("UNSCREENED median = %.4f mol/mol -- reported for transparency only; it includes %d flights with negative (unphysical) slopes.",
                all_med, sum(B$basin_ratio < 0, na.rm = TRUE)))
message(sprintf("SCREENED median basin ratio = %.4f mol/mol (range %.4f to %.4f). This is the value to use.",
                basin_med, basin_range[1], basin_range[2]))
message(sprintf("Implied beta_source = %.4f at an O&G share of 0.50, %.4f at 0.65.",
                basin_med / 0.50, basin_med / 0.65))
message(sprintf("For comparison, the adopted endmember is %.4f and the ARC ground measurement is %.4f.",
                SOURCE_C2H6_CH4, SOURCE_C2H6_CH4_ARC))
message(sprintf("Requiring >= 3 contributing legs (%d flights): median = %.4f, implied beta_source = %.4f at a 0.50 O&G share.",
                sum(sel3), basin_med3, basin_med3 / 0.50))
message(sprintf("Taking the ARC ground ratio (%.4f) as the endmember, the basin median implies an O&G share of %.1f%% (biogenic %.1f%%), directly comparable to the apportionment in the AMMBEC report.",
                SOURCE_C2H6_CH4_ARC, 100 * og_share_implied, 100 * (1 - og_share_implied)))
if (!has_years)
  message("literature_basin_ratios.csv has no verified measurement years, so the figure shows per-flight values against published reference lines rather than a time series. Fill in year_measured to get the trend panel.")
print(B, row.names = FALSE)
message("\nWrote basin_ethane_methane.csv, basin_implied_source_ratio.csv and figures/FigS8_basin_ethane_trend.png")
