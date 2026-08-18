# 31_basin_ethane_trend.R -----------------------------------------------------
# DJB BASIN ethane:methane enhancement ratio from the 2024 AMMBEC flights.
#
# WHY THIS EXISTS. Reviewers asked whether the fossil (source-gas) ethane:methane
# ratio has continued to fall after 2021. Published Front Range values compiled in
# Kille et al. (2019) Table 2 are about 0.10 to 0.19 mol/mol for the mid-2010s, and
# ethane fluxes are reported to have fallen by a factor of ~3.3 between 2015 and
# 2021 while methane fluxes stayed flat. Nothing is published for 2024. The AMMBEC
# flights sampled the basin heavily (legs north of BASIN_LAT), so they can supply a
# 2024 point measured on the same basis as the urban analysis in the manuscript.
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

# Selection thresholds, deliberately identical to the urban analysis (script 12/15)
# so the basin and urban ratios are directly comparable.
MIN_LEGS    <- 3     # basin legs required for a flight to contribute
MIN_SAMPLES <- 50    # in-basin 1 Hz samples required
MIN_ENH     <- 20    # ppb; dCH4 gate, so the fit sees plumes not baseline scatter
SD_CH4      <- 1.0   # ppb, 1-sigma instrument precision (ICARTT header)
SD_C2H6     <- 0.2   # ppb, 1-sigma instrument precision (ICARTT header)

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
  rows[[fl]] <- data.frame(
    flight        = fl,
    date          = as.character(ic$meta$date),
    n_basin_legs  = length(unique(bas)),
    n_points      = sum(k),
    basin_ratio   = round(fit$slope, 5),
    ci_lo         = round(bo$lo, 5),
    ci_hi         = round(bo$hi, 5),
    r             = round(suppressWarnings(stats::cor(x[k], y[k])), 3),
    stringsAsFactors = FALSE)
}

if (!length(rows)) {
  message("No flight had >= ", MIN_LEGS, " basin legs with usable ethane. Nothing written.")
  quit(save = "no")
}
B <- do.call(rbind, rows); rownames(B) <- NULL
B <- B[order(-B$basin_ratio), ]
write.csv(B, file.path(OUT_DIR, "basin_ethane_methane.csv"), row.names = FALSE)

good        <- B$basin_ratio[is.finite(B$basin_ratio)]
basin_med   <- stats::median(good)
basin_range <- range(good)

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

# ---- comparison with published Front Range values --------------------------
# Values live in literature_basin_ratios.csv (editable, with provenance) rather
# than hardcoded here, so they can be checked and corrected independently. The
# year_measured column ships EMPTY on purpose: the measurement years were not
# verified against the primary sources. Fill it in and this script additionally
# draws the time-series panel; leave it empty and the values are drawn as labelled
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
  abline(h = pretty(yr), col = "grey93")
  sdok <- is.finite(L$ratio_sd) & L$ratio_sd > 0
  if (any(sdok))
    arrows(L$yr[sdok], L$ratio[sdok] - L$ratio_sd[sdok],
           L$yr[sdok], L$ratio[sdok] + L$ratio_sd[sdok],
           angle = 90, code = 3, length = 0.03, col = "grey45")
  points(L$yr, L$ratio, pch = 21, bg = "grey70", cex = 1.3)
  text(L$yr, L$ratio, L$study, pos = 4, cex = 0.6, col = "grey30")
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
  abline(h = pretty(c(0, ymax)), col = "grey93")
  if (!is.null(LIT)) {
    for (i in seq_len(nrow(LIT))) {
      if (!is.finite(LIT$ratio[i])) next
      abline(h = LIT$ratio[i], lty = 3, col = "grey55")
      text(n + 0.6, LIT$ratio[i], sprintf("%s  %.3f", LIT$study[i], LIT$ratio[i]),
           pos = 4, cex = 0.58, col = "grey35", xpd = NA)
    }
  }
  arrows(seq_len(n), B$ci_lo, seq_len(n), B$ci_hi, angle = 90, code = 3,
         length = 0.035, col = "#2C7FB8", lwd = 1.6)
  points(seq_len(n), B$basin_ratio, pch = 19, col = "#1f3864", cex = 1.2)
  axis(1, at = seq_len(n), labels = sub("_R0", "", B$flight), las = 2, cex.axis = 0.6)
  abline(h = basin_med, col = "#c0392b", lwd = 2)
  text(n + 0.6, basin_med, sprintf("AMMBEC 2024 median  %.4f", basin_med),
       pos = 4, cex = 0.62, col = "#c0392b", font = 2, xpd = NA)
  abline(h = SOURCE_C2H6_CH4_ARC, lty = 2, col = "#1b7837", lwd = 1.4)
  text(n + 0.6, SOURCE_C2H6_CH4_ARC, sprintf("ARC ground  %.4f", SOURCE_C2H6_CH4_ARC),
       pos = 4, cex = 0.58, col = "#1b7837", xpd = NA)
  mtext("dotted grey: published Front Range values (see literature_basin_ratios.csv)",
        side = 3, line = 0.1, cex = 0.62, col = "grey40")
}
dev.off()

message(sprintf("Basin ethane:methane from %d flights (%d basin legs total).",
                nrow(B), sum(B$n_basin_legs)))
message(sprintf("Campaign median basin ratio = %.4f mol/mol (per-flight range %.4f to %.4f).",
                basin_med, basin_range[1], basin_range[2]))
message(sprintf("Implied beta_source = %.4f at an O&G share of 0.50, %.4f at 0.65.",
                basin_med / 0.50, basin_med / 0.65))
message(sprintf("For comparison, the adopted endmember is %.4f and the ARC ground measurement is %.4f.",
                SOURCE_C2H6_CH4, SOURCE_C2H6_CH4_ARC))
if (!has_years)
  message("literature_basin_ratios.csv has no verified measurement years, so the figure shows per-flight values against published reference lines rather than a time series. Fill in year_measured to get the trend panel.")
print(B, row.names = FALSE)
message("\nWrote basin_ethane_methane.csv, basin_implied_source_ratio.csv and figures/FigS8_basin_ethane_trend.png")
