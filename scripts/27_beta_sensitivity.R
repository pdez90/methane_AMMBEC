# 27_beta_sensitivity.R -------------------------------------------------------
# Sensitivity of the fossil fraction to the assumed ethane endmember beta_source.
# The absolute fossil fractions scale as 1/beta_source (Eq. 5), so the
# biogenic-leaning result depends on the adopted value. This recomputes
# every urban flight's fossil fraction across a plausible range of beta_source and
# shows that the campaign median stays biogenic (< 50% fossil) throughout, so the
# conclusion does not rest on the particular endmember chosen.
#
# Method: script 15 saves each flight's UNROUNDED York ethane:methane slope
# (c2h6_ch4_slope_york). For any endmember beta, f(beta) = max(0, min(1, slope/beta)).
# We use the raw slope, not the rounded fossil fraction, to avoid discretization.
#
# Out: <OUT_DIR>/beta_sensitivity.csv, <OUT_DIR>/figures/FigS5_beta_sensitivity.png
# Run: Rscript scripts/27_beta_sensitivity.R   (after 15)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))

beta0 <- SOURCE_C2H6_CH4                       # adopted endmember (see config.R)
rmf <- read.csv(file.path(OUT_DIR, "ratio_method_flux.csv"), stringsAsFactors = FALSE)
# use the UNROUNDED York ethane:methane slope saved by script 15 (fall back to the
# rounded fossil fraction only if an older CSV lacks the raw-slope column)
if ("c2h6_ch4_slope_york" %in% names(rmf)) {
  slope <- suppressWarnings(as.numeric(rmf$c2h6_ch4_slope_york))
} else {
  slope <- suppressWarnings(as.numeric(rmf$fossil_frac_york)) * beta0
}
keep <- is.finite(slope); slope <- slope[keep]; fl <- rmf$flight[keep]
if (!length(slope)) { message("No finite ethane slopes; run script 15 first."); quit(save = "no") }

# Swept endmember range. It spans (i) the published Front Range source-gas values
# compiled in Kille et al. (2019) Table 2, which run about 0.10 to 0.19 mol/mol;
# (ii) the roughly threefold decline in the DJB ethane:methane ratio reported
# between 2015 and 2021, which pushes the present-day value toward 0.06; and
# (iii) still lower values, because urban fossil methane is dominated by processed
# distribution gas, which is ethane-depleted relative to raw wellhead gas.
# NOT inferred as a hard bound.
# round before unique(): seq() accumulates floating-point error, so the value it
# generates at 0.11 is not bit-identical to the literal SOURCE_C2H6_CH4 and would
# otherwise survive unique() as a duplicate row.
betas <- sort(unique(round(c(seq(0.04, 0.16, by = 0.005),
                             SOURCE_C2H6_CH4, SOURCE_C2H6_CH4_ARC), 6)))
FF <- sapply(betas, function(b) pmax(0, pmin(1, slope / b)))   # rows = flights, cols = betas
med <- apply(FF, 2, stats::median)
lo  <- apply(FF, 2, min); hi <- apply(FF, 2, max)
nmaj <- apply(FF, 2, function(x) sum(x >= 0.5))       # flights that would read majority-fossil

lab <- rep("", length(betas))
lab[abs(betas - beta0) < 1e-9]              <- "adopted (lowest Front Range value, Kille et al. 2019 Table 2)"
lab[abs(betas - SOURCE_C2H6_CH4_ARC) < 1e-9] <- "ARC ground measurement, AMMBEC 2024"
out <- data.frame(beta_source = betas,
                  median_fossil_pct = round(100 * med),
                  min_fossil_pct = round(100 * lo),
                  max_fossil_pct = round(100 * hi),
                  n_flights_majority_fossil = nmaj,
                  note = lab)
write.csv(out, file.path(OUT_DIR, "beta_sensitivity.csv"), row.names = FALSE)

FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "FigS5_beta_sensitivity.png"), width = 1150, height = 820, res = 150)
par(mar = c(4.5, 4.5, 3, 1))
plot(NA, xlim = range(betas), ylim = c(0, 100), xlab = expression(beta[source]~"(ethane:methane of source gas, mol/mol)"),
     ylab = "Fossil fraction of urban methane (%)",
     main = "Fossil fraction vs. assumed ethane endmember")
# min-max band across flights
polygon(c(betas, rev(betas)), 100 * c(lo, rev(hi)), col = "#dfe8f3", border = NA)
# per-flight curves
for (i in seq_len(nrow(FF))) lines(betas, 100 * FF[i, ], col = "#9db8d2", lwd = 1)
# median
lines(betas, 100 * med, col = "#1f3864", lwd = 3)
abline(h = 50, lty = 2, col = "#c0392b", lwd = 2)             # majority-fossil line
abline(v = beta0, lty = 3, col = "#444444", lwd = 1.5)        # adopted value
abline(v = SOURCE_C2H6_CH4_ARC, lty = 3, col = "#1b7837", lwd = 1.5)  # ARC-measured value
if (is.finite(beta_break_plot <- tryCatch(stats::uniroot(function(b)
      stats::median(pmax(0, pmin(1, slope / b))) - 0.5, c(0.005, 1))$root,
      error = function(e) NA_real_))) {
  abline(v = beta_break_plot, lty = 1, col = "#c0392b", lwd = 1.2)
  text(beta_break_plot, 8, sprintf("median crosses 50%% at %.3f", beta_break_plot),
       col = "#c0392b", cex = 0.7, pos = 4)
}
text(SOURCE_C2H6_CH4_ARC, 88, sprintf("ARC measured %.4f", SOURCE_C2H6_CH4_ARC),
     col = "#1b7837", cex = 0.7, pos = 4)
text(mean(range(betas)), 53, "majority fossil", col = "#c0392b", cex = 0.8, pos = 3)
text(beta0, 96, sprintf("adopted %.3f", beta0), col = "#444444", cex = 0.75, pos = 4)
legend("topright", bty = "n", cex = 0.8, lwd = c(3, 1), col = c("#1f3864", "#9db8d2"),
       legend = c("campaign median", "individual flights"))
dev.off()

message(sprintf("Adopted beta_source = %.3f. Median fossil fraction across beta = %.3f to %.3f: %d%% to %d%%.",
                beta0, min(betas), max(betas), round(100*min(med)), round(100*max(med))))
message(sprintf("Campaign median is biogenic-leaning (<50%% fossil) for %d of the %d tested endmembers; it crosses 50%% only below beta_source = %.4f (see break-even below).",
                sum(med < 0.5), length(med),
                tryCatch(stats::uniroot(function(b) stats::median(pmax(0, pmin(1, slope/b))) - 0.5,
                                        c(0.005, 1))$root, error = function(e) NA_real_)))
print(out, row.names = FALSE)
# Break-even endmember: beta at which the CAMPAIGN MEDIAN fossil fraction reaches
# 50%. Below it the median would read majority-fossil; above it the biogenic-leaning
# median holds. Reported in the manuscript as the single robustness statement that
# covers every published endmember value.
.medgap <- function(b) stats::median(pmax(0, pmin(1, slope / b))) - 0.5
beta_break <- tryCatch(stats::uniroot(.medgap, c(0.005, 1))$root, error = function(e) NA_real_)
write.csv(data.frame(beta_breakeven = beta_break,
                     median_at_breakeven_pct = 50,
                     n_flights = length(slope)),
          file.path(OUT_DIR, "beta_breakeven.csv"), row.names = FALSE)
message(sprintf(
  "BREAK-EVEN endmember: median fossil fraction reaches 50%% at beta_source = %.4f mol/mol. The biogenic-leaning median holds for any endmember above this.",
  beta_break))

# Explicit ARC-endmember case. These are the numbers quoted in the manuscript
# limitation on the ethane endmember, so they are emitted by the pipeline rather
# than computed by hand.
j    <- which.min(abs(betas - SOURCE_C2H6_CH4_ARC))
ffA  <- pmax(0, pmin(1, slope / SOURCE_C2H6_CH4_ARC))
arc  <- data.frame(flight = fl, fossil_pct_adopted = round(100 * pmax(0, pmin(1, slope / beta0))),
                   fossil_pct_ARC = round(100 * ffA))
write.csv(arc, file.path(OUT_DIR, "beta_endmember_ARC.csv"), row.names = FALSE)
message(sprintf(
  "ARC-measured endmember %.4f: median fossil %d%% (adopted %.2f gives %d%%); range %d to %d%%; %d of %d flights majority-fossil (adopted: %d).",
  SOURCE_C2H6_CH4_ARC, round(100 * med[j]), beta0, round(100 * med[which.min(abs(betas - beta0))]),
  round(100 * min(ffA)), round(100 * max(ffA)), sum(ffA >= 0.5), length(ffA),
  nmaj[which.min(abs(betas - beta0))]))

message("\nWrote beta_sensitivity.csv, beta_endmember_ARC.csv and figures/FigS5_beta_sensitivity.png")
