# 27_beta_sensitivity.R -------------------------------------------------------
# Sensitivity of the fossil fraction to the assumed ethane endmember beta_source.
# The absolute fossil fractions scale as 1/beta_source (Eq. 5), so a reviewer will
# ask how the biogenic-leaning result depends on the adopted value (0.11). This
# recomputes every urban flight's fossil fraction across a plausible range of
# beta_source and shows that the campaign median stays biogenic (< 50% fossil)
# throughout, which is the point that defuses the endmember concern.
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

beta0 <- SOURCE_C2H6_CH4                       # adopted endmember (0.11)
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

# plausible/illustrative endmember range (spans regional source-gas values and the
# range of observed atmospheric enhancement ratios; NOT inferred as a hard bound)
betas <- sort(unique(c(seq(0.08, 0.15, by = 0.005), SOURCE_C2H6_CH4_ARC)))
FF <- sapply(betas, function(b) pmax(0, pmin(1, slope / b)))   # rows = flights, cols = betas
med <- apply(FF, 2, stats::median)
lo  <- apply(FF, 2, min); hi <- apply(FF, 2, max)
nmaj <- apply(FF, 2, function(x) sum(x >= 0.5))       # flights that would read majority-fossil

lab <- rep("", length(betas))
lab[abs(betas - beta0) < 1e-9]              <- "adopted (raw DJB/Wattenberg gas)"
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
text(SOURCE_C2H6_CH4_ARC, 88, sprintf("ARC measured %.4f", SOURCE_C2H6_CH4_ARC),
     col = "#1b7837", cex = 0.7, pos = 4)
text(mean(range(betas)), 53, "majority fossil", col = "#c0392b", cex = 0.8, pos = 3)
text(beta0, 96, sprintf("adopted %.2f", beta0), col = "#444444", cex = 0.75, pos = 4)
legend("topright", bty = "n", cex = 0.8, lwd = c(3, 1), col = c("#1f3864", "#9db8d2"),
       legend = c("campaign median", "individual flights"))
dev.off()

message(sprintf("Adopted beta_source = %.2f. Median fossil fraction across beta = 0.08 to 0.15: %d%% to %d%%.",
                beta0, round(100*min(med)), round(100*max(med))))
message(sprintf("Campaign median stays below 50%% (biogenic-leaning) for every tested endmember: %s.",
                if (all(med < 0.5)) "TRUE" else "FALSE"))
print(out, row.names = FALSE)
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
