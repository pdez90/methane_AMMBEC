# 29_regression_comparison.R --------------------------------------------------
# Side-by-side comparison of three enhancement-ratio estimators for every urban
# flight, so the choice of estimator is made on evidence:
#   OLS  (ordinary least squares, y on x; attenuated when x is noisy)
#   RMA  (reduced major axis / geometric mean; stable, symmetric)
#   York (errors-in-variables with instrument precisions; correct in theory but
#         becomes unstable on weakly correlated atmospheric data, because plume-to-
#         plume variability is far larger than the instrument precision it assumes)
#
# It reports, per flight: the ethane:methane slope and implied fossil fraction
# under each method, and the CH4:CO slope (the emission driver) under each,
# together with the Pearson correlation so weak fits are visible.
#
# Out: <OUT_DIR>/regression_comparison.csv
#      <OUT_DIR>/figures/FigS7_regression_comparison.png
# Run: Rscript scripts/29_regression_comparison.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "massbalance.R"))

ols_slope <- function(x, y) { k <- is.finite(x) & is.finite(y); if (sum(k) < 10) return(NA)
  unname(stats::coef(stats::lm(y[k] ~ x[k]))[2]) }
ff <- function(s) if (is.finite(s)) max(0, min(1, s / SOURCE_C2H6_CH4)) else NA

rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL); if (is.null(ic)) next
  if (!all(c("CH4_ppb","C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  urb <- tag_region(leg_metrics(d)); urb <- urb$leg_id[urb$region == "urban"]
  du <- d[d$leg_id %in% urb, ]; if (nrow(du) < 50) next
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub(".ict", "", basename(p)))

  # ethane:methane (fossil-fraction driver): y = C2H6, x = CH4
  x <- du$CH4_ppb_enh; y <- du$C2H6_ppb_enh; k <- is.finite(x) & is.finite(y) & x > 20
  r_e <- if (sum(k) > 10) suppressWarnings(stats::cor(x[k], y[k])) else NA
  s_ols <- ols_slope(x[k], y[k]); s_rma <- rma_slope(x[k], y[k])$slope; s_yk <- york_slope(x[k], y[k], 1, 0.2)$slope
  s_yw <- york_within_leg(x[k], y[k], du$leg_id[k], 1, 0.2)$slope   # fixed effects (Table 1 default)

  row <- data.frame(flight = fl, n = sum(k), r_ethane = round(r_e, 2),
    ff_ols = round(100*ff(s_ols)), ff_rma = round(100*ff(s_rma)), ff_york = round(100*ff(s_yk)),
    ff_york_within = round(100*ff(s_yw)),
    stringsAsFactors = FALSE)

  # CH4:CO (emission driver): y = CH4, x = CO
  if ("CO_ppb" %in% names(du)) {
    du <- add_enhancements(du, "CO_ppb")
    # SAME selection as the primary analysis (script 15): CO enhancement > 5 ppb,
    # so the OLS/RMA/York comparison is directly comparable to the reported slopes.
    xc <- du$CO_ppb_enh; yc <- du$CH4_ppb_enh; kc <- is.finite(xc) & is.finite(yc) & xc > 5
    row$r_co       <- if (sum(kc) > 10) round(suppressWarnings(stats::cor(xc[kc], yc[kc])), 2) else NA
    row$co_ols     <- round(ols_slope(xc[kc], yc[kc]), 3)
    row$co_rma     <- round(rma_slope(xc[kc], yc[kc])$slope, 3)
    row$co_york    <- round(york_slope(xc[kc], yc[kc], 5, 1)$slope, 3)
  }
  rows[[fl]] <- row
}
cmp <- do.call(rbind, rows)
cmp <- cmp[order(cmp$flight), ]
write.csv(cmp, file.path(OUT_DIR, "regression_comparison.csv"), row.names = FALSE)

FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "FigS7_regression_comparison.png"), width = 1500, height = 900, res = 140)
# Deeper bottom margin: the axis labels now carry the Pearson r for each flight.
par(mfrow = c(1, 2), mar = c(11, 4.2, 3, 1))
cols <- c(OLS = "#7f8c8d", RMA = "#2e86c1", York = "#c0392b", `York within-leg` = "#1b7f5a")
# Panel A: fossil fraction. All four are pooled fits except the last, which centres
# each leg first (the Table 1 estimator since 12 Sep 2026).
M <- t(as.matrix(cmp[, c("ff_ols","ff_rma","ff_york","ff_york_within")]))
# r is shown per flight on the axis. It used to be one run-on mtext line under
# the panel, which was wider than the panel and got clipped at both ends.
nm_ff <- ifelse(is.na(cmp$r_ethane), cmp$flight,
                sprintf("%s  (r=%.2f)", cmp$flight, cmp$r_ethane))
bp <- barplot(M, beside = TRUE, col = cols, names.arg = nm_ff, las = 2,
              ylab = "Fossil fraction (%)", main = "Ethane:methane fossil fraction by estimator",
              ylim = c(0, 100), cex.names = 0.55)
abline(h = 50, lty = 2, col = "#888888"); legend("topright", fill = cols, legend = names(cols), bty = "n", cex = 0.8)
# Panel B: CH4:CO slope (emission driver), if present
if ("co_york" %in% names(cmp)) {
  Mc <- t(as.matrix(cmp[, c("co_ols","co_rma","co_york")]))
  ymax <- max(2, quantile(unlist(cmp[,c("co_ols","co_rma","co_york")]), 0.9, na.rm=TRUE))
  nm_co <- ifelse(is.na(cmp$r_co), cmp$flight, sprintf("%s  (r=%.2f)", cmp$flight, cmp$r_co))
  # Three estimators here (no within-leg CH4:CO slope), so pass exactly three colours:
  # a four-colour vector would be recycled bar by bar and mis-colour every fourth bar.
  bp2 <- barplot(pmin(Mc, ymax), beside = TRUE, col = cols[1:3], names.arg = nm_co, las = 2,
                 ylab = "CH4:CO slope (mol/mol)", main = "CH4:CO slope by estimator (capped for display)",
                 cex.names = 0.55)
  legend("topright", fill = cols[1:3], legend = names(cols)[1:3], bty = "n", cex = 0.8)
  text(colMeans(bp2), pmin(Mc[3,], ymax), ifelse(Mc[3,] > ymax, sprintf("%.1f", Mc[3,]), ""), pos = 3, cex = 0.5, col = "#c0392b")
}
dev.off()
message("Wrote regression_comparison.csv and figures/FigS7_regression_comparison.png")
print(cmp, row.names = FALSE)
