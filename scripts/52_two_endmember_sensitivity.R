# 52_two_endmember_sensitivity.R ----------------------------------------------
# Urban fossil methane is not one gas. Advected DJB methane carries the basin's
# production/midstream ethane ratio (contemporary values 0.063-0.10 mol/mol), while
# leakage from the metropolitan distribution and post-meter system is processed
# gas, which is ethane-depleted (delivered-gas assays in other cities 0.017-0.037;
# config.R SOURCE_C2H6_CH4_PIPELINE_*). The single-endmember mixing model of
# section 3.2 therefore has an EFFECTIVE fossil endmember
#
#     beta_eff = p_dist * beta_dist + (1 - p_dist) * beta_DJB
#
# where p_dist is the share of the urban FOSSIL methane that is distribution gas.
# This script evaluates every flight's within-leg fossil fraction over the joint
# range of (beta_dist, beta_DJB, p_dist), reports where the campaign median crosses
# 50%, and draws the sensitivity as Figure S3b. It REPORTS; nothing published changes.
#
#   Rscript scripts/52_two_endmember_sensitivity.R
#   Out: <OUT_DIR>/two_endmember_sensitivity.csv   (full grid)
#        <OUT_DIR>/two_endmember_breakeven.csv     (p_dist at which the median hits 50%)
#        <OUT_DIR>/two_endmember_flights.csv       (per-flight fractions at named cases)
#        <FIG>/FigS3b_two_endmember.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))

t1 <- read.csv(file.path(OUT_DIR, "table1_full.csv"), stringsAsFactors = FALSE)
sl <- t1$ethane_slope_within; names(sl) <- t1$flight
sl <- sl[is.finite(sl)]
if (length(sl) < 3) stop("table1_full.csv has fewer than 3 finite within-leg slopes")
med_slope <- median(sl)

# Endmember menus ------------------------------------------------------------
# Distribution gas: the lean delivered gas assayed in East Coast cities (0.02-0.045,
# config.R SOURCE_C2H6_CH4_PIPELINE_*) AND the measured Denver delivered gas (PSCo
# Denver zone; psco_gas_quality.csv, config.R SOURCE_C2H6_CH4_DENVER_DELIVERED).
gq <- tryCatch(read.csv(file.path(proj, "psco_gas_quality.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
DENVER_JJ24 <- if (!is.null(gq)) {
  z <- gq[gq$zone == "DENVER" & gq$year == 2024 & gq$month %in% c("JUN", "JUL"), ]
  round(mean(z$c2h6_ch4_mol), 4) } else SOURCE_C2H6_CH4_DENVER_DELIVERED
BETA_DIST <- c(0.020, 0.025, 0.030, 0.037, 0.045, DENVER_JJ24)   # last = Denver, June-July 2024
DJB_2021  <- 0.061                                         # 2021 aircraft flux partition (SI S5; Daley et al.)
BETA_DJB  <- c(DJB_2021, 0.0813, 0.102)                    # contemporary DJB: 2021 flux ratio,
                                                           # 2024 ARC ground ratio, lowest published
P_DIST    <- seq(0, 1, by = 0.05)
ff <- function(beta) pmin(100, 100 * sl / beta)

grid <- expand.grid(beta_dist = BETA_DIST, beta_djb = BETA_DJB, p_dist = P_DIST)
grid$beta_eff <- grid$p_dist * grid$beta_dist + (1 - grid$p_dist) * grid$beta_djb
res <- t(sapply(grid$beta_eff, function(b) {
  f <- ff(b); c(median_fossil_pct = median(f), min_fossil_pct = min(f), max_fossil_pct = max(f),
                n_flights_majority = sum(f > 50))
}))
grid <- cbind(grid, round(res, 1))
grid$median_fossil_pct <- round(grid$median_fossil_pct)
write.csv(grid, file.path(OUT_DIR, "two_endmember_sensitivity.csv"), row.names = FALSE)

# Break-even: median = 50% when beta_eff = 2 * median slope
be <- 2 * med_slope
bk <- expand.grid(beta_dist = BETA_DIST, beta_djb = BETA_DJB)
bk$p_dist_breakeven <- with(bk, ifelse(beta_djb <= be, 0, ifelse(beta_dist >= be, NA, (beta_djb - be) / (beta_djb - beta_dist))))
# NA = the median never reaches 50% for any mixture (both endmembers above the break-even)
bk$p_dist_breakeven <- round(bk$p_dist_breakeven, 2)
bk$beta_eff_breakeven <- round(be, 4)
write.csv(bk, file.path(OUT_DIR, "two_endmember_breakeven.csv"), row.names = FALSE)

# Named cases ------------------------------------------------------------------
INV_P_DIST <- local({                     # inventory share of box fossil methane that is distribution + post-meter
  inv <- tryCatch(read.csv(file.path(OUT_DIR, "inventory_comparison.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(inv)) NA_real_ else {
    fos <- sum(inv$t_hr[inv$group == "fossil"]); dist <- sum(inv$t_hr[grepl("Distribution|PostMeter", inv$sector)])
    dist / fos
  }
})
cases <- data.frame(
  case = c("DJB only, 2021 flux ratio", "DJB only, 2024 ARC ground ratio", "lowest published (adopted)",
           "inventory-weighted, other-cities delivered gas (mean)", "inventory-weighted, other-cities delivered gas (low)",
           "inventory-weighted, other-cities delivered gas (high)",
           "half distribution, other-cities delivered gas", "distribution only, other-cities delivered gas",
           "inventory-weighted, DENVER delivered gas (PSCo Jun-Jul 2024), DJB 2021 flux ratio",
           "inventory-weighted, DENVER delivered gas, DJB 0.0813",
           "half distribution, DENVER delivered gas, DJB 2021 flux ratio",
           "distribution only, DENVER delivered gas (PSCo Jun-Jul 2024)"),
  beta_dist = c(NA, NA, NA, SOURCE_C2H6_CH4_PIPELINE_MEAN, SOURCE_C2H6_CH4_PIPELINE_RANGE[1], SOURCE_C2H6_CH4_PIPELINE_RANGE[2], SOURCE_C2H6_CH4_PIPELINE_MEAN, SOURCE_C2H6_CH4_PIPELINE_MEAN,
                DENVER_JJ24, DENVER_JJ24, DENVER_JJ24, DENVER_JJ24),
  beta_djb  = c(DJB_2021, SOURCE_C2H6_CH4_ARC, SOURCE_C2H6_CH4, DJB_2021, DJB_2021, DJB_2021, DJB_2021, NA, DJB_2021, SOURCE_C2H6_CH4_ARC, DJB_2021, NA),
  p_dist    = c(0, 0, 0, INV_P_DIST, INV_P_DIST, INV_P_DIST, 0.5, 1, INV_P_DIST, INV_P_DIST, 0.5, 1), stringsAsFactors = FALSE)
cases$beta_eff <- with(cases, ifelse(p_dist == 0, beta_djb, ifelse(p_dist == 1, beta_dist, p_dist * beta_dist + (1 - p_dist) * beta_djb)))
fl <- t(sapply(cases$beta_eff, function(b) round(ff(b))))
colnames(fl) <- names(sl)
cases <- cbind(cases, median_fossil_pct = apply(fl, 1, median), n_majority = apply(fl, 1, function(v) sum(v > 50)), fl)
cases$beta_eff <- round(cases$beta_eff, 4); cases$p_dist <- round(cases$p_dist, 2)
write.csv(cases, file.path(OUT_DIR, "two_endmember_flights.csv"), row.names = FALSE)

# Empirical floor on beta_eff from tightly correlated legs --------------------
# In a leg whose ethane and methane enhancements are tightly correlated (r >= 0.7,
# n >= 30), the slope equals beta_eff times the fossil share of that leg's methane,
# so beta_eff cannot be below the slope: the steepest such leg on a flight is a lower
# bound on the effective fossil endmember of the air that flight sampled. Uses the
# per-leg table from script 48 when present.
pl <- tryCatch(read.csv(file.path(OUT_DIR, "perleg_fossil_fraction.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
if (!is.null(pl)) {
  tight <- pl[is.finite(pl$leg_r) & pl$leg_r >= 0.7 & pl$n_gated >= 30, ]
  fl_floor <- aggregate(leg_slope ~ flight, tight, max); names(fl_floor)[2] <- "beta_eff_floor"
  fl_floor$n_tight_legs <- as.vector(table(tight$flight)[fl_floor$flight])
  camp_floor <- max(fl_floor$beta_eff_floor)
  pmax_tab <- expand.grid(beta_dist = c(SOURCE_C2H6_CH4_PIPELINE_RANGE[1], SOURCE_C2H6_CH4_PIPELINE_MEAN, SOURCE_C2H6_CH4_PIPELINE_RANGE[2], DENVER_JJ24),
                          beta_djb = BETA_DJB)
  # share of the fossil methane that can be distribution gas without the leg slope exceeding beta_eff;
  # 1 = unconstrained (a distribution gas at least as ethane-rich as the floor never violates it)
  pmax_tab$p_dist_max <- with(pmax_tab, ifelse(beta_dist >= camp_floor, 1,
                                              round(pmin(1, pmax(0, (beta_djb - camp_floor) / (beta_djb - beta_dist))), 2)))
  pmax_tab$beta_eff_floor <- round(camp_floor, 4)
  write.csv(fl_floor, file.path(OUT_DIR, "two_endmember_floor_flights.csv"), row.names = FALSE)
  write.csv(pmax_tab, file.path(OUT_DIR, "two_endmember_floor_pdist.csv"), row.names = FALSE)
  cat(sprintf("\nempirical floor on beta_eff (steepest leg with r >= 0.7 and n >= 30): %.4f\n", camp_floor))
  print(fl_floor, row.names = FALSE)
  cat("\nlargest distribution-gas share of the fossil methane consistent with that floor:\n"); print(pmax_tab, row.names = FALSE)
}

# Figure S3b -------------------------------------------------------------------
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "FigS3b_two_endmember.png"), 1300, 820, res = 150)
par(mar = c(4.4, 4.4, 1.5, 1))
plot(NA, xlim = c(0, 1), ylim = c(0, 100), xlab = "share of urban fossil methane that is distribution gas, p_dist",
     ylab = "campaign median fossil fraction (%)", las = 1)
abline(h = 50, lty = 3, col = "gray40")
if (is.finite(INV_P_DIST)) { abline(v = INV_P_DIST, lty = 2, col = "gray55")
  text(INV_P_DIST, 3, sprintf("inventory share %.2f", INV_P_DIST), pos = 4, cex = 0.75, col = "gray30") }
cols <- c("#b2182b", "#ef8a62", "#67a9cf", "#2166ac", "#1a9850", "#000000"); lt <- c(1, 2, 4)
for (j in seq_along(BETA_DJB)) for (i in seq_along(BETA_DIST)) {
  g <- grid[grid$beta_dist == BETA_DIST[i] & grid$beta_djb == BETA_DJB[j], ]
  g <- g[order(g$p_dist), ]
  lines(g$p_dist, g$median_fossil_pct, col = cols[i], lty = lt[j], lwd = if (j == 1) 2.2 else 1.4)
}
legend("topleft", bty = "n", cex = 0.78, ncol = 2,
       legend = c(sprintf("beta_dist = %.3f%s", BETA_DIST, c(rep(" (other cities)", 5), " (Denver, PSCo Jun-Jul 2024)")),
                  sprintf("beta_DJB = %.4g", BETA_DJB)),
       col = c(cols, rep("gray40", 3)), lty = c(rep(1, 6), lt), lwd = c(rep(2, 6), 2.2, 1.4, 1.4))
title(main = sprintf("Two-endmember sensitivity (within-leg slopes; median %.4f, break-even beta_eff %.3f)", med_slope, be),
      cex.main = 0.85)
dev.off()

cat(sprintf("Denver delivered gas, PSCo Denver zone, June-July 2024: C2H6:CH4 = %.4f mol/mol (psco_gas_quality.csv)\n", DENVER_JJ24))
cat(sprintf("median within-leg slope %.4f; the campaign median is 50%% fossil at beta_eff = %.4f\n", med_slope, be))
cat("p_dist at which the median reaches 50%:\n"); print(bk, row.names = FALSE)
ec <- expand.grid(beta_dist = SOURCE_C2H6_CH4_PIPELINE_RANGE, beta_djb = BETA_DJB)
ec$p_dist_breakeven <- with(ec, round((beta_djb - be) / (beta_djb - beta_dist), 3))
cat("\nbreak-even p_dist for the measured East Coast delivered-gas range:\n"); print(ec, row.names = FALSE)
write.csv(ec, file.path(OUT_DIR, "two_endmember_breakeven_eastcoast.csv"), row.names = FALSE)
cat("\nnamed cases:\n"); print(cases[, c("case", "beta_dist", "beta_djb", "p_dist", "beta_eff", "median_fossil_pct", "n_majority")], row.names = FALSE)
cat("\nwrote two_endmember_sensitivity.csv, two_endmember_breakeven.csv, two_endmember_flights.csv, FigS3b_two_endmember.png\n")
