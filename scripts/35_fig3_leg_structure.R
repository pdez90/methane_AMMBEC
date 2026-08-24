# 35_fig3_leg_structure.R ------------------------------------------------------
# Is the ethane-methane scatter on the low-correlation flights ONE population
# with noise, or TWO? On the biogenic example flight (Figure 3, left panel) the
# gated scatter shows a dense uncorrelated band plus an elevated-ethane cluster
# that could be read as a second, positively correlated population. Whether that
# matters depends on HOW the populations differ:
#   * separated in INTERCEPT (an air mass with higher background ethane, e.g.
#     advected basin air): adds scatter and lowers r, but does not create a
#     co-emission slope, so the fossil fraction is not biased;
#   * separated in SLOPE (some legs sampling genuinely ethane-rich plumes): a
#     real mixture, which the whole-flight fit would average - exactly the risk
#     the leg-block bootstrap prices (its upper bound on this flight is 35%).
# This script settles which, by fitting each urban leg separately on the two
# Figure 3 flights plus the other near-zero flight (2024-07-08 L1).
#
# Out: <OUT_DIR>/fig3_leg_structure.csv,
#      <OUT_DIR>/figures/diag_fig3_leg_structure.png
# Run: Rscript scripts/35_fig3_leg_structure.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R"))

FLIGHTS <- c("20240710_R0_L1", "20240713_R0_L2", "20240708_R0_L1")
MIN_ENH <- 20; MIN_PTS_LEG <- 10

ols <- function(x, y) { k <- is.finite(x) & is.finite(y)
  if (sum(k) < MIN_PTS_LEG) return(c(NA_real_, NA_real_))
  c(unname(stats::coef(stats::lm(y[k] ~ x[k]))[2]), suppressWarnings(stats::cor(x[k], y[k]))) }

rows <- list(); panels <- list()
for (key in FLIGHTS) {
  p <- { f <- list_flights(DATA_DIR); f[grepl("ARL-Suite", f) & grepl(key, f)][1] }
  if (is.na(p)) { message("missing flight ", key); next }
  d <- detect_level_legs(read_icartt(p)$data)
  lg <- tag_region(leg_metrics(d))
  du <- d[d$leg_id %in% lg$leg_id[lg$region == "urban"], ]
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  k  <- is.finite(du$CH4_ppb_enh) & is.finite(du$C2H6_ppb_enh) & du$CH4_ppb_enh > MIN_ENH
  g  <- du[k, ]
  panels[[key]] <- g
  # whole-flight fit (should reproduce the Table 1 machinery)
  wf <- york_slope(g$CH4_ppb_enh, g$C2H6_ppb_enh, 1, 0.2)
  for (id in sort(unique(g$leg_id))) {
    s <- g[g$leg_id == id, ]
    fit <- ols(s$CH4_ppb_enh, s$C2H6_ppb_enh)
    yk  <- if (nrow(s) >= MIN_PTS_LEG)
             york_slope(s$CH4_ppb_enh, s$C2H6_ppb_enh, 1, 0.2)$slope else NA_real_
    rows[[paste(key, id)]] <- data.frame(
      flight = key, leg_id = id, n_gated = nrow(s),
      agl_m = round(mean(s$ALTAGL, na.rm = TRUE)),
      dC2H6_median_ppb = round(stats::median(s$C2H6_ppb_enh, na.rm = TRUE), 2),
      ols_slope = round(fit[1], 4), york_slope = round(yk, 4), r = round(fit[2], 2),
      fossil_pct_if_alone = round(100 * pmax(0, pmin(1, yk / SOURCE_C2H6_CH4))),
      wholeflight_york = round(wf$slope, 4), stringsAsFactors = FALSE)
  }
}
res <- do.call(rbind, rows); rownames(res) <- NULL
write.csv(res, file.path(OUT_DIR, "fig3_leg_structure.csv"), row.names = FALSE)
print(res, row.names = FALSE)

## ---- diagnostic figure: gated scatter colored by leg, per-leg OLS lines ----
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "diag_fig3_leg_structure.png"), width = 2100, height = 720, res = 150)
par(mfrow = c(1, length(panels)), mar = c(4.2, 4.2, 3, 1))
for (key in names(panels)) {
  g <- panels[[key]]
  ids <- sort(unique(g$leg_id)); cols <- grDevices::hcl.colors(length(ids), "Dark 3")
  plot(g$CH4_ppb_enh, g$C2H6_ppb_enh, pch = 20, cex = 0.55,
       col = cols[match(g$leg_id, ids)],
       xlab = "dCH4 (ppb)", ylab = "dC2H6 (ppb)",
       main = sprintf("%s - gated urban samples by leg", key), cex.main = 0.95)
  for (j in seq_along(ids)) {
    s <- g[g$leg_id == ids[j], ]
    if (nrow(s) < MIN_PTS_LEG) next
    cf <- stats::coef(stats::lm(C2H6_ppb_enh ~ CH4_ppb_enh, data = s))
    xr2 <- range(s$CH4_ppb_enh, na.rm = TRUE)
    lines(xr2, cf[1] + cf[2] * xr2, col = cols[j], lwd = 2)
  }
  legend("topright", bty = "n", cex = 0.62, ncol = 2, pch = 20, col = cols,
         legend = sprintf("leg %d (%d m)", ids,
                          round(vapply(ids, function(i) mean(g$ALTAGL[g$leg_id == i], na.rm = TRUE), 1))))
}
dev.off()
message("\nWrote fig3_leg_structure.csv and figures/diag_fig3_leg_structure.png")
message("Read the per-leg rows: an elevated dC2H6_median with a NEAR-ZERO ols_slope means an
ethane OFFSET (different background air), which lowers r but does not bias the slope;
a leg with a clearly positive ols_slope AND r would be a genuine second population,
which is what the leg-block bootstrap's upper bound already prices.")
