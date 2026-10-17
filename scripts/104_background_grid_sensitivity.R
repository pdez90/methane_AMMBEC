# 104_background_grid_sensitivity.R ------------------------------------------
# How much does the BACKGROUND DEFINITION move the urban fossil fraction? (SI S4;
# the Shiny explorer's "Background" tab reads the output.)
#
# The paper's background is the rolling 5th percentile of each species within a
# 300-sample window run over the flight's urban constant-altitude transects joined
# end to end (scripts 15 and 21). Scripts 85 (along-track window) and 100 (air above
# the boundary layer) test two alternative constructions; this script sweeps the two
# parameters of the rolling construction itself and adds a constant background:
#
#   window      60, 120, 180, 300, 450, 600, 900, 1800 samples (1 to 30 min of flight)
#   percentile  1, 2, 5, 10, 25 %
#   constant    the same percentiles of ALL the flight's urban-transect samples
#               (window = Inf; one number per species and flight)
#
# For every (flight, window, percentile) the same 20 ppb gate and within-transect
# York fit as Table 1 give the slope and the fossil fraction at SOURCE_C2H6_CH4, plus
# the CH4:CO correlation and GRA2PES-anchored emission as in script 15. Because each
# transect has its own intercept in the fit, a background that only shifts a whole
# transect cannot change the slope; the background acts through which samples pass
# the gate and through along-transect structure on scales longer than the window.
#
# Out: <OUT_DIR>/background_grid_sensitivity.csv   one row per flight x window x percentile
#      <OUT_DIR>/background_grid_summary.csv       campaign median per window x percentile
#      <OUT_DIR>/figures/FigS_background_grid.png
# Run: Rscript scripts/104_background_grid_sensitivity.R      (base R, ~1 min)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths", "massbalance", "enhancements", "ratios"))
  source(file.path(proj, "R", paste0(f, ".R")))

MW_CH4 <- 16.04; MW_CO <- 28.01; GGYR_TO_THR <- 1e3 / (365.25 * 24)
GATE_PPB <- 20; CO_GATE_PPB <- 5
WINDOWS <- c(60, 120, 180, 300, 450, 600, 900, 1800, Inf)
QS <- c(0.01, 0.02, 0.05, 0.10, 0.25)
T1 <- read.csv(file.path(OUT_DIR, "table1.csv"), stringsAsFactors = FALSE)
FL9 <- T1$Flight[T1$Fossil_pct_CI != "NA"]

# rolling low quantiles, all percentiles in one pass (same window edges as rolling_baseline)
rolling_q <- function(x, window, qs) {
  n <- length(x)
  if (!is.finite(window)) {
    v <- stats::quantile(x, probs = qs, na.rm = TRUE, names = FALSE)
    return(matrix(v, n, length(qs), byrow = TRUE))
  }
  half <- window %/% 2; out <- matrix(NA_real_, n, length(qs))
  for (i in seq_len(n)) {
    lo <- max(1, i - half); hi <- min(n, i + half)
    out[i, ] <- stats::quantile(x[lo:hi], probs = qs, na.rm = TRUE, names = FALSE)
  }
  out
}
fit <- function(z, e_co) {
  k <- is.finite(z$dch4) & is.finite(z$dc2h6) & z$dch4 > GATE_PPB
  s <- if (sum(k) >= 10) york_within_leg(z$dch4[k], z$dc2h6[k], z$leg_id[k])$slope else NA_real_
  kco <- is.finite(z$dch4) & is.finite(z$dco) & z$dco > CO_GATE_PPB
  rco <- if (sum(kco) >= 10) suppressWarnings(stats::cor(z$dco[kco], z$dch4[kco])) else NA_real_
  bco <- if (sum(kco) >= 10) unname(stats::coef(stats::lm(z$dch4[kco] ~ z$dco[kco]))[2]) else NA_real_
  c(n_gated = sum(k), n_legs = length(unique(z$leg_id[k])), slope = s,
    fossil_pct = round(100 * fossil_fraction(s, SOURCE_C2H6_CH4)),
    r_co = round(rco, 2), E_co_t_hr = round(bco * (MW_CH4 / MW_CO) * e_co * GGYR_TO_THR, 2))
}

rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  if (!fl %in% FL9) next
  ic <- read_icartt(p); d <- detect_level_legs(ic$data)
  legs <- tag_region(leg_metrics(d)); urb <- legs$leg_id[legs$region == "urban"]
  du <- d[d$leg_id %in% urb, ]                    # the joined urban transects, as scripts 15 and 21
  eco <- e_co_anchor(ic$meta$date)
  for (w in WINDOWS) {
    bg <- lapply(c("CH4_ppb", "C2H6_ppb", "CO_ppb"), function(v) rolling_q(du[[v]], w, QS))
    for (j in seq_along(QS)) {
      z <- data.frame(leg_id = du$leg_id, dch4 = du$CH4_ppb - bg[[1]][, j],
                      dc2h6 = du$C2H6_ppb - bg[[2]][, j], dco = du$CO_ppb - bg[[3]][, j])
      r <- fit(z, eco)
      rows[[length(rows) + 1]] <- data.frame(flight = fl, window_samples = w, percentile = 100 * QS[j],
        n_urban = nrow(du), n_gated = r[["n_gated"]], n_legs = r[["n_legs"]], slope = signif(r[["slope"]], 4),
        fossil_pct = r[["fossil_pct"]], r_co = r[["r_co"]], E_co_t_hr = r[["E_co_t_hr"]], stringsAsFactors = FALSE)
    }
  }
  pr <- function(w, q) { x <- rows[[length(rows)]]; r <- do.call(rbind, rows); r <- r[r$flight == fl, ]
    r$fossil_pct[r$window_samples == w & r$percentile == q] }
  message(sprintf("%-16s n %5d | fossil %% at 5th pct: w60 %3s w300 %3s (paper) w900 %3s const %3s | at w300: p1 %3s p25 %3s",
                  fl, nrow(du), pr(60, 5), pr(300, 5), pr(900, 5), pr(Inf, 5), pr(300, 1), pr(300, 25)))
}
res <- do.call(rbind, rows)
write.csv(res, file.path(OUT_DIR, "background_grid_sensitivity.csv"), row.names = FALSE)

# the paper's setting must reproduce Table 1
chk <- merge(res[res$window_samples == 300 & res$percentile == 5, c("flight", "fossil_pct")],
             data.frame(flight = T1$Flight, t1 = as.integer(sub(" .*", "", T1$Fossil_pct_CI)), stringsAsFactors = FALSE), by = "flight")
if (!all(chk$fossil_pct == chk$t1)) stop("window 300 / 5th percentile does not reproduce Table 1: ", paste(chk$flight[chk$fossil_pct != chk$t1], collapse = ", "))

agg <- aggregate(cbind(fossil_pct, n_gated) ~ window_samples + percentile, res, function(v) stats::median(v, na.rm = TRUE), na.action = na.pass)
cnt <- aggregate(fossil_pct ~ window_samples + percentile, res, function(v) sum(is.finite(v)), na.action = na.pass)
mx  <- aggregate(fossil_pct ~ window_samples + percentile, res, function(v) if (any(is.finite(v))) max(v, na.rm = TRUE) else NA, na.action = na.pass)
summ <- data.frame(window_samples = agg$window_samples, percentile = agg$percentile, n_flights = cnt$fossil_pct,
                   median_fossil_pct = agg$fossil_pct, max_fossil_pct = mx$fossil_pct, median_n_gated = agg$n_gated)
summ <- summ[order(summ$percentile, summ$window_samples), ]
write.csv(summ, file.path(OUT_DIR, "background_grid_summary.csv"), row.names = FALSE)
message("\ncampaign median fossil % (rows: percentile; columns: window in samples)")
print(xtabs(median_fossil_pct ~ percentile + window_samples, summ))
message(sprintf("range of the campaign median over the grid: %d to %d%%; max single-flight value %d%%",
                min(summ$median_fossil_pct), max(summ$median_fossil_pct), max(summ$max_fossil_pct, na.rm = TRUE)))

## ---- figure -----------------------------------------------------------------------
dir.create(file.path(OUT_DIR, "figures"), showWarnings = FALSE)
png(file.path(OUT_DIR, "figures/FigS_background_grid.png"), width = 1900, height = 850, res = 150)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
wl <- ifelse(is.finite(WINDOWS), as.character(WINDOWS), "const")
M <- xtabs(median_fossil_pct ~ percentile + window_samples, summ)
image(seq_along(WINDOWS), seq_along(QS), t(M), col = hcl.colors(20, "YlOrRd", rev = TRUE), zlim = c(0, 50),
      xaxt = "n", yaxt = "n", xlab = "Background window (samples; const = whole flight)", ylab = "Background percentile (%)",
      main = "Campaign median fossil fraction (%)")
axis(1, seq_along(WINDOWS), wl); axis(2, seq_along(QS), 100 * QS)
for (i in seq_along(WINDOWS)) for (j in seq_along(QS)) text(i, j, M[j, i], cex = 0.95)
rect(which(WINDOWS == 300) - 0.5, which(QS == 0.05) - 0.5, which(WINDOWS == 300) + 0.5, which(QS == 0.05) + 0.5, border = "black", lwd = 2.5)
r5 <- res[res$percentile == 5, ]
fls <- unique(r5$flight); cols <- hcl.colors(length(fls), "Dark 3")
plot(NA, xlim = c(1, length(WINDOWS)), ylim = c(0, 100), xaxt = "n", xlab = "Background window (samples; 5th percentile)",
     ylab = "Fossil fraction (%)", main = "Per flight, 5th-percentile background")
axis(1, seq_along(WINDOWS), wl); abline(h = 50, lty = 3); abline(v = which(WINDOWS == 300), col = "grey60", lty = 2)
for (i in seq_along(fls)) { s <- r5[r5$flight == fls[i], ]; s <- s[match(WINDOWS, s$window_samples), ]
  lines(seq_along(WINDOWS), s$fossil_pct, col = cols[i], lwd = 1.6); points(seq_along(WINDOWS), s$fossil_pct, col = cols[i], pch = 16, cex = 0.8) }
med <- summ$median_fossil_pct[summ$percentile == 5][match(WINDOWS, summ$window_samples[summ$percentile == 5])]
lines(seq_along(WINDOWS), med, lwd = 3.5, col = "black")
lab <- function(f) { f <- sub("_R0", "", f); paste(as.integer(substr(f, 7, 8)), "Jul", sub("^[0-9]+_?", "", f)) }
legend("topleft", c(lab(fls), "campaign median"), col = c(cols, "black"), lwd = c(rep(1.6, length(fls)), 3.5), cex = 0.75, bty = "n", ncol = 3)
dev.off()
message("Wrote background_grid_sensitivity.csv, background_grid_summary.csv and figures/FigS_background_grid.png")
