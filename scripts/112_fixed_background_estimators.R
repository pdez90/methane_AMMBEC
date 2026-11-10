# 112_fixed_background_estimators.R -------------------------------------------
# Does the within-transect fit discard information that sits in the intercept?
# (S. Baidar, comment on section 3.2, Nov 2026.)
#
# The paper's fossil fraction is the slope of dC2H6 on dCH4 with a separate
# intercept for every urban transect. A methane enhancement that is uniform over a
# whole transect (a city-wide dome, or air advected into the city) goes into that
# intercept and does not inform the slope. This script computes, for each Table 1
# flight, estimators that keep that information, with ONE FIXED background per
# flight and species instead of the rolling one:
#
#   pooled   York slope through all gated urban-transect samples, one intercept
#   ratio    sum(dC2H6) / sum(dCH4) over the same samples (zero intercept: the
#            whole enhancement above the background is attributed)
#
# for three fixed backgrounds: the 1st percentile of the whole flight record
# ("p01flight"), the 5th percentile of the flight's urban-transect samples
# ("p05urban") and the median of the air above the boundary layer ("aloft", from
# script 100). The gate is dCH4 > 20 ppb above the same background. The ratio is
# also computed over the flight's BASIN transects with the same background: if it
# takes the same value there, it describes the regional air mass and not Denver.
#
# Upwind test: for in-box transect samples below 1 km above ground, the methane and
# ethane of the upwind 15 % (projected on the mean wind) are compared with the mean of
# the downwind half; a negative difference means the air entering the box already
# carried more methane than the air leaving it, so no urban increment can be formed.
#
# Out: <OUT_DIR>/fixed_background_estimators.csv, fixed_background_estimators_table.csv (SI table)
# Run: Rscript scripts/112_fixed_background_estimators.R     (after script 100; base R)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths", "massbalance", "enhancements", "ratios"))
  source(file.path(proj, "R", paste0(f, ".R")))
GATE_PPB <- 20; MIN_N <- 30
T1 <- read.csv(file.path(OUT_DIR, "table1.csv"), stringsAsFactors = FALSE)
FL9 <- T1$Flight[T1$Fossil_pct_CI != "NA"]
AL <- read.csv(file.path(OUT_DIR, "background_aloft_sensitivity.csv"), stringsAsFactors = FALSE)
ff <- function(s) round(100 * fossil_fraction(s, SOURCE_C2H6_CH4))
b <- URBAN_BOX

rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  if (!fl %in% FL9) next
  ic <- read_icartt(p); d <- detect_level_legs(ic$data); legs <- tag_region(leg_metrics(d))
  du <- d[d$leg_id %in% legs$leg_id[legs$region == "urban"], ]
  db <- d[d$leg_id %in% legs$leg_id[legs$region == "basin"], ]
  a <- du; for (v in c("CH4_ppb", "C2H6_ppb")) a <- add_enhancements(a, v)
  k <- is.finite(a$CH4_ppb_enh) & is.finite(a$C2H6_ppb_enh) & a$CH4_ppb_enh > GATE_PPB
  out <- data.frame(flight = fl,
    slope_within_rolling = signif(york_within_leg(a$CH4_ppb_enh[k], a$C2H6_ppb_enh[k], a$leg_id[k])$slope, 4),
    slope_pooled_rolling = signif(york_slope(a$CH4_ppb_enh[k], a$C2H6_ppb_enh[k], 1, 0.2)$slope, 4), stringsAsFactors = FALSE)
  out$fossil_within_rolling <- ff(out$slope_within_rolling); out$fossil_pooled_rolling <- ff(out$slope_pooled_rolling)
  bgs <- list(p01flight = c(stats::quantile(d$CH4_ppb, 0.01, na.rm = TRUE, names = FALSE), stats::quantile(d$C2H6_ppb, 0.01, na.rm = TRUE, names = FALSE)),
              p05urban  = c(stats::quantile(du$CH4_ppb, 0.05, na.rm = TRUE, names = FALSE), stats::quantile(du$C2H6_ppb, 0.05, na.rm = TRUE, names = FALSE)),
              aloft     = c(AL$bg_ch4_aloft[AL$flight == fl], AL$bg_c2h6_aloft[AL$flight == fl]))
  for (nm in names(bgs)) {
    g <- bgs[[nm]]; col <- function(x) paste0(x, "_", nm)
    if (length(g) < 2 || any(!is.finite(g))) { for (x in c("bg_ch4", "bg_c2h6", "n_gated", "slope_pooled", "ratio", "fossil_pooled", "fossil_ratio", "n_gated_basin", "ratio_basin", "fossil_ratio_basin")) out[[col(x)]] <- NA; next }
    x <- du$CH4_ppb - g[1]; y <- du$C2H6_ppb - g[2]; k <- is.finite(x) & is.finite(y) & x > GATE_PPB
    xb <- db$CH4_ppb - g[1]; yb <- db$C2H6_ppb - g[2]; kb <- is.finite(xb) & is.finite(yb) & xb > GATE_PPB
    out[[col("bg_ch4")]] <- round(g[1], 1); out[[col("bg_c2h6")]] <- round(g[2], 2); out[[col("n_gated")]] <- sum(k)
    out[[col("slope_pooled")]] <- signif(york_slope(x[k], y[k], 1, 0.2)$slope, 4); out[[col("ratio")]] <- signif(sum(y[k]) / sum(x[k]), 4)
    out[[col("fossil_pooled")]] <- ff(out[[col("slope_pooled")]]); out[[col("fossil_ratio")]] <- ff(out[[col("ratio")]])
    out[[col("n_gated_basin")]] <- sum(kb); out[[col("ratio_basin")]] <- if (sum(kb) >= MIN_N) signif(sum(yb[kb]) / sum(xb[kb]), 4) else NA
    out[[col("fossil_ratio_basin")]] <- ff(out[[col("ratio_basin")]])
  }
  # upwind edge against the downwind half, in-box transect samples below 1 km above ground
  s <- d[d$leg_id > 0 & is.finite(d$ALTAGL) & d$ALTAGL < 1000 & d$Latitude >= b$lat_s & d$Latitude <= b$lat_n & d$Longitude >= b$lon_w & d$Longitude <= b$lon_e &
           is.finite(d$CH4_ppb) & is.finite(d$C2H6_ppb) & is.finite(d$WS) & is.finite(d$WD), ]
  u <- mean(-s$WS * sin(s$WD * pi / 180)); v <- mean(-s$WS * cos(s$WD * pi / 180)); spd <- sqrt(u^2 + v^2)
  xk <- (s$Longitude - (b$lon_w + b$lon_e) / 2) * 111.32 * cos((b$lat_s + b$lat_n) / 2 * pi / 180); yk <- (s$Latitude - (b$lat_s + b$lat_n) / 2) * 111.32
  al <- (xk * u + yk * v) / spd; q <- stats::quantile(al, c(0.15, 0.5)); up <- al <= q[1]; dn <- al >= q[2]
  out$wind_from_deg <- round((atan2(-u, -v) * 180 / pi) %% 360); out$wind_vector_ms <- round(spd, 1)
  out$ch4_upwind_ppb <- round(stats::median(s$CH4_ppb[up]), 1); out$c2h6_upwind_ppb <- round(stats::median(s$C2H6_ppb[up]), 2)
  out$dch4_downwind_minus_upwind <- round(mean(s$CH4_ppb[dn]) - stats::median(s$CH4_ppb[up]), 1)
  out$dc2h6_downwind_minus_upwind <- round(mean(s$C2H6_ppb[dn]) - stats::median(s$C2H6_ppb[up]), 2)
  rows[[fl]] <- out
  message(sprintf("%-16s within %2d%% pooled %2d%% | fixed bg, pooled / ratio (basin ratio): flight p01 %2s / %2s (%2s)  urban p05 %2s / %2s (%2s)  aloft %2s / %2s (%2s) | downwind - upwind CH4 %+6.1f ppb",
    fl, out$fossil_within_rolling, out$fossil_pooled_rolling, out$fossil_pooled_p01flight, out$fossil_ratio_p01flight, out$fossil_ratio_basin_p01flight,
    out$fossil_pooled_p05urban, out$fossil_ratio_p05urban, out$fossil_ratio_basin_p05urban, out$fossil_pooled_aloft, out$fossil_ratio_aloft, out$fossil_ratio_basin_aloft, out$dch4_downwind_minus_upwind))
}
R <- do.call(rbind, rows)
write.csv(R, file.path(OUT_DIR, "fixed_background_estimators.csv"), row.names = FALSE)
# the same results laid out for the SI table (fossil %, at the adopted endmember)
lab <- function(f) { d <- sub("_R0.*", "", f); paste0(substr(d, 1, 4), "-", substr(d, 5, 6), "-", substr(d, 7, 8), ifelse(grepl("_L[12]$", f), paste0(" (", sub(".*_", "", f), ")"), "")) }
tri <- function(x) paste(ifelse(is.na(R[[paste0(x, "_p01flight")]]), "-", R[[paste0(x, "_p01flight")]]), ifelse(is.na(R[[paste0(x, "_p05urban")]]), "-", R[[paste0(x, "_p05urban")]]),
                         ifelse(is.na(R[[paste0(x, "_aloft")]]), "-", R[[paste0(x, "_aloft")]]), sep = " / ")
TB <- data.frame(Flight = lab(R$flight), within = R$fossil_within_rolling, pooled = tri("fossil_pooled"), ratio = tri("fossil_ratio"), ratio_basin = tri("fossil_ratio_basin"),
                 wind = sprintf("%d\u00b0, %.1f m/s", R$wind_from_deg, R$wind_vector_ms), dch4 = sprintf("%+.1f", R$dch4_downwind_minus_upwind), stringsAsFactors = FALSE)
write.csv(TB, file.path(OUT_DIR, "fixed_background_estimators_table.csv"), row.names = FALSE)
md <- function(x) stats::median(x, na.rm = TRUE)
message(sprintf("\ncampaign medians (fossil %% at %.3f): within-transect, rolling background %.0f (paper); pooled, rolling %.0f", SOURCE_C2H6_CH4, md(R$fossil_within_rolling), md(R$fossil_pooled_rolling)))
for (nm in c("p01flight", "p05urban", "aloft"))
  message(sprintf("  fixed background %-9s: pooled York %.1f | zero-intercept ratio %.1f (flights at or above 50%%: %d of %d) | same ratio over the basin %.1f", nm,
                  md(R[[paste0("fossil_pooled_", nm)]]), md(R[[paste0("fossil_ratio_", nm)]]), sum(R[[paste0("fossil_ratio_", nm)]] >= 50, na.rm = TRUE),
                  sum(is.finite(R[[paste0("fossil_ratio_", nm)]])), md(R[[paste0("fossil_ratio_basin_", nm)]])))
message(sprintf("upwind test: on %d of %d flights the downwind half of the box carried LESS methane than its upwind edge", sum(R$dch4_downwind_minus_upwind < 0), nrow(R)))
message("Wrote fixed_background_estimators.csv")
