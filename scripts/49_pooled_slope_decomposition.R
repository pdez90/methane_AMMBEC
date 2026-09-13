# 49_pooled_slope_decomposition.R ---------------------------------------------
# WHY does the pooled ethane:methane slope reverse sign on a flight whose every
# leg has a positive slope?  Script 48 showed that it happens (20240710_R0_L1:
# legs +0.0346, +0.0005, +0.0228; pooled -0.0129) and tested one mechanism --
# an ethane background that rises with altitude -- which FAILED: across the 25
# urban legs r(AGL, dC2H6) = -0.23, the wrong sign.  So the reversal is real
# and unexplained.  This script explains it by arithmetic rather than by story.
#
#   Rscript scripts/49_pooled_slope_decomposition.R
#
# THE IDENTITY.  For an ordinary least-squares slope over points that belong
# to legs, the sums of squares split exactly into a within-leg and a
# between-leg part:
#
#     Sxx = Sxx_within + Sxx_between        Sxy = Sxy_within + Sxy_between
#     b_pooled = (Sxx_w * b_within + Sxx_b * b_between) / (Sxx_w + Sxx_b)
#
# b_within  is the slope fitted with a separate intercept per leg (what the
#           per-leg estimators in script 48 approximate: the SOURCE ratio, if
#           each leg samples one air mass);
# b_between is the slope through the LEG MEANS, weighted by points per leg
#           (does the leg with more methane also have more ethane?).
# The pooled slope is a variance-weighted blend of the two, so it can carry
# the sign of b_between whenever the between-leg spread of methane is large
# and the legs disagree about ethane.  That is a Simpson's-paradox reversal,
# and this script prints the weights so it can be seen, not asserted.
#
# It also prints, per leg, WHERE the methane was (the dCH4-weighted centroid
# and the single highest point, with distances to the landmarks the Carbon
# Mapper comparison already uses), so the leg that drives the reversal can be
# placed on a map.  The landmark coordinates are approximate (~1 km) and are
# for orientation only; confirm against a map before naming a facility.
#
# The pooled fit in script 21 is York, not OLS; both are printed.  With
# sx = 1, sy = 0.2 and slopes of a few hundredths the York weights are nearly
# constant, so the OLS decomposition describes the York fit to within the
# difference shown on the "pooled York vs OLS" line.
#
# This script REPORTS.  Nothing published is changed.
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "qc.R"))

MIN_ENH     <- 20      # as scripts 21/35/48
MIN_PTS_LEG <- 10
MIN_PTS_FLT <- 50
BETA        <- SOURCE_C2H6_CH4

# Landmarks named in CARBONMAPPER_COMPARISON.md. Approximate; orientation only.
LANDMARKS <- data.frame(
  name = c("Tower Rd landfill", "DADS landfill", "Cherokee site", "Suncor refinery",
           "Metro Water Recovery (Hite)"),
  lat  = c(39.852, 39.662, 39.807, 39.805, 39.812),
  lon  = c(-104.758, -104.690, -104.964, -104.940, -104.958),
  stringsAsFactors = FALSE)
hav_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180; R <- 6371.0088
  a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}
nearest_landmark <- function(lat, lon) {
  d <- hav_km(lat, lon, LANDMARKS$lat, LANDMARKS$lon)
  i <- which.min(d); sprintf("%s %.1f km", LANDMARKS$name[i], d[i])
}
# Bearing (deg, clockwise from north) FROM the nearest landmark TO a point. A point is
# downwind of that landmark when this bearing is close to the wind's TO-direction,
# i.e. WD + 180 (WD is the direction the wind blows FROM).
bearing_from_nearest <- function(lat, lon) {
  d <- hav_km(lat, lon, LANDMARKS$lat, LANDMARKS$lon); i <- which.min(d)
  p <- pi / 180
  dlon <- (lon - LANDMARKS$lon[i]) * p
  y <- sin(dlon) * cos(lat * p)
  x <- cos(LANDMARKS$lat[i] * p) * sin(lat * p) - sin(LANDMARKS$lat[i] * p) * cos(lat * p) * cos(dlon)
  (atan2(y, x) / p + 360) %% 360
}
ang_diff <- function(a, b) { d <- abs((a - b + 180) %% 360 - 180); d }

.flights <- list_flights(DATA_DIR)
if (!length(.flights))
  stop("no flight files under METHANE_DATA_DIR = ", DATA_DIR,
       "\n  Set the environment first:  eval \"$(bash run_local.sh --print-env)\"")

dec_rows <- list(); leg_rows <- list()
for (p in .flights) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb", "C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data)
  if (!any(d$leg_id > 0)) next
  lg  <- tag_region(leg_metrics(d))
  urb <- lg$leg_id[lg$region == "urban"]
  du  <- d[d$leg_id %in% urb, ]
  if (nrow(du) < MIN_PTS_FLT) next
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))

  x <- du$CH4_ppb_enh; y <- du$C2H6_ppb_enh
  k <- is.finite(x) & is.finite(y) & x > MIN_ENH
  if (sum(k) < MIN_PTS_LEG) next
  g <- du[k, ]; gx <- x[k]; gy <- y[k]
  # keep only legs that script 48 would fit, so the decomposition matches its rows
  keep <- names(which(table(g$leg_id) >= MIN_PTS_LEG))
  g <- g[g$leg_id %in% keep, ]; gx <- g$CH4_ppb_enh; gy <- g$C2H6_ppb_enh
  if (nrow(g) < MIN_PTS_LEG || length(unique(g$leg_id)) < 1) next

  pooled_york <- york_slope(gx, gy, 1, 0.2)$slope
  pooled_ols  <- stats::cov(gx, gy) / stats::var(gx)

  # ---- exact within / between split of the OLS sums ----
  xm <- mean(gx); ym <- mean(gy)
  lm_x <- ave(gx, g$leg_id); lm_y <- ave(gy, g$leg_id)        # leg means, per point
  Sxx_w <- sum((gx - lm_x)^2);        Sxy_w <- sum((gx - lm_x) * (gy - lm_y))
  Sxx_b <- sum((lm_x - xm)^2);        Sxy_b <- sum((lm_x - xm) * (lm_y - ym))
  b_w <- if (Sxx_w > 0) Sxy_w / Sxx_w else NA_real_
  b_b <- if (Sxx_b > 0) Sxy_b / Sxx_b else NA_real_
  w_b <- Sxx_b / (Sxx_w + Sxx_b)                              # weight on the between slope
  stopifnot(abs((Sxx_w * b_w + ifelse(is.finite(b_b), Sxx_b * b_b, 0)) /
                (Sxx_w + Sxx_b) - pooled_ols) < 1e-9)          # the identity holds
  dec_rows[[length(dec_rows) + 1]] <- data.frame(
    flight = fl, n_gated = nrow(g), n_legs = length(unique(g$leg_id)),
    pooled_york = round(pooled_york, 5), pooled_ols = round(pooled_ols, 5),
    within_slope = round(b_w, 5), between_slope = round(b_b, 5),
    between_weight = round(w_b, 3),
    ff_pooled_pct  = round(100 * pooled_york / BETA, 1),
    ff_within_pct  = round(100 * b_w / BETA, 1),
    sign_reversal  = is.finite(b_w) && b_w > 0 && pooled_york < 0,
    stringsAsFactors = FALSE)

  # ---- per-leg: where was the methane? ----
  for (L in sort(unique(g$leg_id))) {
    s <- g[g$leg_id == L, ]
    wgt <- pmax(s$CH4_ppb_enh, 0); wgt <- wgt / sum(wgt)
    clat <- sum(wgt * s$Latitude); clon <- sum(wgt * s$Longitude)
    im <- which.max(s$CH4_ppb_enh)
    tt <- range(s$timestamp, na.rm = TRUE)
    # wind at the gated points (vector mean of WD, so 350 and 10 average to 0, not 180)
    wd <- if ("WD" %in% names(s)) {
      u <- -sin(s$WD * pi / 180); v <- -cos(s$WD * pi / 180)
      (atan2(mean(u, na.rm = TRUE), mean(v, na.rm = TRUE)) * 180 / pi + 180) %% 360
    } else NA_real_
    ws <- if ("WS" %in% names(s)) mean(s$WS, na.rm = TRUE) else NA_real_
    brg <- bearing_from_nearest(s$Latitude[im], s$Longitude[im])
    downwind <- if (is.finite(wd)) ang_diff(brg, (wd + 180) %% 360) else NA_real_
    leg_rows[[length(leg_rows) + 1]] <- data.frame(
      flight = fl, leg_id = L, n_gated = nrow(s),
      utc_start = format(tt[1], "%H:%M"), utc_end = format(tt[2], "%H:%M"),
      agl_m = round(mean(s$ALTAGL, na.rm = TRUE)),
      dCH4_mean_ppb  = round(mean(s$CH4_ppb_enh), 1),
      dCH4_range_ppb = round(diff(range(s$CH4_ppb_enh)), 1),
      dC2H6_med_ppb  = round(stats::median(s$C2H6_ppb_enh), 2),
      leg_slope = round(york_slope(s$CH4_ppb_enh, s$C2H6_ppb_enh, 1, 0.2)$slope, 5),
      leg_r = round(suppressWarnings(stats::cor(s$CH4_ppb_enh, s$C2H6_ppb_enh)), 2),
      ch4_centroid_lat = round(clat, 4), ch4_centroid_lon = round(clon, 4),
      centroid_near = nearest_landmark(clat, clon),
      max_lat = round(s$Latitude[im], 4), max_lon = round(s$Longitude[im], 4),
      max_dCH4_ppb = round(s$CH4_ppb_enh[im], 1), max_dC2H6_ppb = round(s$C2H6_ppb_enh[im], 2),
      max_near = nearest_landmark(s$Latitude[im], s$Longitude[im]),
      wd_deg = round(wd), ws_ms = round(ws, 1),
      bearing_landmark_to_peak_deg = round(brg),
      peak_offwind_deg = round(downwind),        # 0 = peak exactly downwind of that landmark
      stringsAsFactors = FALSE)
  }
}
dec <- do.call(rbind, dec_rows); legs <- do.call(rbind, leg_rows)
if (is.null(dec)) stop("no flight produced a gated urban fit; check METHANE_DATA_DIR.")
write.csv(dec,  file.path(OUT_DIR, "pooled_slope_decomposition.csv"), row.names = FALSE)
write.csv(legs, file.path(OUT_DIR, "perleg_methane_location.csv"),  row.names = FALSE)

cat("\n=== pooled slope = blend of WITHIN-leg and BETWEEN-leg slopes ===\n")
cat("   between_weight = share of methane variance that is between leg means\n")
print(dec[, c("flight", "n_legs", "pooled_york", "pooled_ols", "within_slope",
              "between_slope", "between_weight", "ff_pooled_pct", "ff_within_pct",
              "sign_reversal")], row.names = FALSE)

rev <- dec[dec$sign_reversal, ]
if (nrow(rev)) {
  cat("\n=== sign reversals, explained ===\n")
  for (i in seq_len(nrow(rev))) {
    r <- rev[i, ]
    cat(sprintf("\n  %s: within-leg slope %+0.4f (fossil %0.0f%%), between-leg slope %+0.4f,\n",
                r$flight, r$within_slope, max(0, r$ff_within_pct), r$between_slope))
    cat(sprintf("    %0.0f%% of the methane variance is BETWEEN legs, so the pooled fit (%+0.4f)\n",
                100 * r$between_weight, r$pooled_ols))
    cat("    takes the sign of the between-leg contrast: the leg with the most methane has\n")
    cat("    the least ethane. That is a difference between air masses, not a source ratio.\n")
    ll <- legs[legs$flight == r$flight, ]
    ll <- ll[order(-ll$dCH4_mean_ppb), ]
    cat("    legs, most methane first:\n")
    for (j in seq_len(nrow(ll)))
      cat(sprintf("      leg %-3d n=%-4d %s-%s UTC  AGL %4d m  dCH4 %5.1f (range %5.1f)  dC2H6 %4.2f  slope %+0.4f r %+0.2f\n        methane centroid %.3f, %.3f (%s); peak %.1f ppb at %.3f, %.3f (%s)\n        wind %s deg at %s m/s; landmark->peak bearing %s deg; peak is %s deg off the downwind line\n",
                  ll$leg_id[j], ll$n_gated[j], ll$utc_start[j], ll$utc_end[j], ll$agl_m[j],
                  ll$dCH4_mean_ppb[j], ll$dCH4_range_ppb[j], ll$dC2H6_med_ppb[j],
                  ll$leg_slope[j], ll$leg_r[j],
                  ll$ch4_centroid_lat[j], ll$ch4_centroid_lon[j], ll$centroid_near[j],
                  ll$max_dCH4_ppb[j], ll$max_lat[j], ll$max_lon[j], ll$max_near[j],
                  ll$wd_deg[j], ll$ws_ms[j], ll$bearing_landmark_to_peak_deg[j], ll$peak_offwind_deg[j]))
  }
} else cat("\n  no flight has a positive within-leg slope and a negative pooled slope.\n")

cat("\n=== what the within-leg (fixed-effects) estimator gives, campaign-wide ===\n")
ffw <- pmin(100, pmax(0, dec$ff_within_pct))
cat(sprintf("  median fossil fraction over %d flights, within-leg slope, clamped: %0.0f%%\n",
            nrow(dec), stats::median(ffw)))
cat(sprintf("  (pooled York, clamped, the current headline:                     %0.0f%%)\n",
            stats::median(pmin(100, pmax(0, dec$ff_pooled_pct)))))
cat("\nwrote", file.path(OUT_DIR, "pooled_slope_decomposition.csv"), "and",
    file.path(OUT_DIR, "perleg_methane_location.csv"), "\n")
cat("Nothing published was changed; this script only reports.\n")
