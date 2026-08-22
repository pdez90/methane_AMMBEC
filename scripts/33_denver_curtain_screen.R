# 33_denver_curtain_screen.R --------------------------------------------------
# CAN a downwind-curtain mass balance be done for DENVER with these flights?
#
# WHY THIS EXISTS. Script 13 rules out a CLOSED-LOOP mass balance for Denver on
# flight geometry. A curtain imposes a weaker requirement than a closed loop, and
# some flights did fly north-south transects near the city, so a curtain has to be
# tested separately rather than assumed to fail alongside the loop. This script
# tests it instead of asserting an answer: it searches every flight for candidate
# curtain "screens" and reports, per screen, which of the four conditions a
# curtain mass balance requires are met.
#
# WHAT A CURTAIN NEEDS. A curtain (screen) mass balance replaces the closed loop
# with a single vertical wall of stacked legs downwind of the source, and
# integrates the flux of enhanced methane blowing through that wall:
#
#     F = integral_z integral_y  n_air(z) * dX_CH4 * U_perp  dy dz     [mol/s]
#
# For that integral to equal the CITY's emission, four things must hold.
#
#   (1) GEOMETRY. The whole city must lie upwind of the wall. A wall that cuts
#       through the metro area intercepts only the emissions upwind of it, so
#       the flux is a partial-city number of unknown fraction. Tested here by
#       projecting all four corners of the analysis box onto the mean wind
#       vector and requiring every corner to be upwind of the wall.
#   (2) WIDTH. The wall must be long enough to catch the whole plume, i.e. to
#       span the cross-wind extent of the city. Tested as the fraction of the
#       box's cross-wind width the screen covers.
#   (3) VERTICAL COVERAGE. Legs must be stacked from near the surface to above
#       the mixed layer, or the vertical integral is an extrapolation. Tested as
#       >= MIN_LEVELS distinct altitude levels, a lowest leg below LOW_MAX_AGL,
#       and a highest leg above the mixing height.
#   (4) TRANSPORT. The wind must actually push air through the wall, steadily.
#       A weak or swinging wind makes F scale linearly with a number that is
#       not well defined over the hours the stack takes to fly. Tested as mean
#       |U_perp| >= UPERP_MIN and directional constancy >= CONSTANCY_MIN, where
#       constancy is the ratio of the vector-mean to the scalar-mean wind (1 =
#       perfectly steady direction, 0 = uniformly variable).
#
# Screens that pass ALL FOUR are reported with the flux they would give, from
# the same line integrals and vertical integration used by script 05, so the
# answer is a number and not an opinion. Screens that fail are reported with the
# specific condition(s) they fail on.
#
# This is a FEASIBILITY diagnostic. It does not change the manuscript's emission
# estimate, which comes from the enhancement-ratio method (script 15).
#
# Needs: base R. Uses ncdf4 + lidar for mixing height when available, and falls
#        back to BLH_DEFAULT_M otherwise (the fallback is recorded per row).
# Out:   <OUT_DIR>/denver_curtain_screen.csv    one row per candidate screen
#        <OUT_DIR>/denver_curtain_verdict.csv   one row per flight
# Run:   Rscript scripts/33_denver_curtain_screen.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))

have_lidar <- requireNamespace("ncdf4", quietly = TRUE) && dir.exists(LIDAR_DIR) &&
  length(list.files(LIDAR_DIR, pattern = "velStats_.*\\.nc"))
if (have_lidar) source(file.path(proj, "R", "lidar_blh.R"))

# ---- thresholds -------------------------------------------------------------
# Deliberately GENEROUS. The point is to avoid ruling out a curtain by picking
# strict cutoffs; a screen that fails these fails by a wide margin.
HEAD_TOL      <- 30      # deg; a leg counts as N-S (or E-W) within this of the axis
GROUP_KM      <- 6       # km; legs whose offsets differ by less than this are one wall
NEAR_BOX_DEG  <- 0.35    # deg lon / 0.15 deg lat halo around the box to search in
MIN_LEGS      <- 2       # a wall needs at least two legs to be a stack at all
MIN_LEVELS    <- 3       # distinct altitude levels for a defensible vertical integral
ALT_TOL       <- 150     # m; legs within this AGL span count as one level
LOW_MAX_AGL   <- 400     # m; the lowest leg must be at least this close to the surface
WIDTH_MIN     <- 0.80    # fraction of the box's cross-wind width the wall must span
UPERP_MIN     <- 3       # m/s; mean wind component through the wall
CONSTANCY_MIN <- 0.70    # vector-mean / scalar-mean wind speed
BLH_DEFAULT_M <- 2000    # m AGL, used only when no lidar mixing height is available
WIND_MAX_AGL  <- 2000    # m AGL; samples above this do not characterize the transport

KM_PER_DEG_LAT <- 111.32
km_per_deg_lon <- function(lat) 111.32 * cos(lat * pi / 180)

box_cy <- mean(c(URBAN_BOX$lat_s, URBAN_BOX$lat_n))
box_cx <- mean(c(URBAN_BOX$lon_w, URBAN_BOX$lon_e))
KLON   <- km_per_deg_lon(box_cy)

# box corners in km east/north of the box center
CORNERS <- expand.grid(lon = c(URBAN_BOX$lon_w, URBAN_BOX$lon_e),
                       lat = c(URBAN_BOX$lat_s, URBAN_BOX$lat_n))
CORNERS$E <- (CORNERS$lon - box_cx) * KLON
CORNERS$N <- (CORNERS$lat - box_cy) * KM_PER_DEG_LAT

n_levels_of <- function(agl) {
  la <- sort(agl[is.finite(agl)]); if (!length(la)) return(0L)
  k <- 1L; base <- la[1]
  for (j in seq_along(la)[-1]) if (la[j] - base > ALT_TOL) { k <- k + 1L; base <- la[j] }
  k
}

# Group a sorted numeric vector of offsets into runs no wider than `gap`.
group_runs <- function(x, gap) {
  o <- order(x); g <- integer(length(x)); k <- 1L; g[o[1]] <- 1L
  for (j in seq_along(o)[-1]) {
    if (x[o[j]] - x[o[j - 1]] > gap) k <- k + 1L
    g[o[j]] <- k
  }
  g
}

screens <- list(); verdicts <- list()

for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb", "WS", "WD", "ALTGPS") %in% names(ic$data))) next
  fl <- sub("\\.ict$", "", sub("AMMBEC-ARL-Suite_TwinOtter_", "", basename(p)))

  d <- detect_level_legs(ic$data)
  if (!any(d$leg_id > 0)) next
  legs <- leg_metrics(d)
  if (is.null(legs) || !nrow(legs)) next

  # mixing height for this flight
  tr <- range(d$timestamp, na.rm = TRUE); blh <- BLH_DEFAULT_M; blh_src <- "default"
  if (have_lidar) {
    b <- tryCatch(blh_flight(ic$meta$date, tr[1], tr[2], LIDAR_DIR), error = function(e) NULL)
    if (!is.null(b) && isTRUE(b$n > 0) && is.finite(b$blh_m)) { blh <- b$blh_m; blh_src <- "lidar" }
  }

  # candidate legs: near the city, and aligned with one of the two axes
  near <- legs$lat >= URBAN_BOX$lat_s - 0.15 & legs$lat <= URBAN_BOX$lat_n + 0.15 &
          legs$lon >= URBAN_BOX$lon_w - NEAR_BOX_DEG & legs$lon <= URBAN_BOX$lon_e + NEAR_BOX_DEG
  ax_off <- function(tk) { a <- abs(tk) %% 180; pmin(a, 180 - a) }   # 0 = N-S, 90 = E-W
  off  <- ax_off(legs$track_deg)
  legs$axis <- ifelse(off <= HEAD_TOL, "NS", ifelse(off >= 90 - HEAD_TOL, "EW", NA))
  cand <- legs[near & !is.na(legs$axis), ]

  if (!nrow(cand)) {
    verdicts[[fl]] <- data.frame(flight = fl, date = as.character(ic$meta$date),
      n_candidate_screens = 0L, best_conditions_met = NA_integer_,
      usable = FALSE, note = "no north-south or east-west legs near the city",
      stringsAsFactors = FALSE)
    next
  }

  # one wall = same axis, similar offset (longitude for N-S walls, latitude for E-W)
  cand$offset_km <- ifelse(cand$axis == "NS", (cand$lon - box_cx) * KLON,
                                              (cand$lat - box_cy) * KM_PER_DEG_LAT)
  cand$wall <- NA_integer_
  for (ax in unique(cand$axis)) {
    k <- which(cand$axis == ax)
    cand$wall[k] <- group_runs(cand$offset_km[k], GROUP_KM) * 10L + match(ax, c("NS", "EW"))
  }

  fl_rows <- list()
  for (wid in unique(cand$wall)) {
    w <- cand[cand$wall == wid, ]
    if (nrow(w) < MIN_LEGS) next
    ax <- w$axis[1]
    s  <- d[d$leg_id %in% w$leg_id, ]
    sw <- s[is.finite(s$WS) & is.finite(s$WD) & is.finite(s$ALTAGL) & s$ALTAGL < WIND_MAX_AGL, ]
    if (nrow(sw) < 20) sw <- s[is.finite(s$WS) & is.finite(s$WD), ]
    if (nrow(sw) < 20) next

    # mean wind as a vector; constancy = |vector mean| / mean speed
    toR <- (sw$WD + 180) * pi / 180
    uE <- mean(sw$WS * sin(toR)); uN <- mean(sw$WS * cos(toR))
    spd <- sqrt(uE^2 + uN^2); scal <- mean(sw$WS)
    constancy <- if (scal > 0) spd / scal else 0
    wd_from <- (atan2(uE, uN) * 180 / pi + 180) %% 360
    dnE <- if (spd > 0) uE / spd else 0        # unit vector the wind blows TOWARD
    dnN <- if (spd > 0) uN / spd else 0

    # (1) geometry: is every corner of the box upwind of this wall?
    #     For a N-S wall the along-wind distance from a corner to the wall is the
    #     east offset times the downwind east component; likewise north for E-W.
    off_km <- mean(w$offset_km)
    reach  <- if (ax == "NS") (off_km - CORNERS$E) * dnE else (off_km - CORNERS$N) * dnN
    city_upwind <- all(reach > 0)

    # (2) width: cross-wind extent of the wall vs of the box
    if (ax == "NS") {
      span     <- (max(w$lat) - min(w$lat)) * KM_PER_DEG_LAT
      box_span <- (URBAN_BOX$lat_n - URBAN_BOX$lat_s) * KM_PER_DEG_LAT
    } else {
      span     <- (max(w$lon) - min(w$lon)) * KLON
      box_span <- (URBAN_BOX$lon_e - URBAN_BOX$lon_w) * KLON
    }
    width_frac <- span / box_span

    # (3) vertical coverage
    nlev <- n_levels_of(w$agl_m)
    lowest <- min(w$agl_m, na.rm = TRUE); highest <- max(w$agl_m, na.rm = TRUE)
    vertical_ok <- nlev >= MIN_LEVELS && lowest <= LOW_MAX_AGL && highest >= blh

    # (4) transport
    uperp <- mean(perp_wind(sw$WS, sw$WD, .circ_mean(w$track_deg)), na.rm = TRUE)
    wind_ok <- uperp >= UPERP_MIN && constancy >= CONSTANCY_MIN

    met <- sum(city_upwind, width_frac >= WIDTH_MIN, vertical_ok, wind_ok)
    usable <- met == 4L

    # A flux is computed ONLY for a screen that passes every condition, so the
    # number can never be quoted out of the context that justifies it.
    flux_t_hr <- NA_real_
    if (usable) {
      fx <- tryCatch(curtain_flux(w, blh_m = blh), error = function(e) NULL)
      if (!is.null(fx) && is.finite(fx$flux_t_hr)) flux_t_hr <- fx$flux_t_hr
    }

    # Name only the sub-condition that actually failed, so the CSV's `fails`
    # column never implicates a number that passed.
    fails <- c("city not entirely upwind of the wall")[!city_upwind]
    if (width_frac < WIDTH_MIN)
      fails <- c(fails, sprintf("wall spans %.0f%% of the city width", 100 * width_frac))
    if (nlev < MIN_LEVELS)
      fails <- c(fails, sprintf("only %d altitude level(s) in the stack", nlev))
    if (lowest > LOW_MAX_AGL)
      fails <- c(fails, sprintf("lowest leg %.0f m AGL, too high to constrain the surface layer", lowest))
    if (highest < blh)
      fails <- c(fails, sprintf("highest leg %.0f m AGL is below the %.0f m mixing height", highest, blh))
    if (uperp < UPERP_MIN)
      fails <- c(fails, sprintf("only %.1f m/s of wind through the wall", uperp))
    if (constancy < CONSTANCY_MIN)
      fails <- c(fails, sprintf("wind direction unsteady, constancy %.2f", constancy))

    fl_rows[[length(fl_rows) + 1L]] <- data.frame(
      flight = fl, date = as.character(ic$meta$date), axis = ax,
      offset_km_from_box_center = round(off_km, 1),
      n_legs = nrow(w), n_alt_levels = nlev,
      lowest_agl_m = round(lowest), highest_agl_m = round(highest),
      blh_m = round(blh), blh_src = blh_src,
      span_km = round(span, 1), width_frac = round(width_frac, 2),
      wd_from = round(wd_from), wind_ms = round(scal, 1),
      U_perp_ms = round(uperp, 1), constancy = round(constancy, 2),
      city_upwind = city_upwind, width_ok = width_frac >= WIDTH_MIN,
      vertical_ok = vertical_ok, wind_ok = wind_ok,
      conditions_met = met, usable = usable, flux_t_hr = flux_t_hr,
      fails = paste(fails, collapse = "; "), stringsAsFactors = FALSE)
  }

  if (!length(fl_rows)) {
    verdicts[[fl]] <- data.frame(flight = fl, date = as.character(ic$meta$date),
      n_candidate_screens = 0L, best_conditions_met = NA_integer_, usable = FALSE,
      note = "no wall of two or more aligned legs near the city", stringsAsFactors = FALSE)
    next
  }
  F <- do.call(rbind, fl_rows)
  screens[[fl]] <- F
  best <- F[which.max(F$conditions_met)[1], ]
  verdicts[[fl]] <- data.frame(flight = fl, date = as.character(ic$meta$date),
    n_candidate_screens = nrow(F), best_conditions_met = best$conditions_met,
    usable = any(F$usable),
    note = if (any(F$usable)) "curtain mass balance feasible" else best$fails,
    stringsAsFactors = FALSE)
}

S <- if (length(screens)) do.call(rbind, screens) else NULL
V <- if (length(verdicts)) do.call(rbind, verdicts) else NULL
if (!is.null(S)) S <- S[order(-S$conditions_met, S$flight), ]
if (!is.null(V)) V <- V[order(V$date), ]
if (!is.null(S)) write.csv(S, file.path(OUT_DIR, "denver_curtain_screen.csv"), row.names = FALSE)
if (!is.null(V)) write.csv(V, file.path(OUT_DIR, "denver_curtain_verdict.csv"), row.names = FALSE)

# ---- report -----------------------------------------------------------------
cat("\n=== Can a curtain mass balance be done for Denver with AMMBEC? ===\n")
if (is.null(S)) { cat("No candidate screens found at all.\n"); quit(save = "no") }
cat(sprintf("Flights examined with candidate walls: %d\n", length(screens)))
cat(sprintf("Candidate screens (walls of >= %d aligned legs near the city): %d\n", MIN_LEGS, nrow(S)))
cat(sprintf("Screens meeting all four conditions: %d\n\n", sum(S$usable)))
cat("Condition pass rates across candidate screens:\n")
cat(sprintf("  city entirely upwind of the wall : %d of %d\n", sum(S$city_upwind), nrow(S)))
cat(sprintf("  wall spans >= %.0f%% of city width: %d of %d\n", 100*WIDTH_MIN, sum(S$width_ok), nrow(S)))
cat(sprintf("  vertical stack adequate          : %d of %d\n", sum(S$vertical_ok), nrow(S)))
cat(sprintf("  transport adequate and steady    : %d of %d\n\n", sum(S$wind_ok), nrow(S)))
cat("Best candidates:\n")
top <- head(S, 6)
for (i in seq_len(nrow(top))) with(top[i, ], cat(sprintf(
  "  %-16s %s wall %+6.1f km, %2d legs / %d levels (%.0f-%.0f m AGL), wind %.1f m/s from %03d, U_perp %.1f, constancy %.2f -> %d/4%s\n",
  flight, axis, offset_km_from_box_center, n_legs, n_alt_levels, lowest_agl_m, highest_agl_m,
  wind_ms, wd_from, U_perp_ms, constancy, conditions_met,
  if (usable) sprintf("  FEASIBLE, flux %.1f t/hr", flux_t_hr) else paste0("  [", fails, "]"))))
cat(sprintf("\nWrote denver_curtain_screen.csv (%d screens) and denver_curtain_verdict.csv (%d flights).\n",
            nrow(S), if (is.null(V)) 0 else nrow(V)))
