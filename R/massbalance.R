# massbalance.R --------------------------------------------------------------
# First-order airborne mass-balance flux for methane (AMMBEC memo section 1).
#
# METHOD (single downwind "curtain"): the aircraft flies a set of straight,
# level legs downwind of a source at several altitudes. The CH4 flux through the
# vertical plane spanned by those legs is
#
#     F = integral_z integral_y  n_air(z) * dX_CH4 * U_perp  dy dz      [mol/s]
#
# where n_air is air molar density, dX_CH4 the CH4 mole-fraction enhancement
# above background, U_perp the wind component normal to the leg, y along-leg
# distance and z altitude. This module gives you the pieces:
#   detect_level_legs() -> candidate transects
#   leg_metrics()       -> per-leg length, heading, enhancement, U_perp, line integral
#   curtain_flux()      -> vertical integration to a total flux (kg/hr, t/hr)
#
# IMPORTANT — this is a scaffold, not a turnkey inventory number. A valid flux
# requires YOU to: (1) pick the legs that actually form the downwind screen of
# ONE source region (drop transit/upwind/spiral legs), (2) choose the background,
# (3) supply a boundary-layer height (Doppler-lidar/PUMAS or a sounding). The
# defaults let the pipeline run and produce diagnostics to make those choices.
# -----------------------------------------------------------------------------

MW_CH4 <- 16.043            # g/mol
Rgas   <- 8.314             # J/mol/K

# Great-circle distance (km) between successive points (vectorised pairs).
.hav_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371; d2r <- pi/180
  dlat <- (lat2-lat1)*d2r; dlon <- (lon2-lon1)*d2r
  h <- sin(dlat/2)^2 + cos(lat1*d2r)*cos(lat2*d2r)*sin(dlon/2)^2
  2*R*asin(pmin(1, sqrt(h)))
}

# Circular mean of angles (degrees).
.circ_mean <- function(deg) {
  r <- deg*pi/180
  # NB parentheses: %% binds tighter than / in R, so without them the intended
  # mod-360 wrap silently became /(pi %% 360) and headings could come back
  # negative. Harmless downstream (trig and %%-folding are periodic) but wrong.
  (atan2(mean(sin(r), na.rm=TRUE), mean(cos(r), na.rm=TRUE)) * 180/pi) %% 360
}

#' Air molar density (mol/m^3) from US-standard-atmosphere pressure at altitude
#' z (m) and measured temperature T (degC). n = P/(R T_K).
air_molar_density <- function(z_m, tempC) {
  P <- 101325 * (1 - 2.25577e-5 * pmax(z_m, 0))^5.25588   # Pa
  Tk <- ifelse(is.finite(tempC), tempC + 273.15, 288.15 - 0.0065*pmax(z_m,0))
  P / (Rgas * Tk)
}

#' Wind component (m/s) perpendicular to a leg of given track heading.
#' WD is the meteorological direction the wind blows FROM (deg); WS is speed.
perp_wind <- function(ws, wd_from, track_deg) {
  to <- (wd_from + 180) * pi/180                 # direction wind blows TOWARD
  wE <- ws*sin(to); wN <- ws*cos(to)             # wind vector (east, north)
  th <- track_deg*pi/180
  nE <- cos(th); nN <- -sin(th)                  # unit normal to track
  abs(wE*nE + wN*nN)
}

#' Detect straight, level flight legs (candidate transects).
#'
#' Vertical speed is turbulent second-to-second even in level flight, so the
#' vertical rate is smoothed before thresholding and short out-of-tolerance
#' gaps are bridged. A leg must also hold a roughly constant heading and a
#' small altitude spread end-to-end.
#'
#' @param df ICARTT data.frame (needs ALTGPS, timestamp, TrackAngleTrue, and
#'   ideally Vertical_Speed).
#' @param vspeed_max Max |smoothed vertical speed| (m/s) for level (default 1.0).
#' @param min_seconds Minimum leg duration (default 60 s).
#' @param min_km Minimum leg length (default 2 km).
#' @param head_tol Max heading spread within a leg (deg, default 30).
#' @param smooth_s Vertical-speed smoothing window (s, default 20).
#' @param alt_tol Max altitude spread within a leg (m, default 150).
#' @param gap_max Max consecutive non-level samples bridged inside a leg (default 10).
#' @return the data.frame with an integer `leg_id` column (0 = no leg).
detect_level_legs <- function(df, vspeed_max = 1.0, min_seconds = 60,
                              min_km = 2, head_tol = 30, smooth_s = 20,
                              alt_tol = 150, gap_max = 10) {
  n <- nrow(df)
  vs_raw <- if ("Vertical_Speed" %in% names(df)) df$Vertical_Speed else
    c(0, diff(df$ALTGPS) / pmax(1, as.numeric(diff(df$timestamp), units="secs")))
  w <- max(1L, as.integer(smooth_s))
  vs <- as.numeric(stats::filter(ifelse(is.finite(vs_raw), vs_raw, 0),
                                 rep(1/w, w), sides = 2))
  vs[is.na(vs)] <- vs_raw[is.na(vs)]
  level <- is.finite(vs) & abs(vs) <= vspeed_max &
           is.finite(df$ALTGPS) & is.finite(df$TrackAngleTrue)

  .hspread <- function(a, b) abs(((a - b + 180) %% 360) - 180)

  leg_id <- integer(n); id <- 0; i <- 1
  while (i <= n) {
    if (!isTRUE(level[i])) { i <- i + 1; next }
    j <- i; gap <- 0
    while (j < n) {
      nx <- j + 1
      hok <- is.finite(df$TrackAngleTrue[nx]) &&
             .hspread(df$TrackAngleTrue[nx], df$TrackAngleTrue[i]) <= head_tol
      if (isTRUE(level[nx]) && hok)            { j <- nx; gap <- 0 }
      else if (!isTRUE(level[nx]) && hok && gap < gap_max) { j <- nx; gap <- gap + 1 }
      else break
    }
    dur <- as.numeric(difftime(df$timestamp[j], df$timestamp[i], units="secs"))
    len <- sum(.hav_km(df$Latitude[i:(j-1)], df$Longitude[i:(j-1)],
                       df$Latitude[(i+1):j], df$Longitude[(i+1):j]), na.rm=TRUE)
    aspread <- diff(range(df$ALTGPS[i:j], na.rm = TRUE))
    if (is.finite(dur) && dur >= min_seconds && len >= min_km &&
        is.finite(aspread) && aspread <= alt_tol) {
      id <- id + 1; leg_id[i:j] <- id
    }
    i <- j + 1
  }
  df$leg_id <- leg_id
  df
}

#' Per-leg metrics, including the horizontal line integral of the flux integrand
#' L = integral_y n_air * dX_CH4 * U_perp dy   [mol / (m s)]  (per unit height).
#'
#' @param df output of detect_level_legs(), with CH4_ppb, WS, WD, Temp_True.
#' @param background CH4 background (ppb). Default: 5th percentile over all legs.
#' @param windprof Optional read_windprof() object. If supplied, the
#'   perpendicular wind uses the profiler winds interpolated to each leg's
#'   altitude and time (falling back to aircraft WS/WD when the profiler does
#'   not cover the leg). Requires wind_at() from R/read_windprof.R.
leg_metrics <- function(df, background = NULL, windprof = NULL) {
  legs <- sort(unique(df$leg_id[df$leg_id > 0]))
  inleg <- df[df$leg_id > 0, ]
  if (is.null(background))
    background <- stats::quantile(inleg$CH4_ppb, 0.05, na.rm=TRUE, names=FALSE)

  out <- lapply(legs, function(g) {
    s <- df[df$leg_id == g, ]
    s <- s[is.finite(s$CH4_ppb) & is.finite(s$WS) & is.finite(s$WD), ]
    if (nrow(s) < 5) return(NULL)
    # along-leg step distances (m)
    dy <- .hav_km(s$Latitude[-nrow(s)], s$Longitude[-nrow(s)],
                  s$Latitude[-1], s$Longitude[-1]) * 1000
    track <- .circ_mean(s$TrackAngleTrue)
    wind_src <- "aircraft"
    if (!is.null(windprof)) {
      w <- wind_at(windprof, s$timestamp[which(is.finite(s$timestamp))[1]],
                   mean(s$ALTGPS, na.rm = TRUE))
      if (is.finite(w$ws) && is.finite(w$wd)) {
        s$WS <- w$ws; s$WD <- w$wd; wind_src <- "profiler"
      }
    }
    up <- perp_wind(s$WS, s$WD, track)                       # m/s per sample
    nair <- air_molar_density(s$ALTGPS, if ("Temp_True" %in% names(s)) s$Temp_True else NA)
    dX <- pmax(0, (s$CH4_ppb - background)) * 1e-9           # mol/mol, enhancement
    integrand <- nair * dX * up                              # mol/(m^3) * m/s = mol/(m^2 s)
    # trapezoid along y using midpoint integrand
    mid <- (integrand[-length(integrand)] + integrand[-1]) / 2
    L <- sum(mid * dy, na.rm=TRUE)                           # mol/(m s)
    data.frame(leg_id = g, n = nrow(s),
               lat = round(mean(s$Latitude, na.rm=TRUE), 3),
               lon = round(mean(s$Longitude, na.rm=TRUE), 3),
               mean_alt_m = round(mean(s$ALTGPS, na.rm=TRUE)),
               agl_m = round(mean(s$ALTAGL, na.rm=TRUE)),
               track_deg = round(track,1),
               length_km = round(sum(dy, na.rm=TRUE)/1000, 2),
               ch4_enh_mean_ppb = round(mean(pmax(0,s$CH4_ppb-background), na.rm=TRUE),1),
               U_perp_ms = round(mean(up, na.rm=TRUE),2),
               wind_src = wind_src,
               L_mol_per_m_s = L,
               stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, Filter(Negate(is.null), out))
  attr(res, "background") <- background
  res
}

#' Vertically integrate the per-leg line integrals L(z) to a total flux.
#'
#' Trapezoidal in altitude across the leg AGL heights; below the lowest leg L is
#' held constant to the surface (well-mixed), above the highest leg L tapers
#' linearly to 0 at the boundary-layer height `blh_m`.
#'
#' @param legs leg_metrics() output.
#' @param blh_m Boundary-layer height (m AGL). SET THIS from lidar/sounding.
#' @return list(flux_mol_s, flux_kg_hr, flux_t_hr, detail).
curtain_flux <- function(legs, blh_m = 2000) {
  legs <- legs[order(legs$agl_m), ]
  z <- legs$agl_m; L <- legs$L_mol_per_m_s
  if (length(z) < 1) return(NULL)
  # build a profile from 0 -> blh
  zz <- c(0, z, blh_m)
  LL <- c(L[1], L, 0)                       # surface = lowest leg; 0 at BLH top
  keep <- zz <= blh_m & c(TRUE, !duplicated(zz[-1]))
  zz <- zz[keep]; LL <- LL[keep]
  o <- order(zz); zz <- zz[o]; LL <- LL[o]
  flux_mol_s <- sum((LL[-1] + LL[-length(LL)])/2 * diff(zz))   # integral L dz
  list(flux_mol_s = flux_mol_s,
       flux_kg_hr = flux_mol_s * MW_CH4/1000 * 3600,
       flux_t_hr  = flux_mol_s * MW_CH4/1000 * 3600 / 1000,
       detail = data.frame(z_m = zz, L_mol_per_m_s = LL))
}

# --- Per-flight curtain configuration ---------------------------------------
# A CSV you fill in to turn the diagnostic into a real flux. One row per flight:
#   flight          basename of the .ict (matched exactly)
#   blh_m           boundary-layer height in m AGL (from lidar/PUMAS/sounding)
#   leg_ids         which legs form the downwind screen, e.g. "4;5;6;8"
#                     (blank = use ALL detected legs)
#   background_ppb  upwind CH4 background in ppb (blank = auto 5th percentile)
# Blank cells fall back to the automatic behavior, so a half-filled row still
# works. See scripts/05_massbalance_flux.R.

#' Parse a leg-id cell ("4;5;6" or "4,5,6") to a numeric vector; blank -> NULL.
parse_leg_ids <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return(NULL)
  as.integer(strsplit(x, "[;, ]+")[[1]])
}

#' Read a curtain-config CSV into a lookup keyed by flight basename.
#' Returns NULL if the file does not exist.
read_curtain_config <- function(path) {
  if (!file.exists(path)) return(NULL)
  cfg <- utils::read.csv(path, colClasses = "character", check.names = FALSE)
  if (!"flight" %in% names(cfg)) return(NULL)
  split(cfg, cfg$flight)
}

#' Closed-loop ("box"/perimeter) mass-balance flux.
#'
#' For flights that encircle a source at one or more altitudes (rather than
#' flying a single stacked downwind wall), the emission equals the net methane
#' advected OUT through the flight loop:
#'   F = integral_z  ( closed-loop integral of  n_air * dX_CH4 * (u . n_out) dl )  dz
#' where n_out is the outward-facing horizontal normal of the loop. Using the
#' SIGNED wind component (positive = outflow) makes inflow and outflow cancel
#' correctly, which the single-wall curtain_flux() cannot do.
#'
#' @param d Full ICARTT data with a leg_id column (from detect_level_legs()).
#' @param leg_ids Legs forming the loop (all altitudes).
#' @param background CH4 background (ppb); default 5th percentile of the loop.
#' @param blh_m Boundary-layer height (m AGL).
#' @param windprof Optional read_windprof() object (per-sample wind override).
#' @return list(flux_t_hr, flux_kg_hr, levels) — first-order; see caveats in README.
# Outward unit normals for an ordered set of loop points (meters, E/N), computed
# from the LOCAL path tangent (adjacent points), rotated 90 deg, and flipped to
# point away from the loop centroid. This is the true path-normal, not the radial
# vector from the centroid (which is only correct for a circle).
.path_normals <- function(E, N) {
  n <- length(E)
  ip <- pmax(1L, seq_len(n) - 1L); iq <- pmin(n, seq_len(n) + 1L)
  tE <- E[iq] - E[ip]; tN <- N[iq] - N[ip]            # local tangent
  L <- sqrt(tE^2 + tN^2); L[!is.finite(L) | L == 0] <- 1
  tE <- tE / L; tN <- tN / L
  nE <- tN; nN <- -tE                                 # right-hand normal
  cE <- mean(E); cN <- mean(N)
  rad <- (E - cE) * nE + (N - cN) * nN                # dot with centroid->point
  flip <- is.finite(rad) & rad < 0
  nE[flip] <- -nE[flip]; nN[flip] <- -nN[flip]        # make all point outward
  list(nE = nE, nN = nN)
}

# Convex-hull perimeter (m) of a set of E/N points — the reference closed contour.
.hull_perimeter <- function(E, N) {
  if (length(E) < 3) return(NA_real_)
  h <- grDevices::chull(E, N); h <- c(h, h[1])
  sum(sqrt(diff(E[h])^2 + diff(N[h])^2))
}

# Azimuthal-coverage gap of a loop, viewed from its centroid: the fraction of the
# compass sectors around the centroid that contain NO flown point. This measures
# how much of the perimeter *around the source* was actually sampled, and is robust
# to repeated passes and stacked altitudes (revisiting a sector does not increase
# coverage) — unlike summing flown path length, which they inflate. Computed per
# altitude level. Returns NA if too few points to define an enclosure.
.azimuth_gap <- function(E, N, nsec = 36L) {
  ok <- is.finite(E) & is.finite(N)
  E <- E[ok]; N <- N[ok]
  if (length(E) < 3) return(NA_real_)
  cE <- mean(E); cN <- mean(N)
  ang <- atan2(N - cN, E - cE)                        # -pi..pi
  sec <- floor((ang + pi) / (2*pi) * nsec) + 1L
  sec <- pmin(pmax(sec, 1L), nsec)
  occ <- tabulate(sec, nbins = nsec) > 0
  1 - sum(occ) / nsec
}

perimeter_flux <- function(d, leg_ids, background = NULL, blh_m = 2000, windprof = NULL,
                           alt_tol = 150, closure_frac_max = 0.20) {
  s <- d[d$leg_id %in% leg_ids &
         is.finite(d$Latitude) & is.finite(d$Longitude) &
         is.finite(d$CH4_ppb) & is.finite(d$ALTGPS), ]
  if (nrow(s) < 20) return(NULL)

  # Winds: use aircraft WS/WD, optionally overridden by the profiler (needs BOTH
  # speed and direction). Do not require aircraft wind when the profiler supplies it.
  if (!is.null(windprof)) {
    for (i in seq_len(nrow(s))) {
      w <- wind_at(windprof, s$timestamp[i], if (is.finite(s$ALTAGL[i])) s$ALTAGL[i] else s$ALTGPS[i])
      if (is.finite(w$ws) && is.finite(w$wd)) { s$WS[i] <- w$ws; s$WD[i] <- w$wd }
    }
  }
  s <- s[is.finite(s$WS) & is.finite(s$WD), ]
  if (nrow(s) < 20) return(NULL)
  s <- s[order(s$timestamp), ]
  if (is.null(background)) background <- stats::quantile(s$CH4_ppb, 0.05, na.rm = TRUE, names = FALSE)

  # local ENU meters
  clat <- mean(s$Latitude); clon <- mean(s$Longitude)
  mLat <- 111320; mLon <- 111320 * cos(clat * pi/180)
  s$E <- (s$Longitude - clon) * mLon; s$N <- (s$Latitude - clat) * mLat
  s$agl <- ifelse(is.finite(s$ALTAGL), s$ALTAGL, s$ALTGPS)

  # cluster legs into altitude LEVELS by their mean AGL (tolerance alt_tol),
  # rather than rounding to a fixed 400 m bin.
  legagl <- tapply(s$agl, s$leg_id, mean)
  ord <- order(legagl); la <- as.numeric(legagl[ord]); lid <- as.integer(names(legagl)[ord])
  cl <- integer(length(la)); k <- 1L; cl[1] <- 1L; base <- la[1]
  for (j in seq_along(la)[-1]) { if (la[j] - base > alt_tol) { k <- k + 1L; base <- la[j] }; cl[j] <- k }
  lvl_of_leg <- setNames(cl, lid)
  s$lvl <- lvl_of_leg[as.character(s$leg_id)]

  # per LEVEL: sum the trapezoidal line integral of n_air*dX*(w.n_out) around the
  # loop, computing the outward normal per leg (contiguous points) from the path
  # tangent, and diagnose closure via the convex-hull perimeter.
  lev <- sort(unique(s$lvl))
  Llev <- numeric(length(lev)); zlev <- numeric(length(lev))
  azgap <- numeric(length(lev))
  for (li in seq_along(lev)) {
    sl <- s[s$lvl == lev[li], ]
    cE <- mean(sl$E); cN <- mean(sl$N)                # level centroid for outward test
    Ltot <- 0
    for (g in unique(sl$leg_id)) {
      p <- sl[sl$leg_id == g, ]; if (nrow(p) < 2) next
      nn <- .path_normals(p$E - cE + cE, p$N - cN + cN)   # tangents from this leg's points
      # ensure outward relative to the LEVEL centroid (not the leg centroid)
      rad <- (p$E - cE) * nn$nE + (p$N - cN) * nn$nN
      fl <- is.finite(rad) & rad < 0
      nn$nE[fl] <- -nn$nE[fl]; nn$nN[fl] <- -nn$nN[fl]
      toR <- (p$WD + 180) * pi/180
      wE <- p$WS * sin(toR); wN <- p$WS * cos(toR)
      u_out <- wE * nn$nE + wN * nn$nN                    # signed normal wind
      nair <- air_molar_density(p$ALTGPS, if ("Temp_True" %in% names(p)) p$Temp_True else NA)
      dX <- (p$CH4_ppb - background) * 1e-9               # signed enhancement
      integ <- nair * dX * u_out                          # mol/m^2/s
      seg <- sqrt(diff(p$E)^2 + diff(p$N)^2)              # segment lengths (m)
      mid <- (integ[-length(integ)] + integ[-1]) / 2      # trapezoidal midpoint
      Ltot <- Ltot + sum(mid * seg, na.rm = TRUE)         # mol/(m s)
    }
    Llev[li] <- Ltot; zlev[li] <- mean(sl$agl)
    # closure is measured by AZIMUTHAL coverage at this level (fraction of the
    # compass around the level centroid never sampled), NOT by flown path length,
    # which repeated passes and stacked altitudes would inflate.
    azgap[li] <- .azimuth_gap(sl$E, sl$N)
  }
  keep <- zlev <= blh_m
  Llev <- Llev[keep]; zlev <- zlev[keep]; azgap <- azgap[keep]
  if (!length(Llev)) return(NULL)

  # closure diagnostic: worst per-level azimuthal gap among the levels used in the
  # vertical integration (a valid box must be closed at every level).
  closure_gap_frac <- suppressWarnings(max(azgap, na.rm = TRUE))
  closed <- is.finite(closure_gap_frac) && closure_gap_frac <= closure_frac_max
  n_levels <- length(zlev)

  # vertical integration across levels (each owns the band to neighbor midpoints,
  # from the surface to blh_m). Requires >= 2 levels to resolve the profile.
  o <- order(zlev); z <- zlev[o]; L <- Llev[o]
  edges <- c(0, (z[-length(z)] + z[-1]) / 2, blh_m)
  flux_mol_s <- sum(L * diff(edges))
  list(flux_mol_s = flux_mol_s,
       flux_kg_hr = flux_mol_s * MW_CH4/1000 * 3600,
       flux_t_hr  = flux_mol_s * MW_CH4/1000 * 3600 / 1000,
       background = background, n_levels = n_levels,
       closure_gap_frac = round(closure_gap_frac, 3), closed = closed,
       levels = data.frame(z = round(z), L_mol_per_m_s = L))
}

#' Tag legs by geographic region using a Denver-metro bounding box.
#'
#' Denver and the DJB basin are adjacent, so an urban mass balance must be built
#' from the legs flown inside the metro box rather than the whole flight. Legs
#' with mean latitude below the box's north edge (and inside its lon range) are
#' "urban"; legs north of a basin threshold are "basin"; the gap between is
#' "edge" (ambiguous — inspect before using).
#'
#' @param legs leg_metrics() output (needs lat, lon).
#' @param box list(lat_s, lat_n, lon_w, lon_e) metro box; north edge = lat_n.
#' @param basin_lat latitude north of which a leg is unambiguously basin.
#' @return legs with an added `region` column ("urban"/"edge"/"basin").
tag_region <- function(legs, box = list(lat_s = 39.50, lat_n = 39.95,
                                        lon_w = -105.20, lon_e = -104.55),
                       basin_lat = 40.05) {
  inlon <- legs$lon >= box$lon_w & legs$lon <= box$lon_e
  legs$region <- ifelse(is.finite(legs$lat) & legs$lat >= box$lat_s &
                          legs$lat <= box$lat_n & inlon, "urban",
                 ifelse(is.finite(legs$lat) & legs$lat >= basin_lat, "basin", "edge"))
  legs
}

#' Write a template curtain-config CSV (one blank row per flight) if none exists.
#' `info` is a data.frame with columns flight, date. Does not overwrite.
write_curtain_template <- function(path, info) {
  if (file.exists(path)) return(invisible(FALSE))
  tmpl <- data.frame(flight = info$flight, date = info$date,
                     blh_m = "", leg_ids = "", background_ppb = "",
                     stringsAsFactors = FALSE)
  utils::write.csv(tmpl, path, row.names = FALSE)
  invisible(TRUE)
}
