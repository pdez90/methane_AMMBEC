# transects.R -------------------------------------------------------------------
# Constant-altitude, straight-flight transects from the aircraft's navigation data
# only (GPS altitude, ground track, position). No trace-gas variable is read.
#
# Why not a vertical-speed threshold: the ICARTT `Vertical_Speed` (avionics) and
# `Heave` (DGPS) fields both run at about six times the rate of change of GPS
# altitude (r ~ 0.99 when smoothed over 30 s), so a nominal 1 m/s threshold on
# `Vertical_Speed` was in effect ~0.17 m/s and split continuous transects at small
# altitude corrections. A transect is therefore defined directly by the quantity
# that matters: the aircraft holds a constant altitude and a straight track.
#
# Rule (fixed before any gas data are used with it):
#   * altitude: each new sample must lie within +/- ALT_BAND_M of the median GPS
#     altitude of the candidate transect so far (the median is recomputed after every
#     accepted sample for the first 30 s and every 30 accepted samples thereafter);
#     after the candidate ends, samples at either end more than EDGE_TRIM_M from its
#     median (the last metres of a climb or descent) are trimmed; the candidate is
#     rejected if the ordinary-least-squares slope of GPS altitude on time over the
#     trimmed segment exceeds MAX_TREND_MS in magnitude (a slow approach or descent
#     is not level flight);
#   * straight: the 20-s circular running mean of the ground track must stay within
#     HEAD_TOL_DEG of the reference heading, which is the circular mean of that
#     smoothed track over the candidate's first INIT_S seconds; a turn ends it;
#   * gaps: up to GAP_MAX_S consecutive samples failing either test are bridged only
#     if a sample passing both tests follows; a transect never starts or ends on a
#     bridged sample, so a sustained turn, climb or descent always ends it;
#   * size: at least MIN_S seconds and MIN_KM kilometres;
#   * merge: consecutive transects separated by at most MERGE_GAP_S seconds whose mean
#     ground tracks differ by at most MERGE_HEAD_DEG and median altitudes by at most
#     MERGE_ALT_M are one pass (a gentle course correction split it) and are joined,
#     the samples between them included.
# Each sample keeps its own position: region is assigned per sample, not per
# transect, so a transect crossing the box edge contributes only its in-box part.
# -------------------------------------------------------------------------------

TRANSECT_RULE <- list(ALT_BAND_M = 50, EDGE_TRIM_M = 25, MAX_TREND_MS = 0.25,
                      HEAD_TOL_DEG = 30, SMOOTH_TRACK_S = 20, INIT_S = 10,
                      GAP_MAX_S = 20, MIN_S = 60, MIN_KM = 2,
                      MERGE_GAP_S = 30, MERGE_HEAD_DEG = 15, MERGE_ALT_M = 50)

.tr_hav_km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180; dlat <- (lat2 - lat1) * r; dlon <- (lon2 - lon1) * r
  a <- sin(dlat / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin(dlon / 2)^2
  6371 * 2 * atan2(sqrt(a), sqrt(1 - a))
}
.tr_angdiff <- function(a, b) abs(((a - b + 180) %% 360) - 180)
.tr_smooth_track <- function(trk, w) {          # circular running mean
  ok <- is.finite(trk); s <- ifelse(ok, sin(trk * pi / 180), 0); c <- ifelse(ok, cos(trk * pi / 180), 0)
  k <- rep(1 / w, w)
  ss <- as.numeric(stats::filter(s, k, sides = 2)); cc <- as.numeric(stats::filter(c, k, sides = 2))
  nn <- as.numeric(stats::filter(as.numeric(ok), k, sides = 2))
  out <- (atan2(ss, cc) * 180 / pi) %% 360
  out[!is.finite(nn) | nn < 0.5] <- NA
  out
}

#' Constant-altitude transects. Returns df with integer `transect_id` (0 = none).
detect_transects <- function(df, rule = TRANSECT_RULE) {
  n <- nrow(df); alt <- df$ALTGPS
  trk <- .tr_smooth_track(df$TrackAngleTrue, rule$SMOOTH_TRACK_S)
  ok <- is.finite(alt) & is.finite(trk) & is.finite(df$Latitude) & is.finite(df$Longitude)
  id <- integer(n); k <- 0L; i <- 1L
  while (i <= n) {
    if (!ok[i]) { i <- i + 1L; next }
    w0 <- i:min(n, i + rule$INIT_S - 1L); w0 <- w0[ok[w0]]
    h0 <- .tr_circmean(trk[w0])
    j <- i; last_good <- i; gap <- 0L; med <- alt[i]; na <- 1L
    while (j < n) {
      nx <- j + 1L
      good <- ok[nx] && abs(alt[nx] - med) <= rule$ALT_BAND_M && .tr_angdiff(trk[nx], h0) <= rule$HEAD_TOL_DEG
      if (good) {
        j <- nx; last_good <- nx; gap <- 0L; na <- na + 1L
        if (na <= 30L || na %% 30L == 0L) med <- stats::median(alt[i:nx][ok[i:nx]])   # running median of the transect so far
      }
      else if (gap < rule$GAP_MAX_S) { j <- nx; gap <- gap + 1L }
      else break
    }
    j <- last_good                                   # never end on a bridged gap
    if (j > i) {                                     # trim climb/descent tails at both ends
      med <- stats::median(alt[i:j][ok[i:j]])
      while (i < j && (!ok[i] || abs(alt[i] - med) > rule$EDGE_TRIM_M)) i <- i + 1L
      while (j > i && (!ok[j] || abs(alt[j] - med) > rule$EDGE_TRIM_M)) j <- j - 1L
    }
    tt <- as.numeric(df$timestamp[i:j]); tt <- tt - tt[1]; aa <- alt[i:j]; kk <- is.finite(aa) & is.finite(tt)
    trend <- if (sum(kk) > 10) abs(unname(stats::coef(stats::lm(aa[kk] ~ tt[kk]))[2])) else Inf
    if (!is.finite(trend)) trend <- Inf
    dur <- as.numeric(difftime(df$timestamp[j], df$timestamp[i], units = "secs"))
    len <- if (j > i) sum(.tr_hav_km(df$Latitude[i:(j - 1)], df$Longitude[i:(j - 1)],
                                     df$Latitude[(i + 1):j], df$Longitude[(i + 1):j]), na.rm = TRUE) else 0
    if (is.finite(dur) && is.finite(len) && dur >= rule$MIN_S && len >= rule$MIN_KM && trend <= rule$MAX_TREND_MS) {
      k <- k + 1L; id[i:j] <- k; i <- j + 1L
    } else i <- i + 1L
  }
  df$transect_id <- .tr_merge(df, id, trk, rule)
  df
}

.tr_circmean <- function(deg) { deg <- deg[is.finite(deg)]
  (atan2(mean(sin(deg * pi / 180)), mean(cos(deg * pi / 180))) * 180 / pi) %% 360 }

# join consecutive transects that are one pass split by a gentle course correction
.tr_merge <- function(df, id, trk, rule) {
  repeat {
    ids <- sort(unique(id[id > 0])); if (length(ids) < 2) break
    st <- sapply(ids, function(g) min(which(id == g))); en <- sapply(ids, function(g) max(which(id == g)))
    hd <- sapply(ids, function(g) .tr_circmean(trk[id == g])); am <- sapply(ids, function(g) stats::median(df$ALTGPS[id == g], na.rm = TRUE))
    gap <- as.numeric(difftime(df$timestamp[st[-1]], df$timestamp[en[-length(en)]], units = "secs"))
    hit <- which(gap <= rule$MERGE_GAP_S & .tr_angdiff(hd[-1], hd[-length(hd)]) <= rule$MERGE_HEAD_DEG &
                 abs(am[-1] - am[-length(am)]) <= rule$MERGE_ALT_M)
    if (!length(hit)) break
    k <- hit[1]; id[st[k]:en[k + 1]] <- ids[k]              # absorb the next transect and the gap between
  }
  ids <- sort(unique(id[id > 0])); id[id > 0] <- match(id[id > 0], ids)   # renumber 1..K in time order
  id
}

#' Per-sample region: "urban" inside the box, "basin" north of basin_lat, else "edge".
sample_region <- function(lat, lon, box = list(lat_s = 39.50, lat_n = 39.95, lon_w = -105.20, lon_e = -104.55),
                          basin_lat = 40.05) {
  ifelse(is.finite(lat) & lat >= box$lat_s & lat <= box$lat_n & lon >= box$lon_w & lon <= box$lon_e, "urban",
         ifelse(is.finite(lat) & lat >= basin_lat, "basin", "edge"))
}

#' One row per transect: timing, geometry, altitude statistics, and how many of its
#' samples fall in each region. Navigation variables only.
transect_table <- function(df) {
  ids <- sort(unique(df$transect_id[df$transect_id > 0]))
  if (!length(ids)) return(NULL)
  reg <- sample_region(df$Latitude, df$Longitude)
  gr <- c(0, diff(df$ALTGPS)); gr30 <- as.numeric(stats::filter(gr, rep(1 / 30, 30), sides = 2))
  do.call(rbind, lapply(ids, function(g) {
    w <- which(df$transect_id == g); s <- df[w, ]
    len <- sum(.tr_hav_km(s$Latitude[-nrow(s)], s$Longitude[-nrow(s)], s$Latitude[-1], s$Longitude[-1]), na.rm = TRUE)
    th <- s$TrackAngleTrue[is.finite(s$TrackAngleTrue)] * pi / 180
    data.frame(transect_id = g, t_start = format(s$timestamp[1], "%H:%M:%S"), t_end = format(s$timestamp[nrow(s)], "%H:%M:%S"),
               dur_s = nrow(s), length_km = round(len, 1),
               alt_med_m = round(stats::median(s$ALTGPS, na.rm = TRUE)),
               alt_range_m = round(diff(range(s$ALTGPS, na.rm = TRUE))),
               agl_med_m = if ("ALTAGL" %in% names(s)) round(stats::median(s$ALTAGL, na.rm = TRUE)) else NA,
               track_deg = round((atan2(mean(sin(th)), mean(cos(th))) * 180 / pi) %% 360),
               share_climbing = round(mean(abs(gr30[w]) > 1, na.rm = TRUE), 3),
               n_urban = sum(reg[w] == "urban"), n_basin = sum(reg[w] == "basin"), n_edge = sum(reg[w] == "edge"),
               stringsAsFactors = FALSE)
  }))
}
