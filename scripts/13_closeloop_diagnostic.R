# 13_closeloop_diagnostic.R --------------------------------------------------
# QUALITATIVE loop-geometry diagnostic. No flux, no numeric closure statistic.
#
# A closed-loop (box) mass balance requires the aircraft to encircle the source
# at two or more altitude levels, so the net methane crossing the loop equals the
# enclosed emission. The AMMBEC flights were laid out to encircle the DJB oil and
# gas BASIN to the north-east, not the Denver metropolitan area, so none of them
# forms a closed contour around the city. This script documents that qualitatively:
# for each urban flight it reports the urban-leg count, the number of altitude
# levels, and whether the flown urban legs even approximate an enclosure of the
# metropolitan box (they do not). It deliberately does NOT compute a flux or a
# precise "closure gap" percentage, because with this basin-optimized geometry
# such a number would carry more meaning than the flight design supports. The
# urban emission rate comes from the enhancement-ratio method (script 15); the
# measured Doppler-lidar mixing heights are retained here only as documentation.
#
# Run:  Rscript scripts/13_closeloop_diagnostic.R
# Out:  <OUT_DIR>/closeloop_diagnostic.csv
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R"))
have_lidar <- requireNamespace("ncdf4", quietly = TRUE) && dir.exists(LIDAR_DIR) &&
  length(list.files(LIDAR_DIR, pattern = "velStats_.*\\.nc"))
if (have_lidar) source(file.path(proj, "R", "lidar_blh.R"))

MIN_URBAN_LEGS <- 3
ALT_TOL <- 150                         # m; legs within this AGL span are one level
# Internal screening HEURISTIC only (not a formal mass-balance criterion): a flight
# is flagged as "encircling" the metro box if it samples most of the compass around
# the box centre at >= 2 altitudes. The exact fraction below is a coarse cutoff used
# only to produce a yes/no label; NO coverage percentage is reported, and the
# manuscript states simply that no flight approximated a complete metropolitan
# enclosure. In this campaign every flight falls far short, so the label is robust
# to the exact cutoff.
ENCLOSE_COV_MIN <- 0.85
box_cx <- mean(c(URBAN_BOX$lon_w, URBAN_BOX$lon_e))
box_cy <- mean(c(URBAN_BOX$lat_s, URBAN_BOX$lat_n))

# Azimuthal coverage around the METRO BOX CENTRE (not the flown centroid): the
# fraction of compass sectors that contain a flown point. A pattern that encircles
# the city covers ~all sectors; a one-sided or transect pattern covers few. Used
# only to CLASSIFY enclosure qualitatively; the number itself is never reported.
azcov_box <- function(lon, lat, nsec = 36L) {
  E <- (lon - box_cx) * 111320 * cos(box_cy * pi/180); N <- (lat - box_cy) * 111320
  ok <- is.finite(E) & is.finite(N); if (sum(ok) < 3) return(0)
  ang <- atan2(N[ok], E[ok]); sec <- floor((ang + pi)/(2*pi)*nsec) + 1L
  sec <- pmin(pmax(sec, 1L), nsec); sum(tabulate(sec, nbins = nsec) > 0) / nsec
}
n_levels_of <- function(agl) {
  la <- sort(agl[is.finite(agl)]); if (!length(la)) return(0L)
  k <- 1L; base <- la[1]
  for (j in seq_along(la)[-1]) if (la[j] - base > ALT_TOL) { k <- k + 1L; base <- la[j] }
  k
}

rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb","ALTGPS") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  legs <- tag_region(leg_metrics(d))
  urb <- legs[legs$region == "urban" & legs$agl_m < 3000 & legs$length_km < 20, ]
  if (nrow(urb) < MIN_URBAN_LEGS) next
  s <- d[d$leg_id %in% urb$leg_id & is.finite(d$Latitude) & is.finite(d$Longitude), ]

  n_lev <- n_levels_of(urb$agl_m)
  cov   <- azcov_box(s$Longitude, s$Latitude)
  encloses <- n_lev >= 2 && cov >= ENCLOSE_COV_MIN
  reason <- if (n_lev < 2) "single altitude level (no vertical closure)" else
            if (!encloses) "flight does not encircle the metropolitan box (basin-optimized geometry)" else
            "encircles the metropolitan box"

  tr <- range(d$timestamp, na.rm = TRUE); blh <- NA_real_; blh_src <- "none"
  if (have_lidar) {
    b <- tryCatch(blh_flight(ic$meta$date, tr[1], tr[2], LIDAR_DIR), error = function(e) NULL)
    if (!is.null(b) && isTRUE(b$n > 0) && is.finite(b$blh_m)) { blh <- round(b$blh_m); blh_src <- "lidar" }
  }
  rows[[p]] <- data.frame(
    flight = sub("AMMBEC-ARL-Suite_TwinOtter_","",sub(".ict","",basename(p))),
    date = as.character(ic$meta$date),
    n_urban_legs = nrow(urb), n_alt_levels = n_lev,
    blh_m = blh, blh_src = blh_src,
    encloses_metro = encloses, reason = reason,
    stringsAsFactors = FALSE)
}
res <- do.call(rbind, rows); res <- res[order(res$date), ]
write.csv(res, file.path(OUT_DIR, "closeloop_diagnostic.csv"), row.names = FALSE)

message("\n=== Loop-geometry diagnostic (qualitative; no flux computed) ===")
print(res[, c("flight","n_urban_legs","n_alt_levels","encloses_metro","reason")], row.names = FALSE)
message("\nFlights that encircle the Denver metropolitan box: ", sum(res$encloses_metro),
        " of ", nrow(res), ". No closed-loop flux is reported; the emission rate ",
        "comes from the enhancement-ratio method (script 15).")
