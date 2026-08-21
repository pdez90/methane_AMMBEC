# 24_facility_separation_scan.R ----------------------------------------------
# Scans every flight to find any day whose wind lets the aircraft SEPARATE the
# two co-located sources by the Suncor refinery: the refinery itself (fossil,
# ethane-rich) and the Robert W. Hite / Metro Water Recovery wastewater plant
# (biogenic, ethane-poor), which sits about 1.1 km SOUTH of the refinery.
#
# The two plumes are separable only when the wind blows ACROSS the line joining
# them, i.e. an easterly or westerly wind. Then the refinery and the plant lie
# side by side across the wind and produce two distinct cross-wind plumes. When
# the wind is northerly or southerly the two stack front to back, their plumes
# merge, and the aircraft cannot tell them apart (this is the 9 July case).
#
# For each flight the scan reports: how many in-plume points fall near the
# complex, the median wind there, and the cross-wind separation of the two
# facilities under that wind. A flight is flagged SEPARABLE when the cross-wind
# separation clears a threshold and there are enough plume points. For every
# separable flight it then fits the ethane:methane slope in the refinery-upwind
# sector and in the plant-upwind sector, so a genuinely fossil vs biogenic
# contrast between the two facilities can be read off directly.
#
# Out: <OUT_DIR>/facility_separation_scan.csv   (one row per flight near complex)
#      <OUT_DIR>/facility_sector_fits.csv        (sector fits for separable flights)
# Run: Rscript scripts/24_facility_separation_scan.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "ratios.R"))

## ---- facility geometry and scan parameters ---------------------------------
SUNCOR <- c(lat = 39.802, lon = -104.939)   # Commerce City refinery  (fossil)
HITE   <- c(lat = 39.792, lon = -104.938)   # Robert W. Hite WWTP      (biogenic)
COMPLEX_R_KM   <- 4      # sample must be within this radius of the complex
ENH_MIN_PPB    <- 20     # CH4 enhancement above background to count as in-plume
AGL_MIN_M      <- 150    # near-source facility work: keep low passes, drop the ground
AGL_MAX_M      <- 1500   # stay in the boundary layer
CROSS_SEP_MIN_M<- 600    # cross-wind facility separation needed to resolve them
N_PLUME_MIN    <- 20     # minimum near-complex plume points for a usable day
SECTOR_HALFWID <- 500    # plume half-width for the upwind-sector assignment (m)
SECTOR_MAXDOWN <- 4000   # max downwind distance to attribute a point (m)
MPERDEG_LAT <- 111000; MPERDEG_LON <- 85300   # metres per degree at ~39.8 N

## metres east/north of a point relative to a facility
enu <- function(lat, lon, f) list(e = (lon - f["lon"]) * MPERDEG_LON,
                                   n = (lat - f["lat"]) * MPERDEG_LAT)

## along/cross-wind split of a displacement, given met wind direction (deg FROM)
alongcross <- function(e, n, wd_from) {
  th <- (wd_from + 180) * pi / 180        # direction the plume TRAVELS (compass)
  ue <- sin(th); un <- cos(th)            # unit vector of travel
  list(along = e * ue + n * un, cross = -e * un + n * ue)
}

scan_rows <- list(); fit_rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL); if (is.null(ic)) next
  d <- ic$data
  need <- c("Latitude","Longitude","CH4_ppb","C2H6_ppb","WD","WS")
  if (!all(need %in% names(d))) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub(".ict", "", basename(p)))

  # distance to the complex centre (mean of the two facilities)
  cen <- c(lat = mean(c(SUNCOR["lat"], HITE["lat"])), lon = mean(c(SUNCOR["lon"], HITE["lon"])))
  dkm <- sqrt(((d$Latitude - cen["lat"]) * 111)^2 + ((d$Longitude - cen["lon"]) * 85.3)^2)

  agl <- if ("ALTAGL" %in% names(d)) d$ALTAGL else rep(NA_real_, nrow(d))
  ch4bg <- stats::quantile(d$CH4_ppb, 0.05, na.rm = TRUE)
  c2bg  <- stats::quantile(d$C2H6_ppb, 0.05, na.rm = TRUE)
  xh <- d$CH4_ppb - ch4bg; xe <- d$C2H6_ppb - c2bg
  altok <- is.na(agl) | (agl > AGL_MIN_M & agl < AGL_MAX_M)
  keep <- dkm < COMPLEX_R_KM & is.finite(xh) & xh > ENH_MIN_PPB &
          is.finite(xe) & is.finite(d$WD) & altok
  n_plume <- sum(keep)
  if (n_plume < 1) next                                   # this flight never sampled the complex

  wd_med <- stats::median(d$WD[keep], na.rm = TRUE)
  ws_med <- stats::median(d$WS[keep], na.rm = TRUE)
  # cross-wind separation of the two facilities under the median wind
  off <- enu(HITE["lat"], HITE["lon"], SUNCOR)           # Suncor -> Hite vector
  ac  <- alongcross(off$e, off$n, wd_med)
  cross_sep <- abs(ac$cross); along_sep <- abs(ac$along)
  separable <- is.finite(cross_sep) && cross_sep >= CROSS_SEP_MIN_M && n_plume >= N_PLUME_MIN

  scan_rows[[fl]] <- data.frame(
    flight = fl, n_plume_near = n_plume,
    wd_med = round(wd_med), ws_med = round(ws_med, 1),
    cross_sep_m = round(cross_sep), along_sep_m = round(along_sep),
    separable = separable, stringsAsFactors = FALSE)

  if (!separable) next

  ## ---- separable day: fit the two upwind sectors --------------------------
  idx <- which(keep)
  in_sector <- function(i, f) {
    o <- enu(d$Latitude[i], d$Longitude[i], f)
    a <- alongcross(o$e, o$n, d$WD[i])
    a$along > 0 & a$along < SECTOR_MAXDOWN & abs(a$cross) < SECTOR_HALFWID
  }
  inS <- vapply(idx, in_sector, logical(1), f = SUNCOR)
  inH <- vapply(idx, in_sector, logical(1), f = HITE)
  # Require a meaningful sample: at least N_SECTOR_MIN points AND at least two
  # temporally separated plume intercepts (a >60 s gap starts a new intercept),
  # because york_slope() itself needs >= 10 points and a single ~20 s pass is not an
  # independent facility measurement.
  N_SECTOR_MIN <- 20L
  n_intercepts <- function(ii) {
    if (!length(ii) || !("timestamp" %in% names(d))) return(if (length(ii)) 1L else 0L)
    ts <- sort(as.numeric(d$timestamp[ii])); 1L + sum(diff(ts) > 60)
  }
  fit_sector <- function(sel, who) {
    ii <- idx[sel]; n_int <- n_intercepts(ii)
    if (length(ii) < N_SECTOR_MIN || n_int < 2)
      return(data.frame(flight = fl, sector = who, n = length(ii), n_intercepts = n_int,
        slope = NA, r = NA, fossil_pct = NA))
    x <- xh[ii]; y <- xe[ii]; s <- york_slope(x, y, 1, 0.2)$slope
    data.frame(flight = fl, sector = who, n = length(ii), n_intercepts = n_int,
      slope = round(s, 3), r = round(suppressWarnings(cor(x, y)), 2),
      fossil_pct = round(100 * fossil_fraction(s, SOURCE_C2H6_CH4)))
  }
  fit_rows[[paste0(fl,"_S")]] <- fit_sector(inS & !inH, "Suncor_refinery_fossil")
  fit_rows[[paste0(fl,"_H")]] <- fit_sector(inH & !inS, "RobertHite_WWTP_biogenic")
}

scan <- if (length(scan_rows)) do.call(rbind, scan_rows) else
  data.frame(flight=character(), n_plume_near=integer(), wd_med=numeric(),
             ws_med=numeric(), cross_sep_m=numeric(), along_sep_m=numeric(),
             separable=logical())
scan <- scan[order(-scan$cross_sep_m), ]
write.csv(scan, file.path(OUT_DIR, "facility_separation_scan.csv"), row.names = FALSE)

fits <- if (length(fit_rows)) do.call(rbind, fit_rows) else NULL
if (!is.null(fits)) write.csv(fits, file.path(OUT_DIR, "facility_sector_fits.csv"), row.names = FALSE)

## ---- report ----------------------------------------------------------------
message("Wind convention: WD is degrees the wind comes FROM. Cross-wind facility ",
        "separation is large for EASTERLY/WESTERLY winds (~90 or 270), small for ",
        "NORTHERLY/SOUTHERLY winds (the two plumes then stack and merge).")
message("\n== Flights that sampled near the Suncor / Robert Hite complex ==")
print(scan, row.names = FALSE)
nsep <- sum(scan$separable)
if (nsep == 0) {
  message("\nNo flight has both a cross-wind (easterly/westerly) wind over the complex ",
          "and enough plume points. The two facilities cannot be separated from the ",
          "aircraft with this campaign; the blended fossil-vs-biogenic split over the ",
          "complex is the most the flights support. The mobile ground survey, which can ",
          "drive between the two facilities, is the route to a per-facility answer.")
} else {
  message(sprintf("\n%d flight(s) have a separating wind. Sector fits ",
                  nsep), sprintf("(pure fossil slope ~%.3f, pure biogenic ~0):", SOURCE_C2H6_CH4))
  print(fits, row.names = FALSE)
  message("\nRead-off (EXPLORATORY, not confirmatory): a higher ethane:methane slope ",
          "in the Suncor sector is consistent with a thermogenic refinery source; a ",
          "near-zero slope in the Robert Hite sector would be consistent with a ",
          "biogenic plant. Idealized straight-line sector assignment, non-independent ",
          "1 Hz samples, and few intercepts mean these are suggestive, not confirmed.")
}
message("\nWrote facility_separation_scan.csv",
        if (!is.null(fits)) " and facility_sector_fits.csv" else "")
