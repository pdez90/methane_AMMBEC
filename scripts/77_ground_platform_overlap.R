# 77_ground_platform_overlap.R ------------------------------------------------
# Where and when the two AMMBEC ground platforms sampled the same roads on 9 July 2024:
# the NOAA CSL Mobile Laboratory (no ethane; scripts 07) and the NOAA ARL Air Resources
# Car (CH4 + C2H6; script 71). They are different vehicles; this quantifies how much of
# the CSL transect the ARC also drove, how far behind it was, and whether they were ever
# side by side.
#
# A CSL second counts as "shared" when an ARC sample lies within SHARE_M of it and within
# SHARE_S of it in time. The time offset reported is ARC minus CSL at the nearest such
# sample, so a positive offset means the car passed the spot after the laboratory.
#
# Run:  Rscript scripts/77_ground_platform_overlap.R
# Out:  <OUT_DIR>/ground_platform_overlap.csv (quantity, value)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R"))

DAY <- "20240709"
SHARE_M <- 150; SHARE_S <- 1800; SIDE_M <- 500; SIDE_S <- 60; METRO_LAT <- 39.80

# ---- CSL Mobile Laboratory position ---------------------------------------------------
mfile <- list.files(MOBILELAB_DIR, pattern = paste0("MetNav_MobileLab_", DAY, ".*\\.ict$"), full.names = TRUE)
if (!length(mfile)) stop("no CSL MetNav file for ", DAY, " in ", MOBILELAB_DIR)
m <- read_icartt(mfile[1])$data
C <- data.frame(t = as.numeric(m[[1]]), lat = m$GPS_Lat, lon = m$GPS_Lon)
C <- C[is.finite(C$lat) & is.finite(C$lon) & C$lat != 0, ]

# ---- ARL ARC position (same directory resolution as script 71) ----------------------
ARC_DIR <- Sys.getenv("METHANE_ARC_DIR", unset = "")
if (!nzchar(ARC_DIR)) for (.c in c(file.path(DATA_DIR, "ARC"), file.path(dirname(DATA_DIR), "instruments", "ARC")))
  if (dir.exists(.c)) { ARC_DIR <- .c; break }
af <- list.files(ARC_DIR, pattern = paste0(DAY, ".*\\.(ict|zip)$"), full.names = TRUE)
if (!length(af)) stop("no ARC file for ", DAY, " in ", ARC_DIR)
if (grepl("\\.zip$", af[1])) {
  td <- file.path(tempdir(), "arc77"); dir.create(td, showWarnings = FALSE)
  inner <- utils::unzip(af[1], list = TRUE)$Name; inner <- inner[grepl("\\.ict$", inner)][1]
  utils::unzip(af[1], files = inner, exdir = td, junkpaths = TRUE); af <- file.path(td, basename(inner))
}
a <- read_icartt(af[1])$data
col <- function(d, p) d[[grep(p, names(d), ignore.case = TRUE)[1]]]
R <- data.frame(t = as.numeric(a[[1]]), lat = col(a, "^lat"), lon = col(a, "^lon"))
R <- R[is.finite(R$lat) & is.finite(R$lon) & R$lat != 0, ]
R <- R[order(R$t), ]

dist_m <- function(la1, lo1, la2, lo2)
  sqrt(((lo2 - lo1) * cos((la1 + la2) / 2 * pi / 180) * 111320)^2 + ((la2 - la1) * 110570)^2)

# For every CSL second: nearest ARC sample (in space) within +/- SHARE_S, and the nearest
# ARC sample within +/- SIDE_S (side by side).
shared <- rep(FALSE, nrow(C)); lag <- rep(NA_real_, nrow(C)); side <- rep(Inf, nrow(C))
for (k in seq_len(nrow(C))) {
  i0 <- findInterval(C$t[k] - SHARE_S, R$t) + 1; i1 <- findInterval(C$t[k] + SHARE_S, R$t)
  if (i1 < i0) next
  idx <- i0:i1; d <- dist_m(C$lat[k], C$lon[k], R$lat[idx], R$lon[idx])
  j <- which.min(d)
  if (d[j] <= SHARE_M) { shared[k] <- TRUE; lag[k] <- R$t[idx[j]] - C$t[k] }
  w <- abs(R$t[idx] - C$t[k]) <= SIDE_S
  if (any(w)) side[k] <- min(d[w])
}
# ARC metro points (south of METRO_LAT, inside the analysis box) that lie on the CSL route
in_box <- R$lat >= URBAN_BOX$lat_s & R$lat <= URBAN_BOX$lat_n & R$lon >= URBAN_BOX$lon_w & R$lon <= URBAN_BOX$lon_e
metro <- which(in_box & R$lat < METRO_LAT)
on_route <- vapply(metro, function(i) {
  w <- abs(C$t - R$t[i]) <= SHARE_S
  any(w) && min(dist_m(R$lat[i], R$lon[i], C$lat[w], C$lon[w])) <= SHARE_M }, TRUE)

hm <- function(s) paste0(sprintf("%02d:%02d", as.integer((s %/% 3600) %% 24), as.integer((s %% 3600) %/% 60)),
                          ifelse(s >= 86400, " (next day)", ""))
q <- stats::quantile(lag[shared], c(0.25, 0.5, 0.75), names = FALSE)
OUT <- data.frame(quantity = c(
  "date", "csl_window_utc", "arc_window_utc", "csl_gps_seconds", "csl_lat_range", "arc_lat_range",
  "csl_seconds_shared", "csl_share_shared_pct", "shared_window_utc", "shared_lat_range",
  "lag_arc_minus_csl_median_s", "lag_iqr_s", "min_separation_same_minute_m",
  "arc_metro_seconds", "arc_metro_seconds_on_csl_route", "criteria"),
  value = c(DAY, paste(hm(min(C$t)), "to", hm(max(C$t))), paste(hm(min(R$t)), "to", hm(max(R$t))), nrow(C),
            sprintf("%.2f to %.2f", min(C$lat), max(C$lat)), sprintf("%.2f to %.2f", min(R$lat), max(R$lat)),
            sum(shared), round(100 * mean(shared)),
            paste(hm(min(C$t[shared])), "to", hm(max(C$t[shared]))),
            sprintf("%.2f to %.2f", min(C$lat[shared]), max(C$lat[shared])),
            round(q[2]), sprintf("%.0f to %.0f", q[1], q[3]), round(min(side)),
            length(metro), sum(on_route),
            sprintf("shared = ARC within %d m and %d s; side by side = within %d s", SHARE_M, SHARE_S, SIDE_S)),
  stringsAsFactors = FALSE)
write.csv(OUT, file.path(OUT_DIR, "ground_platform_overlap.csv"), row.names = FALSE)
print(OUT, row.names = FALSE)
