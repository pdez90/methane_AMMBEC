# qc.R -----------------------------------------------------------------------
# Quality-control filter for the enhancement-ratio and mass-balance analyses,
# following Schafer, Peischl et al. (2025, ES&T; L.A. Basin). Applied so the
# Denver analysis uses the same data-selection rules as the published method:
#
#   (1) inside the Denver-metro box (URBAN_BOX);
#   (2) within the planetary boundary layer  (ALTAGL < BLH);
#   (3) above the near-source cut            (ALTAGL > agl_min, default 200 m,
#       to avoid contamination during low approaches to airfields);
#   (4) daytime, well-mixed hours            (day_start-day_end LOCAL, default
#       10:00-17:00; Denver is MDT = UTC-6 in the campaign period).
#
# In the AMMBEC urban flights these filters are mild — the flights are already
# daytime and only ~4-12% of in-box samples are below 200 m AGL. The largest
# per-flight change in the York fossil fraction is 12 percentage points
# (2024-07-13 L2; results/qc_robustness.csv), with the flight ranking preserved,
# so the biogenic-leaning result is robust to them. Adopting them keeps the
# method consistent with the L.A. study.
#
# @param df    tidy ICARTT data.frame (from read_icartt), with a POSIXct
#              `timestamp`, Latitude, Longitude and ALTAGL columns.
# @param blh   boundary-layer height for this flight (m); pass the lidar value
#              (see lidar_blh.R). If NULL, the PBL filter (2) is skipped.
# @param box   named list lat_s/lat_n/lon_w/lon_e (default URBAN_BOX).
# @return the filtered data.frame (attributes record how many rows each rule cut).
# -----------------------------------------------------------------------------
qc_filter <- function(df, blh = NULL, box = URBAN_BOX,
                      agl_min = 200, day_start = 10, day_end = 17,
                      utc_off_hours = -6,
                      lat = "Latitude", lon = "Longitude", agl = "ALTAGL",
                      time_col = "timestamp") {
  n0 <- nrow(df)
  keep <- rep(TRUE, n0)

  # (1) spatial box
  if (all(c(lat, lon) %in% names(df)))
    keep <- keep & df[[lat]] >= box$lat_s & df[[lat]] <= box$lat_n &
                   df[[lon]] >= box$lon_w & df[[lon]] <= box$lon_e

  # (3) near-source floor and (2) PBL ceiling
  if (is.list(blh)) blh <- if (!is.null(blh$blh_m)) blh$blh_m else blh[[1]]  # accept blh_flight() output
  blh <- suppressWarnings(as.numeric(blh))[1]
  if (agl %in% names(df)) {
    keep <- keep & is.finite(df[[agl]]) & df[[agl]] > agl_min
    if (length(blh) && is.finite(blh)) keep <- keep & df[[agl]] < blh
  }

  # (4) daytime local hours
  if (time_col %in% names(df)) {
    secs <- as.numeric(df[[time_col]])                       # UTC seconds
    localh <- ((secs + utc_off_hours * 3600) %% 86400) / 3600
    keep <- keep & localh >= day_start & localh < day_end
  }

  out <- df[keep & !is.na(keep), , drop = FALSE]
  attr(out, "qc") <- list(n_in = n0, n_out = nrow(out),
                          agl_min = agl_min, blh = blh,
                          day = c(day_start, day_end), utc_off = utc_off_hours)
  out
}
