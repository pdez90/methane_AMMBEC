# 51_footprint_extents.R ------------------------------------------------------
# Pins three geographic numbers that the manuscript quotes by hand:
#
#   1. the area of the seven-county EPA-NEI footprint (Fig 1 caption, section 5),
#      computed from the committed Census boundary in denver7_metro_outline.csv
#      (scripts/make_metro_outline.R; cartographic boundary, 1:500k, land + water);
#   2. the area of the Denver-metro analysis box (config.R URBAN_BOX) on the same
#      sphere, and the footprint-to-box ratio ("about four times");
#   3. the latitude/longitude extent of every flight track and of the urban legs,
#      so "reaching as far south as 39.28 N" (section 4.1) is traceable: that is the
#      southernmost point of the 3 July L1 TRACK; the southernmost URBAN LEG centroid
#      is inside the box.
#
# Areas use the spherical-excess formula on the authalic sphere (R = 6371.0088 km),
# which is within ~0.3% of an ellipsoidal (sf/GEOS) calculation at this latitude.
#
#   Rscript scripts/51_footprint_extents.R
#   Out: <OUT_DIR>/footprint_extents.csv
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))

R_EARTH_KM <- 6371.0088
sph_area_km2 <- function(lon, lat) {
  n <- length(lon); j <- c(2:n, 1)
  a <- sum((lon[j] - lon) * pi / 180 * (2 + sin(lat * pi / 180) + sin(lat[j] * pi / 180)))
  abs(a) * R_EARTH_KM^2 / 2
}

## ---- seven-county outline ---------------------------------------------------
of <- local({
  cand <- c(file.path(INV_DIR, "denver7_metro_outline.csv"),
            file.path(proj, "denver7_metro_outline.csv"),
            normalizePath(file.path(proj, "..", "inventories", "denver7_metro_outline.csv"),
                          mustWork = FALSE),
            file.path(Sys.getenv("HOME"), "MethaneData", "EmissionsInventory",
                      "denver7_metro_outline.csv"))
  hit <- cand[file.exists(cand)]; if (length(hit)) hit[1] else NA_character_
})
if (is.na(of)) stop("denver7_metro_outline.csv not found; run scripts/make_metro_outline.R once.")
oc <- read.csv(of); if (!"part" %in% names(oc)) oc$part <- 1L
county_km2 <- sum(sapply(split(oc, oc$part), function(p) sph_area_km2(p$lon, p$lat)))

## ---- analysis box -----------------------------------------------------------
b <- URBAN_BOX
box_km2 <- sph_area_km2(c(b$lon_w, b$lon_e, b$lon_e, b$lon_w),
                        c(b$lat_s, b$lat_s, b$lat_n, b$lat_n))
box_ns_km <- (b$lat_n - b$lat_s) * pi / 180 * R_EARTH_KM
box_ew_km <- (b$lon_e - b$lon_w) * pi / 180 * R_EARTH_KM * cos((b$lat_n + b$lat_s) / 2 * pi / 180)

## ---- flight-track and urban-leg extents ------------------------------------
mf <- read.csv(file.path(OUT_DIR, "manifest_flights.csv"), stringsAsFactors = FALSE)
mf <- mf[grepl("ARL-Suite", mf$file) & is.finite(mf$lat_min), ]
southmost <- mf[which.min(mf$lat_min), ]
ul <- read.csv(file.path(OUT_DIR, "urban_legs.csv"), stringsAsFactors = FALSE)
ul <- ul[ul$region == "urban", ]

out <- data.frame(
  quantity = c("seven_county_outline_km2", "analysis_box_km2", "county_over_box",
               "box_ns_km", "box_ew_km",
               "track_lat_min", "track_lat_min_flight", "track_lat_max", "track_lon_min", "track_lon_max",
               "urban_leg_lat_min", "urban_leg_lat_max", "urban_leg_lon_min", "urban_leg_lon_max"),
  value = c(round(county_km2), round(box_km2), round(county_km2 / box_km2, 2),
            round(box_ns_km, 1), round(box_ew_km, 1),
            min(mf$lat_min), sub("^AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", southmost$file)),
            max(mf$lat_max), min(mf$lon_min), max(mf$lon_max),
            min(ul$lat), max(ul$lat), min(ul$lon), max(ul$lon)),
  stringsAsFactors = FALSE)
write.csv(out, file.path(OUT_DIR, "footprint_extents.csv"), row.names = FALSE)

cat(sprintf("seven-county NEI footprint : %6.0f km2  (%s)\n", county_km2, basename(of)))
cat(sprintf("analysis box               : %6.0f km2  (%.0f km N-S by %.0f km E-W)\n", box_km2, box_ns_km, box_ew_km))
cat(sprintf("footprint / box            : %6.2f\n", county_km2 / box_km2))
cat(sprintf("flight tracks (ARL suite)  : lat %.3f to %.3f, lon %.3f to %.3f; southernmost %s\n",
            min(mf$lat_min), max(mf$lat_max), min(mf$lon_min), max(mf$lon_max), out$value[7]))
cat(sprintf("urban-leg centroids        : lat %.3f to %.3f, lon %.3f to %.3f (box %.2f to %.2f N)\n",
            min(ul$lat), max(ul$lat), min(ul$lon), max(ul$lon), b$lat_s, b$lat_n))
cat("wrote", file.path(OUT_DIR, "footprint_extents.csv"), "\n")
