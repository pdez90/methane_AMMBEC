# 11_urban_flux.R ------------------------------------------------------------
# Urban-leg CLASSIFICATION and exploratory curtain diagnostics. This isolates the
# Denver-metropolitan-area flight legs (inside the metro box) from the DJB-basin
# legs, and reports leg counts used downstream. It ALSO computes a per-flight
# curtain "flux" as a diagnostic only (column urban_curtain_diag_t_hr): the urban
# legs are not a validated downwind curtain, so NO city-wide flux is inferred from
# them. The manuscript's urban emission rate comes from the enhancement-ratio
# method (scripts 15/21); only the leg COUNTS from this file feed downstream.
#
# Denver and the DJB are adjacent, so legs are classified by location (see
# tag_region() / URBAN_BOX in config.R), NOT by whole-flight bounding box.
#
# Run:  Rscript scripts/11_urban_flux.R [optional: BLH_m]
# Out:  <OUT_DIR>/urban_flux.csv (leg counts + curtain DIAGNOSTIC), urban_legs.csv,
#       <OUT_DIR>/figures/urban_legs_map.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R"))
source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R"))
source(file.path(proj, "R", "ratios.R"))
source(file.path(proj, "R", "enhancements.R"))
wp <- NULL
if (exists("WINDPROF_FILE") && file.exists(WINDPROF_FILE)) {
  source(file.path(proj, "R", "read_windprof.R"))
  wp <- tryCatch(read_windprof(WINDPROF_FILE), error = function(e) NULL)
}

args <- commandArgs(trailingOnly = TRUE)
blh <- if (length(args)) suppressWarnings(as.numeric(args[1])) else NA
if (is.na(blh)) blh <- 2000

flux_rows <- list(); leg_rows <- list()
for (p in list_flights(DATA_DIR)) {
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb","C2H6_ppb","WS","WD","TrackAngleTrue","ALTGPS") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data)
  if (!any(d$leg_id > 0)) next
  legs <- leg_metrics(d, windprof = wp)
  if (is.null(legs) || !nrow(legs)) next
  legs <- tag_region(legs, box = URBAN_BOX, basin_lat = BASIN_LAT)
  legs$flight <- basename(p); leg_rows[[p]] <- legs

  urb <- legs[legs$region == "urban", ]
  bas <- legs[legs$region == "basin", ]
  fx_u <- if (nrow(urb)) curtain_flux(urb, blh_m = blh) else NULL
  fx_b <- if (nrow(bas)) curtain_flux(bas, blh_m = blh) else NULL

  # ethane:methane on the urban legs only (fossil vs biogenic of the urban flux)
  du <- d[d$leg_id %in% urb$leg_id, ]
  em <- NA; ff <- NA
  if (nrow(du) > 50) {
    du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
    fit <- ethane_methane_ratio(du, "CH4_ppb", "C2H6_ppb", min_enh = 20)
    em <- round(fit$slope, 4); ff <- round(fossil_fraction(fit$slope, SOURCE_C2H6_CH4), 2)
  }

  flux_rows[[p]] <- data.frame(
    flight = basename(p), date = as.character(ic$meta$date),
    n_urban_legs = nrow(urb), n_edge_legs = sum(legs$region=="edge"),
    n_basin_legs = nrow(bas),
    urban_lat_min = if (nrow(urb)) min(urb$lat) else NA,
    urban_curtain_diag_t_hr = if (!is.null(fx_u)) round(fx_u$flux_t_hr, 3) else NA,
    urban_c2h6_ch4 = em, urban_fossil_frac = ff,
    basin_flux_t_hr = if (!is.null(fx_b)) round(fx_b$flux_t_hr, 3) else NA,
    blh_m = blh, stringsAsFactors = FALSE)
}

flux <- do.call(rbind, flux_rows)
flux <- flux[order(flux$date), ]
write.csv(flux, file.path(OUT_DIR, "urban_flux.csv"), row.names = FALSE)
alllegs <- do.call(rbind, leg_rows)
write.csv(alllegs, file.path(OUT_DIR, "urban_legs.csv"), row.names = FALSE)

# ---- map: all legs colored by region ----
png(file.path(OUT_DIR, "figures", "urban_legs_map.png"), 760, 780, res = 120)
col <- c(urban="firebrick", edge="gray60", basin="steelblue")[alllegs$region]
plot(alllegs$lon, alllegs$lat, col = col, pch = 19, cex = 0.7, asp = 1,
     xlab = "Longitude", ylab = "Latitude", main = "Flight legs: urban (metro) vs DJB basin")
rect(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n, border = "firebrick", lty = 2)
abline(h = BASIN_LAT, col = "steelblue", lty = 3)
legend("topright", c("urban leg","edge","basin leg","metro box"),
       col = c("firebrick","gray60","steelblue","firebrick"),
       pch = c(19,19,19,NA), lty = c(NA,NA,NA,2), bty = "n", cex = .8)
dev.off()

uf <- flux[is.finite(flux$urban_curtain_diag_t_hr), ]
message("Flights with urban legs: ", nrow(uf), " of ", nrow(flux), ".")
# NB: urban_curtain_diag_t_hr is a DIAGNOSTIC only. The urban-classified legs are
# not a validated downwind curtain (they may run in different directions, sit up-
# and down-wind, or belong to separate loop segments), so these values are NOT a
# defensible city flux and are not used in the manuscript. Only the leg COUNTS
# from this file feed downstream; the emission rate comes from the enhancement-
# ratio method (script 15) and the validated closed-loop screen (script 13).
if (nrow(uf)) message("Urban curtain DIAGNOSTIC (not a validated flux): ",
        round(min(uf$urban_curtain_diag_t_hr),2), "-",
        round(max(uf$urban_curtain_diag_t_hr),2), " t/hr; urban fossil frac median ",
        round(stats::median(uf$urban_fossil_frac, na.rm=TRUE),2), ".")
message("Wrote urban_flux.csv, urban_legs.csv, urban_legs_map.png.")
print(flux[, c("flight","date","n_urban_legs","urban_curtain_diag_t_hr","urban_fossil_frac","n_basin_legs")], row.names = FALSE)
