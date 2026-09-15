# 59_v2_pointsource_constraint.R ---------------------------------------------
# CAN THE AIRCRAFT CONSTRAIN GRA2PES v2.0beta'S WASTE POINT SOURCES?
#
# v2.0beta places 12.2 t CH4/hr of waste (landfill + wastewater) methane in the
# analysis box, above the whole airborne range (4.6-10.7 t/hr). Its waste field is
# concentrated in a few 4-km cells (the facilities), so it can be tested facility by
# facility: where an urban leg passed downwind of such a cell, the plume crossing that
# leg gives a single-transect mass-balance rate, and its ethane slope splits that rate
# into fossil and biogenic parts. The biogenic part is compared with the v2 waste
# emissions in the leg's upwind fetch (all 4-km cells whose downwind line reaches the
# crossed segment), and the total with the v2 total in the same fetch.
#
# What this is: an order-of-magnitude, per-encounter consistency test, the same
# single-transect method used for facility plumes elsewhere (uniform mixing to the
# lidar mixing height, plume-integrated across the leg, the aircraft wind). What it
# is not: an inversion or a facility emission estimate for publication on its own.
# The main uncertainties are the mixing height (the rate scales with it), the wind
# (perpendicular component), and emissions from cells outside the fetch that reach
# the segment anyway. They are stated as ranges, not hidden.
#
# Inputs: gra2pes_cells_v2.0beta.csv (script 57, hotspots and fetch sums), optionally
#         gra2pes_cells_v1.1.csv, carbonmapper_denver_sources.csv (script 43/44),
#         the aircraft ICARTT files (DATA_DIR) and the lidar velocity statistics
#         (LIDAR_DIR) for the flight mixing height.
# Out:    <OUT_DIR>/v2_pointsource_hotspots.csv      the v2 waste cells tested
#         <OUT_DIR>/v2_pointsource_encounters.csv    one row per (flight, leg, hotspot)
#         <OUT_DIR>/v2_pointsource_summary.csv       per hotspot
#         <OUT_DIR>/figures/FigS14_v2_pointsource.png
# Run:    Rscript scripts/59_v2_pointsource_constraint.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R"))
have_lidar <- file.exists(file.path(proj, "R", "lidar_blh.R")); if (have_lidar) source(file.path(proj, "R", "lidar_blh.R"))
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

WASTE_MIN_T_HR <- 0.25      # a v2 cell with at least this much waste methane is a "hotspot" (facility-scale)
D_MAX_KM  <- 12; ALIGN_DEG <- 25          # same downwind criterion as the Carbon Mapper encounters (script 54)
MIN_PTS   <- 10; MIN_ENH_PPB <- 20; AGL_MAX_M <- 1500
LATERAL_PAD_KM <- 2                       # fetch cells may sit this far outside the crossed segment's lateral span
MW_CH4 <- 16.04
BETA0 <- SOURCE_C2H6_CH4

hav_km <- function(la1, lo1, la2, lo2) { R <- 6371.0088; p <- pi / 180
  a <- sin((la2 - la1) * p / 2)^2 + cos(la1 * p) * cos(la2 * p) * sin((lo2 - lo1) * p / 2)^2
  2 * R * asin(pmin(1, sqrt(a))) }
bearing <- function(la1, lo1, la2, lo2) { p <- pi / 180
  y <- sin((lo2 - lo1) * p) * cos(la2 * p); x <- cos(la1 * p) * sin(la2 * p) - sin(la1 * p) * cos(la2 * p) * cos((lo2 - lo1) * p)
  (atan2(y, x) / p + 360) %% 360 }
ang_diff <- function(a, b) abs((a - b + 180) %% 360 - 180)
# local east/north km relative to a reference point
to_xy <- function(lat, lon, lat0, lon0) cbind(x = (lon - lon0) * 111.32 * cos(lat0 * pi / 180), y = (lat - lat0) * 110.57)

## ---- v2 cells, hotspots -----------------------------------------------------------
gf <- file.path(OUT_DIR, "gra2pes_cells_v2.0beta.csv")
if (!file.exists(gf)) stop("gra2pes_cells_v2.0beta.csv not found; run scripts/57_gra2pes_cells.R on the v2.0beta trees first")
G <- read.csv(gf, stringsAsFactors = FALSE)
G1 <- tryCatch(read.csv(file.path(OUT_DIR, "gra2pes_cells_v1.1.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
hot <- G[G$waste_t_hr >= WASTE_MIN_T_HR, ]; hot <- hot[order(-hot$waste_t_hr), ]
if (!nrow(hot)) stop("no v2 cell carries >= ", WASTE_MIN_T_HR, " t/hr of waste methane; lower WASTE_MIN_T_HR")
LM <- tryCatch(read.csv(file.path(proj, "biogenic_sources.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
# the Tower Road landfill is not in biogenic_sources.csv; its location is the Carbon Mapper source (scripts 44/54)
LM <- rbind(LM[, c("name", "lat", "lon")], data.frame(name = "Tower Road landfill", lat = 39.852, lon = -104.758))
CM <- tryCatch(read.csv(file.path(OUT_DIR, "carbonmapper_denver_sources.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
hot$name <- sprintf("v2 waste cell %.3fN %.3fW", hot$lat, -hot$lon)
hot$nearest_facility <- NA_character_; hot$facility_km <- NA_real_
if (!is.null(LM)) for (i in seq_len(nrow(hot))) { d <- hav_km(hot$lat[i], hot$lon[i], LM$lat, LM$lon)
  hot$nearest_facility[i] <- LM$name[which.min(d)]; hot$facility_km[i] <- round(min(d), 1) }
# the refinery and the Metro Water Recovery plant are 1 km apart; a waste cell nearest the refinery is the complex
hot$nearest_facility <- sub("^Suncor refinery$|^Metro Water Recovery \\(Robert W. Hite\\)$", "Suncor/Metro Water Recovery complex", hot$nearest_facility)
hot$carbonmapper_kg_hr <- NA_real_
if (!is.null(CM)) { cm <- CM[CM$gas == "CH4", ]
  for (i in seq_len(nrow(hot))) { d <- hav_km(hot$lat[i], hot$lon[i], cm$geom_lat, cm$geom_lon)
    if (min(d) < 3.5) hot$carbonmapper_kg_hr[i] <- round(as.numeric(cm$emission_auto[which.min(d)]) * as.numeric(cm$persistence[which.min(d)])) } }
hot$hotspot_id <- seq_len(nrow(hot))
write.csv(hot[, c("hotspot_id", "name", "lat", "lon", "waste_t_hr", "og_t_hr", "postmeter_t_hr", "ch4_total_t_hr", "fossil_frac",
                  "nearest_facility", "facility_km", "carbonmapper_kg_hr")], file.path(OUT_DIR, "v2_pointsource_hotspots.csv"), row.names = FALSE)
cat(sprintf("v2.0beta waste hotspots (>= %.2f t/hr): %d cells holding %.1f of %.1f t/hr box waste\n", WASTE_MIN_T_HR, nrow(hot),
            sum(hot$waste_t_hr), sum(G$waste_t_hr)))
print(hot[, c("hotspot_id", "lat", "lon", "waste_t_hr", "ch4_total_t_hr", "nearest_facility", "facility_km", "carbonmapper_kg_hr")], row.names = FALSE)

## ---- aircraft urban legs -------------------------------------------------------------
.flights <- list_flights(DATA_DIR); if (!length(.flights)) stop("no flight files under METHANE_DATA_DIR")
enc <- list()
for (p in .flights) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb", "C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  lg <- tag_region(leg_metrics(d)); urb <- lg$leg_id[lg$region == "urban"]; if (!length(urb)) next
  du <- d[d$leg_id %in% urb, ]
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  blh <- NA_real_
  if (have_lidar) blh <- tryCatch(blh_flight(as.character(ic$meta$date), min(du$timestamp, na.rm = TRUE),
                                             max(du$timestamp, na.rm = TRUE), LIDAR_DIR)$blh_m, error = function(e) NA_real_)
  for (g in urb) {
    s <- du[du$leg_id == g, ]
    s <- s[is.finite(s$WD) & is.finite(s$WS) & is.finite(s$ALTAGL) & is.finite(s$CH4_ppb_enh) & is.finite(s$C2H6_ppb_enh), ]
    if (nrow(s) < MIN_PTS || mean(s$ALTAGL) > AGL_MAX_M) next
    track <- lg$track_deg[lg$leg_id == g]
    for (h in seq_len(nrow(hot))) {
      dist <- hav_km(s$Latitude, s$Longitude, hot$lat[h], hot$lon[h])
      mis  <- ang_diff(bearing(hot$lat[h], hot$lon[h], s$Latitude, s$Longitude), (s$WD + 180) %% 360)
      link <- dist <= D_MAX_KM & mis <= ALIGN_DEG
      if (sum(link) < MIN_PTS) next
      # the crossed segment: from the first to the last linked sample along the leg (contiguous crossing)
      i0 <- min(which(link)); i1 <- max(which(link)); seg <- s[i0:i1, ]
      if (max(seg$CH4_ppb_enh) < MIN_ENH_PPB) next
      # single-transect flux through the segment: sum nair * dX * U_perp * dy, then x mixing height
      dy <- hav_km(seg$Latitude[-nrow(seg)], seg$Longitude[-nrow(seg)], seg$Latitude[-1], seg$Longitude[-1]) * 1000
      up <- perp_wind(seg$WS, seg$WD, track)
      nair <- air_molar_density(seg$ALTGPS, if ("Temp_True" %in% names(seg)) seg$Temp_True else NA)
      integrand <- nair * pmax(0, seg$CH4_ppb_enh) * 1e-9 * abs(up)
      L <- sum((integrand[-length(integrand)] + integrand[-1]) / 2 * dy, na.rm = TRUE)   # mol m-1 s-1
      H <- if (is.finite(blh)) blh else NA_real_
      Q_t_hr <- L * H * MW_CH4 * 3.6 / 1000                                             # mol/s -> t/hr
      # ethane split of the plume samples
      k <- seg$CH4_ppb_enh > MIN_ENH_PPB
      yk <- if (sum(k) >= MIN_PTS) tryCatch(york_slope(seg$CH4_ppb_enh[k], seg$C2H6_ppb_enh[k], 1, 0.2)$slope, error = function(e) NA_real_) else NA_real_
      f <- if (is.finite(yk)) max(0, min(1, yk / BETA0)) else NA_real_
      # inventory fetch: cells upwind of the segment whose downwind line reaches it
      wd_to <- (mean(seg$WD) + 180) %% 360                    # direction the air moves toward (deg)
      lat0 <- mean(seg$Latitude); lon0 <- mean(seg$Longitude)
      ux <- sin(wd_to * pi / 180); uy <- cos(wd_to * pi / 180)  # unit vector along the flow (east, north)
      sxy <- to_xy(seg$Latitude, seg$Longitude, lat0, lon0)
      lat_seg <- sxy[, 1] * (-uy) + sxy[, 2] * ux             # cross-wind coordinate of the segment samples
      gxy <- to_xy(G$lat, G$lon, lat0, lon0)
      along <- -(gxy[, 1] * ux + gxy[, 2] * uy)                # positive = cell is UPWIND of the segment
      lateral <- gxy[, 1] * (-uy) + gxy[, 2] * ux
      infetch <- along > -2 & along <= D_MAX_KM & lateral >= min(lat_seg) - LATERAL_PAD_KM & lateral <= max(lat_seg) + LATERAL_PAD_KM
      fetch_waste <- sum(G$waste_t_hr[infetch]); fetch_fossil <- sum(G$fossil_t_hr[infetch]); fetch_total <- sum(G$ch4_total_t_hr[infetch])
      fetch_v1 <- if (!is.null(G1)) { g1xy <- to_xy(G1$lat, G1$lon, lat0, lon0)
        a1 <- -(g1xy[, 1] * ux + g1xy[, 2] * uy); l1 <- g1xy[, 1] * (-uy) + g1xy[, 2] * ux
        sum(G1$ch4_total_t_hr[a1 > -2 & a1 <= D_MAX_KM & l1 >= min(lat_seg) - LATERAL_PAD_KM & l1 <= max(lat_seg) + LATERAL_PAD_KM]) } else NA_real_
      enc[[length(enc) + 1]] <- data.frame(flight = fl, leg_id = g, hotspot_id = h, hotspot = hot$nearest_facility[h],
        n_seg = nrow(seg), n_linked = sum(link), seg_km = round(sum(dy) / 1000, 1), dist_km = round(mean(dist[link]), 1),
        agl_m = round(mean(seg$ALTAGL)), blh_m = round(H), wd_deg = round(mean(seg$WD)), ws_ms = round(mean(seg$WS), 1),
        u_perp_ms = round(mean(abs(up)), 1), dch4_max_ppb = round(max(seg$CH4_ppb_enh)),
        L_mol_m_s = signif(L, 3), Q_t_hr = round(Q_t_hr, 2), slope = round(yk, 4), fossil_frac = round(f, 2),
        Q_bio_t_hr = round((1 - f) * Q_t_hr, 2), Q_fossil_t_hr = round(f * Q_t_hr, 2),
        n_fetch_cells = sum(infetch), v2_fetch_waste_t_hr = round(fetch_waste, 2), v2_fetch_fossil_t_hr = round(fetch_fossil, 2),
        v2_fetch_total_t_hr = round(fetch_total, 2), v1_fetch_total_t_hr = round(fetch_v1, 2),
        hotspot_in_fetch = infetch[which(G$i == hot$i[h] & G$j == hot$j[h])], stringsAsFactors = FALSE)
    }
  }
}
if (!length(enc)) stop("no urban leg crossed downwind of a v2 waste hotspot under the criterion")
E <- do.call(rbind, enc)
E$ratio_bio_over_v2waste <- round(E$Q_bio_t_hr / E$v2_fetch_waste_t_hr, 2)
E$ratio_total_over_v2total <- round(E$Q_t_hr / E$v2_fetch_total_t_hr, 2)
E <- E[order(E$hotspot_id, E$flight, E$leg_id), ]
write.csv(E, file.path(OUT_DIR, "v2_pointsource_encounters.csv"), row.names = FALSE)

ok <- is.finite(E$Q_t_hr) & E$hotspot_in_fetch
summ <- do.call(rbind, lapply(split(E[ok, ], E$hotspot_id[ok]), function(s) data.frame(
  hotspot_id = s$hotspot_id[1], hotspot = s$hotspot[1], v2_cell_waste_t_hr = hot$waste_t_hr[s$hotspot_id[1]],
  n_encounters = nrow(s), n_flights = length(unique(s$flight)),
  Q_total_median = median(s$Q_t_hr), Q_total_min = min(s$Q_t_hr), Q_total_max = max(s$Q_t_hr),
  Q_bio_median = median(s$Q_bio_t_hr, na.rm = TRUE), fossil_frac_median = median(s$fossil_frac, na.rm = TRUE),
  v2_fetch_waste_median = median(s$v2_fetch_waste_t_hr), v2_fetch_total_median = median(s$v2_fetch_total_t_hr),
  v1_fetch_total_median = median(s$v1_fetch_total_t_hr),
  ratio_bio_over_v2waste_median = median(s$ratio_bio_over_v2waste, na.rm = TRUE),
  ratio_total_over_v2total_median = median(s$ratio_total_over_v2total, na.rm = TRUE))))
write.csv(summ, file.path(OUT_DIR, "v2_pointsource_summary.csv"), row.names = FALSE)

cat("\nencounters (segment crossed downwind of a v2 waste hotspot; Q from single-transect mass balance to the lidar mixing height):\n")
print(E[, c("flight", "leg_id", "hotspot", "n_seg", "dist_km", "agl_m", "blh_m", "u_perp_ms", "dch4_max_ppb", "Q_t_hr", "fossil_frac",
            "Q_bio_t_hr", "v2_fetch_waste_t_hr", "v2_fetch_total_t_hr", "v1_fetch_total_t_hr", "ratio_bio_over_v2waste", "ratio_total_over_v2total")],
      row.names = FALSE)
cat("\nper hotspot:\n"); print(summ, row.names = FALSE)
cat(sprintf("\nacross all encounters with the hotspot inside the fetch: aircraft biogenic / v2 waste median %.2f (range %.2f to %.2f); aircraft total / v2 total median %.2f\n",
            median(E$ratio_bio_over_v2waste[ok], na.rm = TRUE), min(E$ratio_bio_over_v2waste[ok], na.rm = TRUE), max(E$ratio_bio_over_v2waste[ok], na.rm = TRUE),
            median(E$ratio_total_over_v2total[ok], na.rm = TRUE)))

## ---- figure ---------------------------------------------------------------------------
png(file.path(FIG, "FigS14_v2_pointsource.png"), width = 1900, height = 900, res = 170)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
cols <- c("#1b7837", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666")
pc <- cols[(E$hotspot_id - 1) %% length(cols) + 1]
lim <- range(c(E$Q_bio_t_hr, E$v2_fetch_waste_t_hr, E$Q_t_hr, E$v2_fetch_total_t_hr), na.rm = TRUE); lim <- c(max(0.01, lim[1] * 0.5), lim[2] * 2)
plot(E$v2_fetch_waste_t_hr[ok], pmax(0.01, E$Q_bio_t_hr[ok]), log = "xy", xlim = lim, ylim = lim, pch = 21, bg = pc[ok], cex = 1.3,
     xlab = "GRA2PES v2.0beta waste methane in the upwind fetch (t/hr)", ylab = "aircraft biogenic transect rate (t/hr)",
     main = "A) Biogenic: aircraft vs v2 waste")
abline(0, 1, lty = 2); abline(log10(0.5), 1, lty = 3, col = "grey60"); abline(log10(2), 1, lty = 3, col = "grey60")
legend("topleft", bty = "n", cex = 0.75, pch = 21, pt.bg = cols[seq_len(nrow(hot))], legend = paste0(hot$hotspot_id, ": ", hot$nearest_facility, " (", hot$waste_t_hr |> round(1), " t/hr)"))
plot(E$v2_fetch_total_t_hr[ok], pmax(0.01, E$Q_t_hr[ok]), log = "xy", xlim = lim, ylim = lim, pch = 21, bg = pc[ok], cex = 1.3,
     xlab = "GRA2PES v2.0beta total methane in the upwind fetch (t/hr)", ylab = "aircraft total transect rate (t/hr)",
     main = "B) Total: aircraft vs v2 total")
abline(0, 1, lty = 2); abline(log10(0.5), 1, lty = 3, col = "grey60"); abline(log10(2), 1, lty = 3, col = "grey60")
if (!is.null(G1)) points(E$v1_fetch_total_t_hr[ok], pmax(0.01, E$Q_t_hr[ok]), pch = 4, col = "grey40")
if (!is.null(G1)) legend("bottomright", bty = "n", cex = 0.75, pch = c(21, 4), pt.bg = c("grey80", NA), col = c("black", "grey40"), legend = c("v2.0beta fetch", "v1.1 fetch (same segment)"))
dev.off()
message("wrote v2_pointsource_hotspots.csv, v2_pointsource_encounters.csv, v2_pointsource_summary.csv, figures/FigS14_v2_pointsource.png")
