# 53_wind_source_exposure.R ---------------------------------------------------
# Does the urban ethane:methane slope rise when the air over the city has come
# from the DJB gas field, and fall when it has come across the waste-sector
# sources or from the source-poor west?  Section 4.2 answers this at flight level
# with the fraction of samples from the north-east quadrant (r = 0.35, n = 7).
# This script goes one level finer, to the LEG, and replaces the compass quadrant
# with an upwind-exposure score built from the gridded EPA GHGI:
#
#     E_class(leg) = sum_j Q_j * exp(-dtheta_j^2 / (2 sigma^2)) * exp(-d_j / L)
#
# Q_j is the GHGI methane emission of grid cell j (kg/hr) for a source class,
# dtheta_j the angle between the leg's upwind direction (vector-mean aircraft
# wind, blowing FROM) and the bearing from the leg to the cell, d_j the distance,
# sigma = 20 degrees, L = 40 km.  Classes: DJB-type oil and gas (production,
# gathering, processing, transmission), urban natural-gas distribution and
# post-meter, and waste (landfills, wastewater, composting).  Each leg also gets
# theta_DJB, the angle between its upwind direction and the emission-weighted
# centroid of the basin's oil-and-gas methane.
#
# Tests reported: Spearman correlations of the per-leg York slope with theta_DJB
# and with the DJB exposure fraction; DJB-facing (theta <= 45) vs other legs;
# and the altitude x wind pattern (legs above 1 km AGL vs below).
# It REPORTS; nothing published changes.
#
#   Rscript scripts/53_wind_source_exposure.R
#   Out: <OUT_DIR>/wind_source_exposure_legs.csv, wind_source_exposure_summary.csv
#        <OUT_DIR>/figures/FigS11_wind_exposure.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "qc.R"))
suppressPackageStartupMessages(library(ncdf4))

MIN_ENH <- 20; MIN_PTS_LEG <- 10; MIN_PTS_FLT <- 50
SIGMA_DEG <- 20; L_KM <- 40; R_MAX_KM <- 150
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

hav_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180; R <- 6371.0088
  a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}
bearing <- function(lat1, lon1, lat2, lon2) {          # from point 1 TO point 2, deg clockwise from N
  p <- pi / 180; dlon <- (lon2 - lon1) * p
  y <- sin(dlon) * cos(lat2 * p); x <- cos(lat1 * p) * sin(lat2 * p) - sin(lat1 * p) * cos(lat2 * p) * cos(dlon)
  (atan2(y, x) / p + 360) %% 360
}
ang_diff <- function(a, b) abs((a - b + 180) %% 360 - 180)
vmean_wd <- function(wd, ws) {                          # vector-mean direction the wind blows FROM
  ok <- is.finite(wd) & is.finite(ws); if (!any(ok)) return(c(NA, NA))
  u <- -ws[ok] * sin(wd[ok] * pi / 180); v <- -ws[ok] * cos(wd[ok] * pi / 180)
  c((atan2(-mean(u), -mean(v)) * 180 / pi + 360) %% 360, mean(ws[ok]))
}

## ---- gridded GHGI source classes (kg CH4 / hr per 0.1-degree cell) --------------
if (!file.exists(GHGI_FILE)) stop("GHGI file not found: ", GHGI_FILE)
nc <- nc_open(GHGI_FILE)
glat <- ncvar_get(nc, "lat"); glon <- ncvar_get(nc, "lon")
area <- ncvar_get(nc, "grid_cell_area")                    # cm2
ilat <- which(glat > 38.8 & glat < 41.5); ilon <- which(glon > -106.2 & glon < -103.3)
rd <- function(v) { a <- ncvar_get(nc, v); if (length(dim(a)) == 3) a <- a[, , 1]; a[ilon, ilat] }
to_kg_hr <- function(molec_cm2_s) molec_cm2_s * area[ilon, ilat] / 6.022e23 * 16.04 / 1000 * 3600   # area is cm2
CLASSES <- list(
  og_basin = c("emi_ch4_1B2a_Petroleum_Systems_Exploration", "emi_ch4_1B2a_Petroleum_Systems_Production",
               "emi_ch4_1B2b_Natural_Gas_Exploration", "emi_ch4_1B2b_Natural_Gas_Production",
               "emi_ch4_1B2b_Natural_Gas_Processing", "emi_ch4_1B2b_Natural_Gas_TransmissionStorage"),
  ng_dist  = c("emi_ch4_1B2b_Natural_Gas_Distribution", "emi_ch4_Supp_1B2b_PostMeter"),
  waste    = c("emi_ch4_5A1_Landfills_MSW", "emi_ch4_5A1_Landfills_Industrial", "emi_ch4_5B1_Composting",
               "emi_ch4_5D_Wastewater_Treatment_Domestic", "emi_ch4_5D_Wastewater_Treatment_Industrial"),
  livestock = c("emi_ch4_3A_Enteric_Fermentation", "emi_ch4_3B_Manure_Management"))
Q <- lapply(CLASSES, function(vs) Reduce(`+`, lapply(vs, function(v) to_kg_hr(rd(v)))))
nc_close(nc)
cell <- expand.grid(lon = glon[ilon], lat = glat[ilat])
for (k in names(Q)) cell[[k]] <- as.vector(Q[[k]])
# emission-weighted centroid of DJB-type oil-and-gas methane north of the basin threshold
bas <- cell[cell$lat >= 40.05 & cell$og_basin > 0, ]
DJB <- c(lat = weighted.mean(bas$lat, bas$og_basin), lon = weighted.mean(bas$lon, bas$og_basin))
cat(sprintf("DJB oil-and-gas centroid (GHGI-weighted, north of 40.05 N): %.3f N, %.3f W; %.1f t/hr in %d cells\n",
            DJB["lat"], -DJB["lon"], sum(bas$og_basin) / 1000, nrow(bas)))

exposure <- function(lat, lon, wd) {
  d <- hav_km(lat, lon, cell$lat, cell$lon); ok <- d <= R_MAX_KM & d > 0.5
  b <- bearing(lat, lon, cell$lat[ok], cell$lon[ok])
  w <- exp(-ang_diff(wd, b)^2 / (2 * SIGMA_DEG^2)) * exp(-d[ok] / L_KM)
  sapply(names(Q), function(k) sum(cell[[k]][ok] * w))
}

## ---- per-leg slopes, winds and exposures -----------------------------------------
.flights <- list_flights(DATA_DIR)
if (!length(.flights)) stop("no flight files under METHANE_DATA_DIR = ", DATA_DIR)
rows <- list()
for (p in .flights) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb", "C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  lg <- tag_region(leg_metrics(d)); urb <- lg$leg_id[lg$region == "urban"]
  du <- d[d$leg_id %in% urb, ]; if (nrow(du) < MIN_PTS_FLT || length(urb) < 3) next
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  for (g in urb) {
    s <- du[du$leg_id == g, ]
    k <- is.finite(s$CH4_ppb_enh) & is.finite(s$C2H6_ppb_enh) & s$CH4_ppb_enh > MIN_ENH
    if (sum(k) < MIN_PTS_LEG) next
    yk <- york_slope(s$CH4_ppb_enh[k], s$C2H6_ppb_enh[k], 1, 0.2)
    r  <- suppressWarnings(cor(s$CH4_ppb_enh[k], s$C2H6_ppb_enh[k]))
    w  <- vmean_wd(s$WD, s$WS)
    rows[[length(rows) + 1]] <- data.frame(flight = fl, leg_id = g, n_gated = sum(k),
      agl_m = round(mean(s$ALTAGL, na.rm = TRUE)), lat = NA, lon = NA,
      dch4_mean_ppb = round(mean(s$CH4_ppb_enh[k]), 1), slope = round(yk$slope, 5), r = round(r, 2),
      wd_deg = round(w[1]), ws_ms = round(w[2], 1), stringsAsFactors = FALSE)
    # positions: whichever latitude/longitude columns the ICARTT carries
    latc <- grep("^lat", names(s), ignore.case = TRUE, value = TRUE)[1]
    lonc <- grep("^lon", names(s), ignore.case = TRUE, value = TRUE)[1]
    rows[[length(rows)]]$lat <- round(mean(s[[latc]], na.rm = TRUE), 4)
    rows[[length(rows)]]$lon <- round(mean(s[[lonc]], na.rm = TRUE), 4)
  }
}
L <- do.call(rbind, rows)
if (is.null(L) || nrow(L) < 5) stop("fewer than 5 fitted urban legs")
L$bearing_to_djb <- round(bearing(L$lat, L$lon, DJB["lat"], DJB["lon"]))
L$theta_djb <- round(ang_diff(L$wd_deg, L$bearing_to_djb))
E <- t(mapply(exposure, L$lat, L$lon, L$wd_deg))
colnames(E) <- paste0("E_", colnames(E))
L <- cbind(L, round(E, 1))
L$f_og_basin <- round(L$E_og_basin / (L$E_og_basin + L$E_ng_dist + L$E_waste), 3)
L$f_waste    <- round(L$E_waste / (L$E_og_basin + L$E_ng_dist + L$E_waste), 3)
L$fossil_pct <- round(pmin(100, 100 * L$slope / SOURCE_C2H6_CH4))
L$djb_facing <- L$theta_djb <= 45
L$high_leg   <- L$agl_m > 1000
write.csv(L, file.path(OUT_DIR, "wind_source_exposure_legs.csv"), row.names = FALSE)

## ---- tests ---------------------------------------------------------------------
sp <- function(x, y) suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
t_theta <- sp(L$theta_djb, L$slope); t_fog <- sp(L$f_og_basin, L$slope); t_fw <- sp(L$f_waste, L$slope)
t_eog <- sp(log10(L$E_og_basin + 1), L$slope)
grp <- function(m) c(n = sum(m), median_slope = median(L$slope[m]), median_fossil_pct = median(L$fossil_pct[m]),
                     n_pos = sum(L$slope[m] > 0.01))
summ <- rbind(
  data.frame(test = "Spearman rho: slope vs theta_DJB (angle to basin; expect negative)", stat = round(t_theta$estimate, 2), p = signif(t_theta$p.value, 2), n = nrow(L)),
  data.frame(test = "Spearman rho: slope vs DJB exposure fraction f_og_basin", stat = round(t_fog$estimate, 2), p = signif(t_fog$p.value, 2), n = nrow(L)),
  data.frame(test = "Spearman rho: slope vs log10 DJB exposure E_og_basin", stat = round(t_eog$estimate, 2), p = signif(t_eog$p.value, 2), n = nrow(L)),
  data.frame(test = "Spearman rho: slope vs waste exposure fraction f_waste", stat = round(t_fw$estimate, 2), p = signif(t_fw$p.value, 2), n = nrow(L)))
wt <- suppressWarnings(wilcox.test(L$slope[L$djb_facing], L$slope[!L$djb_facing], exact = FALSE))
summ <- rbind(summ, data.frame(test = "Wilcoxon: DJB-facing (theta <= 45) vs other legs, slope", stat = round(wt$statistic), p = signif(wt$p.value, 2), n = nrow(L)))
write.csv(summ, file.path(OUT_DIR, "wind_source_exposure_summary.csv"), row.names = FALSE)

cat("\nper-leg table (", nrow(L), " fitted urban legs):\n", sep = "")
print(L[order(L$theta_djb), c("flight", "leg_id", "n_gated", "agl_m", "wd_deg", "ws_ms", "theta_djb", "f_og_basin", "f_waste", "slope", "r", "fossil_pct")], row.names = FALSE)
cat("\ngroups (median slope / median fossil % at 0.102 / legs with slope > 0.01):\n")
for (nm in c("DJB-facing", "not DJB-facing", "high legs (>1 km AGL)", "low legs", "high & DJB-facing", "low & DJB-facing")) {
  m <- switch(nm, "DJB-facing" = L$djb_facing, "not DJB-facing" = !L$djb_facing, "high legs (>1 km AGL)" = L$high_leg,
              "low legs" = !L$high_leg, "high & DJB-facing" = L$high_leg & L$djb_facing, "low & DJB-facing" = !L$high_leg & L$djb_facing)
  g <- grp(m); cat(sprintf("  %-24s n=%2d  slope %.4f  fossil %3.0f%%  positive %d\n", nm, g["n"], g["median_slope"], g["median_fossil_pct"], g["n_pos"]))
}
cat("\ntests:\n"); print(summ, row.names = FALSE)

## ---- figure ----------------------------------------------------------------------
png(file.path(FIG, "FigS11_wind_exposure.png"), 1500, 700, res = 150)
par(mfrow = c(1, 2), mar = c(4.4, 4.4, 2, 1))
cx <- 0.6 + 1.6 * sqrt(L$n_gated / max(L$n_gated)); pc <- ifelse(L$high_leg, 24, 21)
colf <- colorRampPalette(c("#1a9850", "#fee08b", "#b2182b"))(100)
plot(L$theta_djb, L$slope, pch = pc, cex = cx, bg = colf[pmax(1, pmin(100, round(L$fossil_pct)))],
     xlab = "angle from upwind direction to DJB oil-and-gas centroid (deg)", ylab = "leg ethane:methane slope (mol/mol)", las = 1)
abline(h = 0, col = "gray70"); abline(v = 45, lty = 3, col = "gray50")
legend("topright", bty = "n", cex = 0.75, pch = c(21, 24), legend = c("leg below 1 km AGL", "leg above 1 km AGL"))
title(sprintf("(a) rho = %.2f, p = %.2g, n = %d", t_theta$estimate, t_theta$p.value, nrow(L)), cex.main = 0.9)
plot(L$f_og_basin, L$slope, pch = pc, cex = cx, bg = colf[pmax(1, pmin(100, round(L$fossil_pct)))],
     xlab = "upwind exposure fraction from DJB-type oil and gas (GHGI)", ylab = "leg ethane:methane slope (mol/mol)", las = 1)
abline(h = 0, col = "gray70")
title(sprintf("(b) rho = %.2f, p = %.2g", t_fog$estimate, t_fog$p.value), cex.main = 0.9)
dev.off()
cat("\nwrote wind_source_exposure_legs.csv, wind_source_exposure_summary.csv, FigS11_wind_exposure.png\n")
