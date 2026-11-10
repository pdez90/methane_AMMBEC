# 113_downwind_leg_0709.R -----------------------------------------------------
# The north-south transect flown east of Denver on 9 July 2024 (about 104.39 W;
# 0.22-0.71 km above ground at 17:51-18:17 UTC and again at 2.05-2.58 km at 18:30-18:52),
# suggested by S. Baidar as a leg downwind of the city. It lies outside the analysis
# box (east edge 104.55 W), so it is not part of Table 1 and can serve as a check.
#
#   1. Ethane against methane on the leg with ONE fixed background per species (the
#      1st percentile of the whole flight record), by latitude band: York slope,
#      intercept, r, and the zero-intercept ratio sum(dC2H6)/sum(dCH4); the measured
#      wind; the CH4:CO correlation.
#   2. Where the air came from: HRRR-STILT footprints (settings and functions taken
#      from script 64 itself) for receptors spread along the leg, convolved with the
#      gridded EPA GHGI. For each receptor the predicted enhancement is split by
#      source class and by region: cells inside the Denver-metro box, cells north of
#      40.05 N (the basin), and elsewhere.
#
# Out: <OUT_DIR>/downwind_leg_0709_passes.csv, downwind_leg_0709_fits.csv,
#      downwind_leg_0709_stilt.csv, downwind_leg_0709_centroids.csv,
#      figures/FigS_downwind_leg_0709.png (SI Figure S19), stilt_runs_downwind_0709/ (resume cache)
# Run: Rscript scripts/113_downwind_leg_0709.R             (needs the HYSPLIT/HRRR set-up
#      of script 64). With METHANE_DOWNWIND_NO_STILT=1 (as run_all.R sets it) only part 1 is
#      computed and the stored STILT results (output folder, else results/) are reused for
#      the figure.
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths", "massbalance", "enhancements", "ratios", "airborne_blh"))
  source(file.path(proj, "R", paste0(f, ".R")))
FLIGHT <- "20240709_R0"; b <- URBAN_BOX; BASIN_LAT <- 40.05
NO_STILT <- Sys.getenv("METHANE_DOWNWIND_NO_STILT", "0") == "1"

p <- grep(paste0("ARL-Suite.*", FLIGHT), list_flights(DATA_DIR), value = TRUE)[1]
ic <- read_icartt(p); d <- detect_level_legs(ic$data)
# the leg: constant-altitude transects east of the box that run north-south over more than 0.2 degrees of latitude
ids <- sort(unique(d$leg_id[d$leg_id > 0])); keep <- integer(0)
for (g in ids) { s <- d[d$leg_id == g, ]
  if (stats::median(s$Longitude) > b$lon_e + 0.1 && diff(range(s$Latitude)) > 0.2) keep <- c(keep, g) }
L <- d[d$leg_id %in% keep, ]; L$level <- ifelse(L$ALTAGL < 1000, "low", "high")
bg <- c(CH4 = stats::quantile(d$CH4_ppb, 0.01, na.rm = TRUE, names = FALSE), C2H6 = stats::quantile(d$C2H6_ppb, 0.01, na.rm = TRUE, names = FALSE),
        CO = stats::quantile(d$CO_ppb, 0.01, na.rm = TRUE, names = FALSE))
L$dch4 <- L$CH4_ppb - bg["CH4"]; L$dc2h6 <- L$C2H6_ppb - bg["C2H6"]; L$dco <- L$CO_ppb - bg["CO"]
message(sprintf("%s: transects %s at %.2f W; low %d samples (%.0f m AGL, %s-%s UTC), high %d samples (%.0f m AGL); fixed background CH4 %.1f ppb, C2H6 %.2f ppb, CO %.1f ppb",
  FLIGHT, paste(keep, collapse = ","), -stats::median(L$Longitude), sum(L$level == "low"), stats::median(L$ALTAGL[L$level == "low"]),
  format(min(L$timestamp[L$level == "low"]), "%H:%M"), format(max(L$timestamp[L$level == "low"]), "%H:%M"), sum(L$level == "high"), stats::median(L$ALTAGL[L$level == "high"]), bg["CH4"], bg["C2H6"], bg["CO"]))
PS <- do.call(rbind, lapply(c("low", "high"), function(lv) { s <- L[L$level == lv, ]
  data.frame(level = lv, n = nrow(s), start_utc = format(min(s$timestamp), "%H:%M"), end_utc = format(max(s$timestamp), "%H:%M"), lon_median = round(stats::median(s$Longitude), 2),
             lat_min = round(min(s$Latitude), 2), lat_max = round(max(s$Latitude), 2), agl_min_m = round(min(s$ALTAGL, na.rm = TRUE)), agl_median_m = round(stats::median(s$ALTAGL, na.rm = TRUE)),
             agl_max_m = round(max(s$ALTAGL, na.rm = TRUE)), blh_airborne_m = round(stats::median(airborne_blh_at(FLIGHT, s$timestamp), na.rm = TRUE)),
             km_east_of_box = round((stats::median(s$Longitude) - b$lon_e) * 111.32 * cos(mean(c(b$lat_s, b$lat_n)) * pi / 180), 1), stringsAsFactors = FALSE) }))
write.csv(PS, file.path(OUT_DIR, "downwind_leg_0709_passes.csv"), row.names = FALSE)

## ---- 1. fits by latitude band -----------------------------------------------------------
mid <- (b$lat_s + b$lat_n) / 2
bands <- list(`box latitudes (39.50-39.95 N)` = c(b$lat_s, b$lat_n), `south half of box latitudes` = c(b$lat_s, mid), `north half of box latitudes` = c(mid, b$lat_n),
              `39.95-40.05 N` = c(b$lat_n, BASIN_LAT), `basin latitudes (north of 40.05 N)` = c(BASIN_LAT, 90), `whole leg` = c(-90, 90))
vw <- function(s) { u <- mean(-s$WS * sin(s$WD * pi / 180), na.rm = TRUE); v <- mean(-s$WS * cos(s$WD * pi / 180), na.rm = TRUE); c((atan2(-u, -v) * 180 / pi) %% 360, sqrt(u^2 + v^2)) }
fits <- list()
for (lv in c("low", "high")) for (nm in names(bands)) {
  s <- L[L$level == lv & L$Latitude >= bands[[nm]][1] & L$Latitude < bands[[nm]][2] & is.finite(L$dch4) & is.finite(L$dc2h6), ]
  if (nrow(s) < 30) next
  yk <- york_slope(s$dch4, s$dc2h6, 1, 0.2); w <- vw(s); blh <- airborne_blh_at(FLIGHT, s$timestamp)
  fits[[paste(lv, nm)]] <- data.frame(level = lv, band = nm, n = nrow(s), agl_m = round(stats::median(s$ALTAGL)), blh_airborne_m = round(stats::median(blh, na.rm = TRUE)),
    dch4_median_ppb = round(stats::median(s$dch4), 1), dch4_max_ppb = round(max(s$dch4), 1), slope_york = signif(yk$slope, 4), intercept_ppb = round(yk$intercept, 2),
    r = round(stats::cor(s$dch4, s$dc2h6), 2), ratio_zero_intercept = signif(sum(s$dc2h6) / sum(s$dch4), 4),
    fossil_pct_slope = round(100 * fossil_fraction(yk$slope, SOURCE_C2H6_CH4)), fossil_pct_ratio = round(100 * fossil_fraction(sum(s$dc2h6) / sum(s$dch4), SOURCE_C2H6_CH4)),
    r_ch4_co = round(stats::cor(s$dch4, s$dco, use = "complete.obs"), 2), dco_median_ppb = round(stats::median(s$dco, na.rm = TRUE), 1),
    wind_from_deg = round(w[1]), wind_vector_ms = round(w[2], 1), stringsAsFactors = FALSE)
}
F <- do.call(rbind, fits); write.csv(F, file.path(OUT_DIR, "downwind_leg_0709_fits.csv"), row.names = FALSE)
print(F[, c("level", "band", "n", "agl_m", "dch4_median_ppb", "slope_york", "intercept_ppb", "r", "ratio_zero_intercept", "fossil_pct_slope", "fossil_pct_ratio", "r_ch4_co", "wind_from_deg", "wind_vector_ms")], row.names = FALSE)

## ---- 2. HRRR-STILT: where the air on the leg came from ---------------------------------
S <- NULL
if (!NO_STILT) {
  suppressPackageStartupMessages({ library(ncdf4); library(parallel) })
  # functions and settings of script 64, taken from the script itself so the two cannot drift apart
  ex <- parse(file.path(proj, "scripts", "64_stilt_djb_exposure.R"))
  want <- c("VARS", "CLASSES", "DOMAIN", "N_HOURS", "NUMPAR", "N_CORES")
  for (e in ex) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
                    ((is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) || as.character(e[[2]]) %in% want)) eval(e, globalenv())
  BASE <- Sys.getenv("METHANE_BASE", unset = dirname(normalizePath(OUT_DIR, mustWork = FALSE)))
  EXEC <- path.expand(Sys.getenv("METHANE_HYSPLIT_EXEC", "~/hysplit/exec")); BDY <- path.expand(Sys.getenv("METHANE_HYSPLIT_BDY", file.path(dirname(EXEC), "bdyfiles")))
  SUB_DIR <- file.path(path.expand(Sys.getenv("METHANE_HRRR_DIR", file.path(BASE, "met", "hrrr"))), "sub")
  RUN_DIR <- file.path(OUT_DIR, "stilt_runs_downwind_0709"); dir.create(RUN_DIR, recursive = TRUE, showWarnings = FALSE)
  pick <- function(s, n, tag) { s <- s[is.finite(s$Latitude) & is.finite(s$ALTAGL), ]; i <- unique(round(seq(1, nrow(s), length.out = n)))
    data.frame(flight = FLIGHT, group = tag, rec = seq_along(i), id = sprintf("%s_downwind_%s_r%d", FLIGHT, tag, seq_along(i)),
               run_time = as.POSIXct(round(as.numeric(s$timestamp[i]) / 60) * 60, origin = "1970-01-01", tz = "UTC"),
               lati = round(s$Latitude[i], 5), long = round(s$Longitude[i], 5), zagl = pmax(5, round(s$ALTAGL[i])),
               dch4 = round(s$dch4[i], 1), dc2h6 = round(s$dc2h6[i], 2), stringsAsFactors = FALSE) }
  lo <- L[L$level == "low", ]; hi <- L[L$level == "high", ]
  R <- rbind(pick(lo[lo$Latitude >= b$lat_s & lo$Latitude < b$lat_n, ], 8, "low_box"), pick(lo[lo$Latitude >= b$lat_n, ], 4, "low_north"),
             pick(hi[hi$Latitude >= b$lat_s & hi$Latitude < b$lat_n, ], 4, "high_box"))
  has_met <- vapply(seq_len(nrow(R)), function(i) all(file.exists(file.path(SUB_DIR, files_for(R$run_time[i])))), TRUE)
  if (!all(has_met)) stop("HRRR subgrids missing for the 9 July receptors under ", SUB_DIR, " (run scripts/64_stilt_djb_exposure.R --met-only)")
  message(sprintf("running %d STILT receptors (%d particles, %g h) ...", nrow(R), NUMPAR, N_HOURS))
  res <- unlist(mclapply(seq_len(nrow(R)), function(i) tryCatch(run_one(i), error = function(e) paste("ERROR", R$id[i], conditionMessage(e))), mc.cores = N_CORES, mc.preschedule = FALSE))
  bad <- res[!grepl("particles\\.rds$", res)]; if (length(bad)) { cat(bad, sep = "\n"); stop("STILT runs failed") }
  # EPA gridded GHGI by class on the 0.1-degree grid (as script 64, section 4)
  nc <- nc_open(GHGI_FILE); glat <- ncvar_get(nc, "lat"); glon <- ncvar_get(nc, "lon")
  ilat <- which(glat >= DOMAIN["ymn"] & glat <= DOMAIN["ymx"]); ilon <- which(glon >= DOMAIN["xmn"] & glon <= DOMAIN["xmx"])
  rdv <- function(v) { a <- ncvar_get(nc, v); if (length(dim(a)) == 3) a <- a[, , 1]; a[ilon, ilat] }
  to_umol <- function(x) x * 1e4 / 6.02214076e23 * 1e6
  allv <- grep("^emi_ch4_", names(nc$var), value = TRUE)
  Fx <- lapply(CLASSES, function(vs) to_umol(Reduce(`+`, lapply(vs, rdv)))); Fx$other <- to_umol(Reduce(`+`, lapply(setdiff(allv, unlist(CLASSES)), rdv))); nc_close(nc)
  for (k in names(Fx)) Fx[[k]][!is.finite(Fx[[k]])] <- 0
  Ftot <- Reduce(`+`, Fx); gx <- glon[ilon]; gy <- glat[ilat]; dx <- stats::median(diff(gx)); dy <- stats::median(diff(gy))
  inbox <- outer(gx, gy, function(x, y) x >= b$lon_w & x <= b$lon_e & y >= b$lat_s & y <= b$lat_n); north <- outer(gx, gy, function(x, y) y >= BASIN_LAT)
  rr <- list(); cen <- list()
  for (i in seq_len(nrow(R))) {
    pt <- readRDS(file.path(RUN_DIR, R$id[i], "particles.rds")); np <- max(pt$indx)
    ix <- round((pt$long - gx[1]) / dx) + 1; iy <- round((pt$lati - gy[1]) / dy) + 1
    ok <- ix >= 1 & ix <= length(gx) & iy >= 1 & iy <= length(gy) & pt$foot > 0; m <- cbind(ix[ok], iy[ok]); f <- pt$foot[ok]
    tot <- sum(f * Ftot[m]) / np * 1000
    cls <- vapply(Fx, function(Fk) sum(f * Fk[m]) / np * 1000, 0)
    rr[[i]] <- data.frame(R[i, ], pred_total_ppb = round(tot, 1), as.list(round(cls, 2)),
      share_box = round(sum(f * Ftot[m] * inbox[m]) / np * 1000 / tot, 3), share_north = round(sum(f * Ftot[m] * north[m]) / np * 1000 / tot, 3),
      foot_share_box = round(sum(f * inbox[m]) / sum(f), 3), foot_share_north = round(sum(f * north[m]) / sum(f), 3),   # of the footprint itself: no inventory involved
      share_og_basin = round(cls[["og_basin"]] / tot, 3), share_og_north = round(sum(f * Fx$og_basin[m] * north[m]) / np * 1000 / tot, 3),
      slope_pred = round((SOURCE_C2H6_CH4 * cls[["og_basin"]] + 0.110 * cls[["ng_dist"]]) / tot, 4), stringsAsFactors = FALSE)
    cc <- aggregate(cbind(long, lati) ~ time, pt[pt$time %% 30 == 0 | pt$time == -1, ], mean); cc$id <- R$id[i]; cc$group <- R$group[i]; cen[[i]] <- cc
  }
  S <- do.call(rbind, rr); S$share_elsewhere <- round(1 - S$share_box - S$share_north, 3); S$run_time <- format(S$run_time, "%Y-%m-%d %H:%M")
  write.csv(S, file.path(OUT_DIR, "downwind_leg_0709_stilt.csv"), row.names = FALSE); CEN <- do.call(rbind, cen)
  write.csv(CEN, file.path(OUT_DIR, "downwind_leg_0709_centroids.csv"), row.names = FALSE)
  OGC <- list(x = gx, y = gy, z = Fx$og_basin)
  for (gname in unique(S$group)) { z <- S[S$group == gname, ]
    message(sprintf("%-10s %d receptors: predicted EPA enhancement %.0f ppb (median); from cells in the Denver box %.0f%% (%.0f-%.0f), north of 40.05 N %.0f%% (%.0f-%.0f), elsewhere %.0f%%; basin-type oil and gas %.0f%%; predicted slope %.3f; footprint itself: box %.0f%% (%.0f-%.0f), north %.0f%%",
      gname, nrow(z), stats::median(z$pred_total_ppb), 100 * stats::median(z$share_box), 100 * min(z$share_box), 100 * max(z$share_box), 100 * stats::median(z$share_north),
      100 * min(z$share_north), 100 * max(z$share_north), 100 * stats::median(z$share_elsewhere), 100 * stats::median(z$share_og_basin), stats::median(z$slope_pred),
      100 * stats::median(z$foot_share_box), 100 * min(z$foot_share_box), 100 * max(z$foot_share_box), 100 * stats::median(z$foot_share_north))) }
}
if (NO_STILT) {     # no HYSPLIT here: reuse the stored STILT results (this run's output folder, else results/) so the figure keeps its map
  fnd <- function(f) { c1 <- file.path(OUT_DIR, f); c2 <- file.path(proj, "results", f); if (file.exists(c1)) c1 else if (file.exists(c2)) c2 else NA }
  fs <- fnd("downwind_leg_0709_stilt.csv"); fc <- fnd("downwind_leg_0709_centroids.csv")
  if (!is.na(fs) && !is.na(fc)) { S <- read.csv(fs, stringsAsFactors = FALSE); CEN <- read.csv(fc, stringsAsFactors = FALSE); OGC <- NULL
    message("STILT not run: receptor results and centroids read from ", dirname(fs)) }
}

## ---- figure -------------------------------------------------------------------------------
dir.create(file.path(OUT_DIR, "figures"), showWarnings = FALSE)
png(file.path(OUT_DIR, "figures/FigS_downwind_leg_0709.png"), width = if (is.null(S)) 1700 else 2400, height = 800, res = 150)
par(mfrow = c(1, if (is.null(S)) 2 else 3), mar = c(4.5, 4.5, 3, 1))
pal <- hcl.colors(50, "Viridis"); lr <- c(39.45, 40.35); colr <- function(lat) pal[pmax(1, pmin(50, round((lat - lr[1]) / diff(lr) * 49) + 1))]
for (lv in c("low", "high")) { s <- L[L$level == lv, ]
  plot(s$dch4, s$dc2h6, col = colr(s$Latitude), cex = 0.6, xlim = c(0, 140), ylim = c(0, 6), xlab = expression(Delta * CH[4] ~ "above the fixed background (ppb)"),
       ylab = expression(Delta * C[2] * H[6] ~ "above the fixed background (ppb)"), main = sprintf("%s transect, %.0f m above ground", if (lv == "low") "Low" else "High", stats::median(s$ALTAGL)))
  fb <- F[F$level == lv & grepl("^box", F$band), ]
  if (nrow(fb)) { abline(fb$intercept_ppb, fb$slope_york, lwd = 2.5); abline(0, fb$ratio_zero_intercept, lwd = 2, lty = 2)
    legend("topleft", bty = "n", cex = 0.8, lwd = c(2.5, 2), lty = c(1, 2), legend = c(sprintf("York fit, box latitudes: slope %.3f (r = %.2f)", fb$slope_york, fb$r), sprintf("zero-intercept ratio %.3f", fb$ratio_zero_intercept))) }
  lg <- seq(39.5, 40.3, by = 0.2); legend("bottomright", bty = "n", cex = 0.75, pch = 1, col = colr(lg), legend = sprintf("%.1f N", lg), title = "latitude") }
if (!is.null(S)) {
  plot(NA, xlim = c(-105.6, -103.9), ylim = c(39.2, 40.9), asp = 1 / cos(40 * pi / 180), xlab = "Longitude", ylab = "Latitude", main = "Particle-cloud centroids, 10 h back")
  rect(b$lon_w, b$lat_s, b$lon_e, b$lat_n, lwd = 1.5); abline(h = BASIN_LAT, lty = 2, col = "grey40")
  if (!is.null(OGC)) contour(OGC$x, OGC$y, OGC$z, levels = stats::quantile(OGC$z[OGC$z > 0], 0.9), add = TRUE, drawlabels = FALSE, col = "#b2182b", lwd = 1)
  cg <- c(low_box = "#2a78d6", low_north = "#eda100", high_box = "#4a3aa7")
  for (id in unique(CEN$id)) { z <- CEN[CEN$id == id, ]; z <- z[order(-z$time), ]; lines(z$long, z$lati, col = cg[z$group[1]], lwd = 1.2); points(z$long[z$time %% 120 == 0], z$lati[z$time %% 120 == 0], col = cg[z$group[1]], pch = 16, cex = 0.5) }
  points(S$long, S$lati, pch = 3, lwd = 1.6)
  legend("bottomleft", bty = "n", cex = 0.75, lwd = c(1.2, 1.2, 1.2, 1, 1.5, 1), lty = c(1, 1, 1, 1, 1, 2), col = c(cg, "#b2182b", "black", "grey40"),
         legend = c("low transect, box latitudes", "low transect, north of 39.95 N", "high transect, box latitudes", "top decile of basin oil-and-gas cells (EPA)", "Denver-metro box", "40.05 N"))
}
dev.off()
message("Wrote downwind_leg_0709_passes.csv, downwind_leg_0709_fits.csv", if (!is.null(S) && !NO_STILT) ", downwind_leg_0709_stilt.csv, downwind_leg_0709_centroids.csv" else "", " and figures/FigS_downwind_leg_0709.png")
