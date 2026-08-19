# 20_basemap_figures.R --------------------------------------------------------
# Study map (Fig 1) and the SI per-flight maps, drawn over a Vulcan fossil-CO2
# basemap (the offline emissions raster traces the urban road network and built-up
# area — no map tiles needed). Pure R:
# terra (to read/reproject the GeoTIFF) + base graphics. install.packages("terra").
#
# Out: <OUT_DIR>/figures/{Fig1_study_map,SI_perflight_maps}.png
# Run: Rscript scripts/20_basemap_figures.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
suppressMessages(library(terra))
FIGD <- file.path(OUT_DIR, "figures"); dir.create(FIGD, showWarnings = FALSE, recursive = TRUE)

CITIES <- data.frame(
  name = c("Denver","Boulder","Aurora","DIA","Golden"),
  lat  = c(39.7392, 40.015, 39.729, 39.856, 39.755),
  lon  = c(-104.9903, -105.27, -104.832, -104.674, -105.221))

# Vulcan basemap cropped/reprojected to a lon/lat extent, as a SpatRaster.
basemap <- function(ext) {                    # ext = c(lonW,lonE,latS,latN)
  r <- rast(VULCAN_FILE)
  e <- project(as.polygons(ext(ext[1],ext[2],ext[3],ext[4]), crs="EPSG:4326"), crs(r))
  r <- crop(r, e)
  r <- project(r, "EPSG:4326")
  rl <- log10(r); rl[rl < log10(20)] <- NA    # log stretch; hide near-zero
  rl
}
# Text with a white outline, so labels stay readable where they fall on top of
# dense flight tracks or leg markers.
halo_text <- function(x, y, labels, col, cex = 0.6, pos = 4, font = 1, r = 0.007) {
  for (dx in c(-r, 0, r)) for (dy in c(-r, 0, r)) if (dx != 0 || dy != 0)
    text(x + dx, y + dy, labels, col = "white", cex = cex, pos = pos, font = font)
  text(x, y, labels, col = col, cex = cex, pos = pos, font = font)
}
draw_cities <- function(ext, cex_pt = 0.7, cex_lab = 0.6) {
  inb <- CITIES$lon > ext[1] & CITIES$lon < ext[2] & CITIES$lat > ext[3] & CITIES$lat < ext[4]
  points(CITIES$lon[inb], CITIES$lat[inb], pch = 15, col = "#c62828", cex = cex_pt)
  halo_text(CITIES$lon[inb], CITIES$lat[inb], CITIES$name[inb], col = "#7a1010", cex = cex_lab)
}
draw_base <- function(rl, ext, mar = c(2.4,2.4,2,1), cities = TRUE) {
  plot(rl, col = rev(grey.colors(64, start = 0.12, end = 0.97)), legend = FALSE,
       axes = TRUE, xlim = ext[1:2], ylim = ext[3:4], mar = mar)
  if (cities) draw_cities(ext)
}
# Quantitative key for the grey Vulcan background. The raster is log10 of the
# Vulcan fossil-CO2 layer, whose native units are tonnes of CARBON per 1-km cell
# per year, so the labels convert to t CO2 with 44.01/12.011 (same factor as
# script 16). Reviewer asked for a scale rather than an unlabelled grey wash.
C_TO_CO2 <- 44.01 / 12.011
vulcan_key <- function(rl, fig = c(0.115, 0.525, 0.800, 0.868)) {
  rng <- range(values(rl), na.rm = TRUE)          # log10 t C / km2 / yr
  if (!all(is.finite(rng))) return(invisible(FALSE))
  op <- par(no.readonly = TRUE)
  par(fig = fig, new = TRUE, mar = c(0, 0, 0, 0))
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "",
       xaxs = "i", yaxs = "i")
  rect(0, 0, 1, 1, col = "white", border = "grey45", lwd = 0.8)
  nb <- 120; xb <- seq(0.07, 0.93, length.out = nb + 1)
  rect(xb[-(nb + 1)], 0.42, xb[-1], 0.70,
       col = rev(grey.colors(nb, start = 0.12, end = 0.97)), border = NA)
  rect(0.07, 0.42, 0.93, 0.70, border = "grey40", lwd = 0.7)
  # decade ticks in t CO2 km-2 yr-1, placed on the log scale actually plotted
  decs <- seq(ceiling(rng[1]), floor(rng[2]))
  if (length(decs)) {
    fr <- 0.07 + (decs - rng[1]) / diff(rng) * (0.93 - 0.07)
    keep <- fr >= 0.07 & fr <= 0.93
    segments(fr[keep], 0.42, fr[keep], 0.36, col = "grey40", lwd = 0.7)
    text(fr[keep], 0.30,
         parse(text = sprintf("10^%d", decs[keep] + round(log10(C_TO_CO2)))),
         cex = 0.52, col = "grey15")
  }
  text(0.5, 0.90, expression("Vulcan fossil CO"[2]*" (t km"^-2*" yr"^-1*")"),
       cex = 0.56, col = "grey10")
  par(op); invisible(TRUE)
}

pal <- function(v, vmax) {                     # plasma-ish for enhancement
  cr <- colorRamp(c("#0d0887","#7e03a8","#cc4778","#f89540","#f0f921"))
  z <- pmin(pmax(v,0), vmax)/vmax; z[!is.finite(z)] <- 0
  rgb(cr(z), maxColorValue = 255)
}
rd <- function(p) { ic <- read_icartt(p); d <- ic$data
  bg <- quantile(d$CH4_ppb, 0.05, na.rm = TRUE); d$enh <- d$CH4_ppb - bg; list(d = d, date = ic$meta$date) }

flights <- Filter(function(p) grepl("ARL-Suite", p), list_flights(DATA_DIR))

## ---- Fig 1: overview ----
ext1 <- c(-105.45,-104.35,39.20,40.40)
bm1 <- basemap(ext1)          # built once; reused by the quantitative key below
png(file.path(FIGD,"Fig1_study_map.png"), width=1280, height=1560, res=200)
draw_base(bm1, ext1, mar = c(2.4, 2.4, 9.2, 2.6), cities = FALSE)
legs <- read.csv(file.path(OUT_DIR, "urban_legs.csv"))
vmax <- quantile(legs$ch4_enh_mean_ppb, 0.95, na.rm = TRUE)

# Actual 1 Hz flight tracks under the leg markers. A reviewer noted that leg
# midpoints alone do not show where the aircraft went, and that the pattern was
# largely repeated day to day, so we draw every track faintly and pick out one
# representative flight: the flight contributing the most urban legs, chosen from
# the data rather than hardcoded.
rep_flight <- NA_character_
if ("region" %in% names(legs)) {
  tabu <- table(legs$flight[legs$region == "urban"])
  if (length(tabu)) rep_flight <- names(sort(tabu, decreasing = TRUE))[1]
}
for (p in flights) {
  if (!is.na(rep_flight) && basename(p) == rep_flight) next   # drawn highlighted below
  x <- tryCatch(read_icartt(p), error = function(e) NULL); if (is.null(x)) next
  lines(x$data$Longitude, x$data$Latitude, col = "#5b5b5b45", lwd = 0.5)
}
if (!is.na(rep_flight)) {
  pr <- flights[basename(flights) == rep_flight]
  if (length(pr)) {
    dr <- tryCatch(read_icartt(pr[1])$data, error = function(e) NULL)
    if (!is.null(dr)) lines(dr$Longitude, dr$Latitude, col = "#1f3864", lwd = 1.1)
  }
}
points(legs$lon, legs$lat, pch = 21, bg = pal(legs$ch4_enh_mean_ppb, vmax), cex = 1.1)
rect(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n, border = "#0a7d0a", lwd = 2)
abline(h = 40.05, col = "#8a5a00", lwd = 1.2, lty = 2)
draw_cities(ext1)          # after the tracks and legs, so labels are not buried
halo_text(-104.62, 40.22, "Wattenberg /\nDJB field", col = "#4a3000", cex = 0.7,
          pos = NULL, font = 2)
op_ttl <- par(no.readonly = TRUE)
par(fig = c(0, 1, 0.872, 1), new = TRUE, mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "",
     xaxs = "i", yaxs = "i")
text(0.5, 0.74, "AMMBEC flight legs over the Denver-Front Range", cex = 0.95, font = 2)
text(0.5, 0.47, "(background: Vulcan fossil CO2, 2022)", cex = 0.95, font = 2)
if (!is.na(rep_flight))
  text(0.5, 0.17, sprintf("all flight tracks in grey; %s highlighted in blue",
                          sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", rep_flight))),
       cex = 0.62, col = "#1f3864")
par(op_ttl)
vulcan_key(bm1)

## CH4-enhancement colour key. Both colour keys sit in the top margin rather than
## on the map: with the flight tracks added there is no longer an empty corner
## large enough for a legend box, and three boxes inside the frame collided with
## each other and with the axes.
op_cb <- par(no.readonly = TRUE)
par(fig = c(0.560, 0.970, 0.800, 0.868), new = TRUE, mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
rect(0, 0, 1, 1, col = "white", border = "grey45", lwd = 0.8)
ncb <- 120; xcb <- seq(0.07, 0.93, length.out = ncb + 1); ycb0 <- 0.42; ycb1 <- 0.70
rect(xcb[-(ncb + 1)], ycb0, xcb[-1], ycb1, col = pal(seq(0, vmax, length.out = ncb), vmax), border = NA)
rect(0.07, ycb0, 0.93, ycb1, border = "grey40", lwd = 0.7)
text(0.07, ycb0 - 0.18, "0", cex = 0.62, col = "grey15")
text(0.93, ycb0 - 0.18, round(vmax), cex = 0.62, col = "grey15", pos = 2, offset = 0.1)
text(0.5, 0.90, "CH4 enhancement (ppb)", cex = 0.64, col = "grey10")
par(op_cb)

## locator inset: the analysis box within the seven-county NEI footprint.
## Boundary is the committed CSV from scripts/make_metro_outline.R (run once).
## If it is absent the inset is skipped, so the figure still builds.
of <- file.path(INV_DIR, "denver7_metro_outline.csv")
if (file.exists(of)) {
  oc <- read.csv(of)
  op <- par(no.readonly = TRUE)
  par(fig = c(0.735, 0.958, 0.088, 0.245), new = TRUE, mar = c(0, 0, 0, 0))
  xr <- range(oc$lon, URBAN_BOX$lon_w, URBAN_BOX$lon_e)
  yr <- range(oc$lat, URBAN_BOX$lat_s, URBAN_BOX$lat_n)
  xr <- xr + c(-1, 1) * 0.04 * diff(xr); yr <- yr + c(-1, 1) * 0.04 * diff(yr)
  plot(NA, xlim = xr, ylim = yr, asp = 1 / cos(mean(yr) * pi / 180),
       axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
       col = "white", border = "grey45")
  for (pp in split(oc, oc$part))
    polygon(pp$lon, pp$lat, border = "#e8710a", lwd = 1.1, col = "#fdece0")
  rect(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n,
       border = "#0a7d0a", lwd = 1.3)
  mtext("7-county anchor vs box", side = 1, line = -0.9, cex = 0.42, col = "grey25")
  par(op)
} else {
  message("Fig 1 locator inset skipped: ", of,
          " not found. Run scripts/make_metro_outline.R once to create it.")
}
dev.off()

## ---- SI: per-flight maps (EXACTLY the analysis flights, >= 3 urban legs) ----
# Select the same flight set the manuscript analyses (Table 1), so the figure and
# its caption agree. Order and fossil fraction come from table1.csv when present.
extP <- c(-105.35,-104.45,39.25,40.10); bmP <- basemap(extP)
uf <- read.csv(file.path(OUT_DIR, "urban_flux.csv"), stringsAsFactors = FALSE)
keep <- uf$flight[uf$n_urban_legs >= 3]                        # basenames incl .ict
sel_paths <- flights[basename(flights) %in% keep]
# per-flight York fossil % (for the panel titles), matched by short flight id
foss_lbl <- setNames(rep(NA_character_, length(sel_paths)),
                     sub("AMMBEC-ARL-Suite_TwinOtter_","",sub(".ict","",basename(sel_paths))))
t1p <- file.path(OUT_DIR, "table1.csv")
if (file.exists(t1p)) { t1 <- read.csv(t1p, stringsAsFactors = FALSE)
  fk <- sub("\\.ict$","",sub("AMMBEC-ARL-Suite_TwinOtter_","",t1$Flight))
  for (i in seq_along(foss_lbl)) { m <- match(names(foss_lbl)[i], fk)
    if (!is.na(m)) foss_lbl[i] <- sub(" .*","",t1$Fossil_pct_CI[m]) } }
n <- length(sel_paths); ncol <- 4; nrow <- max(1, ceiling(n / ncol))
VMAX <- 60   # ppb; top of the plasma CH4-enhancement scale, shared across all panels
png(file.path(FIGD,"SI_perflight_maps.png"), width = 600*ncol, height = 640*nrow + 140, res = 180)
# panel grid plus a dedicated bottom row for one shared colour bar
lay <- rbind(matrix(seq_len(nrow*ncol), nrow = nrow, byrow = TRUE), rep(nrow*ncol + 1L, ncol))
op <- par(no.readonly = TRUE)
layout(lay, heights = c(rep(1, nrow), 0.22))
for (i in seq_len(nrow*ncol)) {
  if (i > n) { plot.new(); next }
  p  <- sel_paths[i]
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_","",sub(".ict","",basename(p)))
  x  <- tryCatch(rd(p), error = function(e) NULL)
  # taller top margin so the flight title sits ABOVE the map, clear of the tracks
  draw_base(bmP, extP, mar = c(2.2, 2.6, 3.2, 0.8))
  if (!is.null(x)) points(x$d$Longitude, x$d$Latitude, pch = 20, cex = 0.3, col = pal(x$d$enh, VMAX))
  ff <- foss_lbl[[fl]]
  title(if (!is.na(ff) && nzchar(ff)) sprintf("%s  (fossil %s%%)", fl, ff) else fl,
        cex.main = 0.95, line = 1.2)
}
# ---- shared horizontal colour bar for CH4 enhancement ----
par(mar = c(3.0, 14, 0.6, 14))
plot(NA, xlim = c(0, VMAX), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
nb <- 200; xb <- seq(0, VMAX, length.out = nb + 1)
rect(xb[-(nb + 1)], 0, xb[-1], 1, col = pal((xb[-(nb + 1)] + xb[-1]) / 2, VMAX), border = NA)
axis(1, at = pretty(c(0, VMAX)), cex.axis = 1.0, mgp = c(1, 0.5, 0))
mtext("CH4 enhancement (ppb)", side = 1, line = 1.8, cex = 1.0)
box(lwd = 0.9)
par(op); dev.off()
message("SI per-flight panels: ", n, " flights (>=3 urban legs).")

# NB: the old "Fig 4" closed-loop flight map was removed. No AMMBEC flight closes a
# valid box loop (script 13), so the manuscript reports no closed-loop flux and the
# loop map would be misleading. The honest replacement is the loop-closure
# diagnostic (Figure S6, script 28). Figure 4 in the manuscript is now the source-
# attribution chart (script 23).
message("figures written to ", FIGD)
