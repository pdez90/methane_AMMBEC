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
draw_base <- function(rl, ext, mar = c(2.4,2.4,2,1)) {
  plot(rl, col = rev(grey.colors(64, start = 0.12, end = 0.97)), legend = FALSE,
       axes = TRUE, xlim = ext[1:2], ylim = ext[3:4], mar = mar)
  inb <- CITIES$lon > ext[1] & CITIES$lon < ext[2] & CITIES$lat > ext[3] & CITIES$lat < ext[4]
  points(CITIES$lon[inb], CITIES$lat[inb], pch = 15, col = "#c62828", cex = 0.7)
  text(CITIES$lon[inb], CITIES$lat[inb], CITIES$name[inb], pos = 4, cex = 0.6, col = "#7a1010")
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
png(file.path(FIGD,"Fig1_study_map.png"), width=1280, height=1400, res=200)
draw_base(basemap(ext1), ext1, mar = c(2.4, 2.4, 4.2, 1))
legs <- read.csv(file.path(OUT_DIR, "urban_legs.csv"))
vmax <- quantile(legs$ch4_enh_mean_ppb, 0.95, na.rm = TRUE)
points(legs$lon, legs$lat, pch = 21, bg = pal(legs$ch4_enh_mean_ppb, vmax), cex = 1.1)
rect(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n, border = "#0a7d0a", lwd = 2)
abline(h = 40.05, col = "#8a5a00", lwd = 1.2, lty = 2)
text(-104.62, 40.22, "Wattenberg /\nDJB field", col = "#4a3000", cex = 0.7, font = 2)
title("AMMBEC flight legs over the Denver-Front Range\n(background: Vulcan fossil CO2, 2022)", cex.main = 0.9)

## CH4-enhancement colour key: the points are coloured by leg-mean enhancement, so
## the reader needs a scale. It floats over the empty bottom-left map corner (no
## legs fall there) inside a white box, mirroring the locator inset on the right.
op_cb <- par(no.readonly = TRUE)
par(fig = c(0.165, 0.560, 0.085, 0.215), new = TRUE, mar = c(0, 0, 0, 0))
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
  par(fig = c(0.755, 0.985, 0.025, 0.185), new = TRUE, mar = c(0, 0, 0, 0))
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
