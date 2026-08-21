# 32_biogenic_source_map.R ----------------------------------------------------
# WHERE the biogenic methane sources are, relative to the analysis box and the DJB.
#
# WHY THIS EXISTS. A larger biogenic fraction is expected nearer landfills and
# wastewater plants, so where the major biogenic sources sit relative to the
# analysis box and the DJB bears on how the urban fossil fraction should be read.
# This draws that spatial context from the gridded U.S. EPA GHGI methane
# inventory (the same file script 14 sums), so every plotted location comes from
# the inventory rather than from hand-entered facility coordinates. The three
# named facilities in biogenic_sources.csv are overlaid on top for orientation,
# and are the only hand-entered points on the figure.
#
# The gridded GHGI is ~0.1 degree, so these are coarse source regions, not
# facility footprints. That is a property of the inventory, not of the plot.
#
# Needs: ncdf4 and the gridded GHGI file (METHANE_GHGI). Base R otherwise.
# Out:   <OUT_DIR>/biogenic_grid.csv
#        <OUT_DIR>/figures/FigS9_biogenic_sources.png
# Run:   Rscript scripts/32_biogenic_source_map.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))

if (!requireNamespace("ncdf4", quietly = TRUE)) {
  message("ncdf4 required for the gridded GHGI; skipping."); quit(save = "no")
}
if (!file.exists(GHGI_FILE)) {
  message("Gridded GHGI file not found: ", GHGI_FILE, " (set METHANE_GHGI); skipping.")
  quit(save = "no")
}

# Same extent as Figure 1, so the two maps are directly comparable.
EXT <- c(-105.45, -104.35, 39.20, 40.40)

nc   <- ncdf4::nc_open(GHGI_FILE)
lat  <- ncdf4::ncvar_get(nc, "lat"); lon <- ncdf4::ncvar_get(nc, "lon")
area <- ncdf4::ncvar_get(nc, "grid_cell_area")          # cm^2 (lon, lat)
ix   <- which(lon >= EXT[1] & lon <= EXT[2])
iy   <- which(lat >= EXT[3] & lat <= EXT[4])
NA_  <- 6.022e23; MW <- 16.04

# molec cm-2 s-1 -> t CH4 / hr, per cell (same conversion as script 14, but kept
# per cell instead of summed)
cell_thr <- function(v) {
  e <- ncdf4::ncvar_get(nc, v)
  e[ix, iy] * area[ix, iy] * MW / NA_ * 3600 / 1e6
}
vars <- names(nc$var); emi <- vars[grepl("^emi_ch4", vars)]

# Group the biogenic sectors into the two families that separate in space here:
# waste, which concentrates in and around the city, and livestock, which
# concentrates to the north-east over the gas field.
waste_pat <- "Landfill|Wastewater|Composting"
lstk_pat  <- "Enteric|Manure"
w_vars <- emi[grepl(waste_pat, emi)]
l_vars <- emi[grepl(lstk_pat,  emi)]
if (!length(w_vars) && !length(l_vars)) {
  ncdf4::nc_close(nc)
  message("No landfill/wastewater/livestock variables found in ", basename(GHGI_FILE), "; skipping.")
  quit(save = "no")
}
acc <- function(vs) {
  if (!length(vs)) return(matrix(0, length(ix), length(iy)))
  Reduce(`+`, lapply(vs, cell_thr))
}
W <- acc(w_vars); L <- acc(l_vars)
ncdf4::nc_close(nc)

G <- expand.grid(lon = lon[ix], lat = lat[iy], KEEP.OUT.ATTRS = FALSE)
G$waste_t_hr     <- as.numeric(W)
G$livestock_t_hr <- as.numeric(L)
G <- G[is.finite(G$waste_t_hr) | is.finite(G$livestock_t_hr), ]
G$waste_t_hr[!is.finite(G$waste_t_hr)] <- 0
G$livestock_t_hr[!is.finite(G$livestock_t_hr)] <- 0
G <- G[order(-(G$waste_t_hr + G$livestock_t_hr)), ]
write.csv(data.frame(lon = G$lon, lat = G$lat,
                     waste_t_hr = signif(G$waste_t_hr, 4),
                     livestock_t_hr = signif(G$livestock_t_hr, 4)),
          file.path(OUT_DIR, "biogenic_grid.csv"), row.names = FALSE)

# ---- figure ----------------------------------------------------------------
CITIES <- data.frame(
  name = c("Denver", "Boulder", "Aurora", "DIA", "Golden"),
  lat  = c(39.7392, 40.015, 39.729, 39.856, 39.755),
  lon  = c(-104.9903, -105.27, -104.832, -104.674, -105.221))

bsp <- file.path(proj, "biogenic_sources.csv")
BS  <- if (file.exists(bsp)) read.csv(bsp, stringsAsFactors = FALSE) else NULL
if (!is.null(BS)) {
  BS <- BS[BS$lon > EXT[1] & BS$lon < EXT[2] & BS$lat > EXT[3] & BS$lat < EXT[4], ]
  BS$short <- trimws(sub(" *\\(.*$", "", BS$name))   # drop parenthetical, keep it legible
}

# One panel per source family. Plotting both families on a single panel put a
# symbol of each colour on every cell, which occluded the pattern; and a single
# shared size scale is dominated by one cell outside the box that alone carries
# more than twice the whole in-box waste total, which shrank everything of
# interest to a dot. Each panel therefore gets its own scale, capped at the 95th
# percentile of its non-zero cells so the bulk of the field stays legible; cells
# above the cap are ringed so they remain identifiable rather than hidden.
panel <- function(v, fillcol, ttl) {
  vv   <- v[v > 0]
  vcap <- if (length(vv)) as.numeric(stats::quantile(vv, 0.95)) else 1
  szf  <- function(x) 0.30 + 3.0 * sqrt(pmin(x, vcap) / vcap)
  plot(NA, xlim = EXT[1:2], ylim = EXT[3:4], xlab = "Longitude", ylab = "Latitude",
       main = ttl, cex.main = 0.95)
  abline(h = pretty(EXT[3:4]), v = pretty(EXT[1:2]), col = "grey94")
  k <- v > 0
  if (any(k)) points(G$lon[k], G$lat[k], pch = 19, col = fillcol, cex = szf(v[k]))
  ov <- v > vcap
  if (any(ov)) points(G$lon[ov], G$lat[ov], pch = 1, col = "grey15", lwd = 1.1, cex = szf(v[ov]))
  rect(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n,
       border = "#0a7d0a", lwd = 2)
  abline(h = 40.05, col = "#8a5a00", lwd = 1.2, lty = 2)
  text(-104.60, 40.28, "Wattenberg / DJB", col = "#4a3000", cex = 0.55, font = 2, pos = 2)
  text(URBAN_BOX$lon_w + 0.02, URBAN_BOX$lat_n - 0.03, "analysis box",
       col = "#0a7d0a", cex = 0.52, pos = 4, font = 2)
  inb <- CITIES$lon > EXT[1] & CITIES$lon < EXT[2] & CITIES$lat > EXT[3] & CITIES$lat < EXT[4]
  points(CITIES$lon[inb], CITIES$lat[inb], pch = 15, col = "#c62828", cex = 0.5)
  text(CITIES$lon[inb], CITIES$lat[inb], CITIES$name[inb], pos = 4, cex = 0.46, col = "#7a1010")
  if (!is.null(BS) && nrow(BS)) {
    points(BS$lon, BS$lat, pch = 4, lwd = 1.5, col = "black", cex = 0.75)
    text(BS$lon, BS$lat + rep(c(0.030, -0.030), length.out = nrow(BS)), BS$short,
         pos = rep(c(4, 2), length.out = nrow(BS)), cex = 0.42, col = "black")
  }
  # size key, drawn by hand so the symbols do not collide on a fixed legend pitch
  sk  <- signif(c(0.25, 0.6, 1) * vcap, 2)
  usr <- par("usr")
  kx  <- usr[1] + 0.10 * diff(usr[1:2]); ky <- usr[3] + 0.155 * diff(usr[3:4])
  dy  <- 0.050 * diff(usr[3:4])
  text(kx, ky + 0.75 * dy, expression("t CH"[4]*" hr"^-1*" per cell"),
       cex = 0.44, col = "grey20", pos = 4, offset = -0.6)
  for (i in seq_along(sk)) {
    points(kx, ky - (i - 1) * dy, pch = 21, bg = "grey88", col = "grey40", cex = szf(sk[i]))
    text(kx + 0.035 * diff(usr[1:2]), ky - (i - 1) * dy, format(sk[i], scientific = FALSE),
         cex = 0.44, col = "grey20", pos = 4)
  }
  invisible(vcap)
}

FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "FigS9_biogenic_sources.png"), width = 1560, height = 900, res = 168)
par(mfrow = c(1, 2), mar = c(4.4, 4.0, 3.0, 0.8), oma = c(2.0, 0, 2.6, 0))
cap_w <- panel(G$waste_t_hr,     "#2C7FB8AA", "Landfills, wastewater, composting")
cap_l <- panel(G$livestock_t_hr, "#e8710aAA", "Enteric fermentation and manure")
mtext("Gridded EPA GHGI biogenic methane sources relative to the analysis box and the DJB",
      outer = TRUE, side = 3, line = 0.7, cex = 0.95, font = 2)
mtext(sprintf("%s; ~0.1 degree cells, so these are source regions not facility footprints. Symbol area is proportional to emission, capped at each panel's 95th percentile (%.2f and %.2f t/hr); ringed cells exceed the cap.",
              basename(GHGI_FILE), cap_w, cap_l),
      outer = TRUE, side = 1, line = 0.5, cex = 0.44, col = "grey35")
dev.off()

message(sprintf("Biogenic source map over %s.", paste(EXT, collapse = ", ")))
message(sprintf("Waste sectors: %s", paste(sub("emi_ch4_", "", w_vars), collapse = ", ")))
message(sprintf("Livestock sectors: %s", paste(sub("emi_ch4_", "", l_vars), collapse = ", ")))
message(sprintf("In-box waste total = %.3f t/hr; in-box livestock total = %.3f t/hr.",
                sum(G$waste_t_hr[G$lon >= URBAN_BOX$lon_w & G$lon <= URBAN_BOX$lon_e &
                                 G$lat >= URBAN_BOX$lat_s & G$lat <= URBAN_BOX$lat_n]),
                sum(G$livestock_t_hr[G$lon >= URBAN_BOX$lon_w & G$lon <= URBAN_BOX$lon_e &
                                     G$lat >= URBAN_BOX$lat_s & G$lat <= URBAN_BOX$lat_n])))
message("Wrote biogenic_grid.csv and figures/FigS9_biogenic_sources.png")
