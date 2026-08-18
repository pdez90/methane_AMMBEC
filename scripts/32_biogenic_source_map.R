# 32_biogenic_source_map.R ----------------------------------------------------
# WHERE the biogenic methane sources are, relative to the analysis box and the DJB.
#
# WHY THIS EXISTS. A reviewer asked to see "the spatial context from an emission
# inventory that shows the locations of major known biogenic sources relative to
# the DJB on a map", noting that a larger biogenic fraction is expected nearer
# landfills and wastewater plants. This draws exactly that, from the gridded U.S.
# EPA GHGI methane inventory (the same file script 14 sums), so every plotted
# location comes from the inventory rather than from hand-entered facility
# coordinates. The three named facilities in biogenic_sources.csv are overlaid on
# top for orientation, and are the only hand-entered points on the figure.
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

# Group the biogenic sectors into the two families the reviewer asked about.
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

vmax <- max(c(G$waste_t_hr, G$livestock_t_hr), na.rm = TRUE)
# area-proportional symbols: radius ~ sqrt(value) so area encodes emission
sz <- function(v) 0.35 + 3.1 * sqrt(pmax(v, 0) / vmax)

FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "FigS9_biogenic_sources.png"), width = 1180, height = 1280, res = 190)
par(mar = c(5.2, 4.2, 3.6, 1))
plot(NA, xlim = EXT[1:2], ylim = EXT[3:4], xlab = "Longitude", ylab = "Latitude",
     main = "Gridded EPA GHGI biogenic methane sources\nrelative to the analysis box and the DJB")
abline(h = pretty(EXT[3:4]), v = pretty(EXT[1:2]), col = "grey94")

kw <- G$waste_t_hr     > 0
kl <- G$livestock_t_hr > 0
if (any(kl)) points(G$lon[kl], G$lat[kl], pch = 19, col = "#e8710a55", cex = sz(G$livestock_t_hr[kl]))
if (any(kw)) points(G$lon[kw], G$lat[kw], pch = 19, col = "#2C7FB866", cex = sz(G$waste_t_hr[kw]))

rect(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n,
     border = "#0a7d0a", lwd = 2)
abline(h = 40.05, col = "#8a5a00", lwd = 1.2, lty = 2)
text(-104.62, 40.24, "Wattenberg /\nDJB field", col = "#4a3000", cex = 0.62, font = 2)
text(URBAN_BOX$lon_w + 0.02, URBAN_BOX$lat_n - 0.03, "analysis box", col = "#0a7d0a",
     cex = 0.6, pos = 4, font = 2)

inb <- CITIES$lon > EXT[1] & CITIES$lon < EXT[2] & CITIES$lat > EXT[3] & CITIES$lat < EXT[4]
points(CITIES$lon[inb], CITIES$lat[inb], pch = 15, col = "#c62828", cex = 0.6)
text(CITIES$lon[inb], CITIES$lat[inb], CITIES$name[inb], pos = 4, cex = 0.55, col = "#7a1010")

# named facilities, the only hand-entered coordinates on this figure
bsp <- file.path(proj, "biogenic_sources.csv")
if (file.exists(bsp)) {
  bs <- read.csv(bsp, stringsAsFactors = FALSE)
  bs <- bs[bs$lon > EXT[1] & bs$lon < EXT[2] & bs$lat > EXT[3] & bs$lat < EXT[4], ]
  if (nrow(bs)) {
    points(bs$lon, bs$lat, pch = 4, lwd = 1.6, col = "black", cex = 0.9)
    # alternate the label side, and nudge vertically, so near-coincident
    # facilities (Suncor and Metro Water Recovery are about 1 km apart) stay legible
    bs <- bs[order(bs$lat), ]
    text(bs$lon, bs$lat + rep(c(0.018, -0.018), length.out = nrow(bs)), bs$name,
         pos = rep(c(4, 2), length.out = nrow(bs)), cex = 0.48, col = "black")
  }
}

legend("bottomleft", bty = "n", cex = 0.6, pch = c(19, 19, 4), pt.cex = c(1.6, 1.6, 0.9),
       col = c("#2C7FB866", "#e8710a55", "black"),
       legend = c("landfills, wastewater, composting", "enteric fermentation, manure",
                  "named facility (see biogenic_sources.csv)"))
# size key
# Size key drawn manually: legend() places entries on a fixed pitch, so
# area-proportional symbols this large overlap each other.
sk  <- signif(c(0.25, 0.5, 1) * vmax, 2)
usr <- par("usr")
kx  <- usr[2] - 0.085 * diff(usr[1:2])
ky  <- usr[4] - 0.055 * diff(usr[3:4])
dy  <- 0.052 * diff(usr[3:4])
text(kx, ky + 0.6 * dy, expression("t CH"[4]*" hr"^-1*" per cell"),
     cex = 0.5, col = "grey20", pos = 2, offset = -0.4)
for (i in seq_along(sk)) {
  points(kx, ky - (i - 1) * dy, pch = 21, bg = "grey85", col = "grey40", cex = sz(sk[i]))
  text(kx + 0.022 * diff(usr[1:2]), ky - (i - 1) * dy, format(sk[i], scientific = FALSE),
       cex = 0.5, col = "grey20", pos = 4)
}
mtext(sprintf("gridded EPA GHGI (%s); ~0.1 degree cells, so these are source regions, not facility footprints",
              basename(GHGI_FILE)), side = 1, line = 3.8, cex = 0.5, col = "grey35")
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
