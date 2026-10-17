# 109_basemap_tiles.R ---------------------------------------------------------
# Street basemaps for the Shiny explorer's maps, fetched ONCE here and stored as
# PNG files, so the app needs no map service, no API key and no network at run
# time. Tiles come from CARTO's "Positron" style (OpenStreetMap data), which is
# served without a key; the attribution the app prints under each map is the one
# maptiles reports for that provider. Nothing in the analysis uses these images.
#
# Extents (lon/lat, WGS84):
#   region   the ARL car map (box to the DJ Basin)            zoom 10
#   metro    the analysis box with a margin (aircraft map)   zoom 11
#   <site>   each CDPHE survey site (from cdphe_hotspots.csv) zoom 15
#
# Out: shiny/data/basemap_<name>.png, shiny/data/basemap_extents.csv
#      (name, xmin, xmax, ymin, ymax, zoom, provider, attribution)
# Run: Rscript scripts/109_basemap_tiles.R          (needs maptiles, terra, png;
#      installs maptiles from CRAN if missing; re-run only to refresh the images)
# Override the provider with METHANE_BASEMAP_PROVIDER (e.g. "OpenStreetMap").
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (p in c("maptiles", "terra", "png")) if (!requireNamespace(p, quietly = TRUE)) {
  message("installing ", p); install.packages(p, repos = "https://cloud.r-project.org") }
suppressPackageStartupMessages({ library(terra) })
PROVIDER <- Sys.getenv("METHANE_BASEMAP_PROVIDER", "CartoDB.Positron")
DEST <- file.path(proj, "shiny", "data"); dir.create(DEST, recursive = TRUE, showWarnings = FALSE)
CACHE <- file.path(tempdir(), "tiles")

b <- URBAN_BOX
ext_list <- list(
  region = list(ext = c(-105.35, -104.30, 39.45, 40.75), zoom = 10),
  metro  = list(ext = c(b$lon_w - 0.12, b$lon_e + 0.12, b$lat_s - 0.08, b$lat_n + 0.08), zoom = 11))
cd <- file.path(DEST, "cdphe_hotspots.csv")
if (file.exists(cd)) {
  h <- read.csv(cd, stringsAsFactors = FALSE)
  for (s in unique(h$site)) {
    hs <- h[h$site == s, ]; ctr <- c(median(hs$lon), median(hs$lat))
    ext_list[[paste0("site_", s)]] <- list(ext = c(ctr[1] - 0.075, ctr[1] + 0.075, ctr[2] - 0.06, ctr[2] + 0.06), zoom = 15)
  }
} else message("cdphe_hotspots.csv not in shiny/data (run script 86 first): no site basemaps")

rows <- list()
for (nm in names(ext_list)) {
  e <- ext_list[[nm]]
  x <- terra::ext(e$ext[1], e$ext[2], e$ext[3], e$ext[4])
  r <- tryCatch(maptiles::get_tiles(x, provider = PROVIDER, zoom = e$zoom, crop = TRUE, cachedir = CACHE, verbose = FALSE),
                error = function(err) { message(nm, ": tile download failed: ", conditionMessage(err)); NULL })
  if (is.null(r)) next
  r <- terra::project(r, "EPSG:4326", method = "bilinear")          # regular in lon/lat so ggplot2::annotation_raster can place it
  r <- terra::crop(r, x)
  a <- terra::as.array(r)[, , 1:3] / 255; a[!is.finite(a)] <- 1
  png::writePNG(a, file.path(DEST, sprintf("basemap_%s.png", nm)))
  xe <- as.vector(terra::ext(r))
  rows[[nm]] <- data.frame(name = nm, xmin = xe[1], xmax = xe[2], ymin = xe[3], ymax = xe[4], zoom = e$zoom,
                           provider = PROVIDER, attribution = maptiles::get_credit(PROVIDER), stringsAsFactors = FALSE)
  message(sprintf("%-22s %d x %d px, %.3f to %.3f E, %.3f to %.3f N", nm, ncol(r), nrow(r), xe[1], xe[2], xe[3], xe[4]))
}
if (!length(rows)) stop("no basemap written (no network, or the tile server refused)")
write.csv(do.call(rbind, rows), file.path(DEST, "basemap_extents.csv"), row.names = FALSE)
message("wrote ", length(rows), " basemap PNG(s) and basemap_extents.csv to shiny/data/ (", maptiles::get_credit(PROVIDER), ")")
