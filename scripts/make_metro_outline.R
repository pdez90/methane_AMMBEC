# make_metro_outline.R --------------------------------------------------------
# ONE-TIME helper (not part of run_all.R). Writes the seven-county Denver-metro
# NEI footprint outline to EmissionsInventory/denver7_metro_outline.csv, which the
# Fig 1 locator inset in scripts/20_basemap_figures.R then draws. This needs
# internet ONCE (tigris downloads the Census cartographic boundary). The pipeline
# itself never calls tigris; it reads the committed CSV. Re-run only if you want
# to regenerate the boundary file.
#
# The seven counties are the same set summed for E_CO_NEI in scripts/17 (FIPS
# 08031 Denver, 08001 Adams, 08005 Arapahoe, 08013 Boulder, 08014 Broomfield,
# 08035 Douglas, 08059 Jefferson). The area printed below should be ~11,800 km2.
#
# Run once:  Rscript scripts/make_metro_outline.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
if (!requireNamespace("tigris", quietly = TRUE) ||
    !requireNamespace("sf", quietly = TRUE))
  stop("This one-time helper needs the 'tigris' and 'sf' packages. ",
       "install.packages(c('tigris','sf')). The main pipeline does not need them.")
options(tigris_use_cache = TRUE)

FIPS7 <- c("031", "001", "005", "013", "014", "035", "059")   # see header
co  <- tigris::counties(state = "08", cb = TRUE, year = 2020, class = "sf")
sub <- co[co$COUNTYFP %in% FIPS7, ]
if (nrow(sub) != 7L)
  warning(sprintf("Expected 7 counties, matched %d: %s",
                  nrow(sub), paste(sort(sub$NAME), collapse = ", ")))

u  <- sf::st_cast(sf::st_union(sf::st_transform(sub, 4326)), "POLYGON")
rows <- list()
for (i in seq_along(u)) {
  xy <- sf::st_coordinates(u[i])
  rows[[i]] <- data.frame(part = i, lon = xy[, "X"], lat = xy[, "Y"])
}
res <- do.call(rbind, rows)
# Write where the rest of the data lives. config.R's INV_DIR default points at
# ~/MethaneData/EmissionsInventory, which is not where this project keeps its
# inventories after reorganize_data.sh; prefer the sibling inventories/ folder when
# it exists, so running this script bare (no environment variables) still puts the
# outline where scripts 20 and 36 will look for it.
inv <- INV_DIR
if (!nzchar(Sys.getenv("METHANE_INV_DIR"))) {
  sib <- normalizePath(file.path(proj, "..", "inventories"), mustWork = FALSE)
  if (dir.exists(sib)) inv <- sib
}
dir.create(inv, showWarnings = FALSE, recursive = TRUE)
outfile <- file.path(inv, "denver7_metro_outline.csv")
write.csv(res, outfile, row.names = FALSE)

area_km2 <- as.numeric(sum(sf::st_area(sf::st_transform(sub, 5070)))) / 1e6
message(sprintf("Wrote %s\n  %d boundary part(s), %d vertices; seven-county area = %.0f km2 (expect ~11,800).",
                outfile, length(u), nrow(res), area_km2))
