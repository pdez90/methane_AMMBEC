# 17_nei_co_metro.R -----------------------------------------------------------
# Denver-metropolitan CO emission rate from the EPA 2020 National Emissions
# Inventory (county-by-sector file), feeding E_CO_DENVER in config.R for the
# CH4:CO enhancement-ratio method (script 15).
#
# Input : 2020neiMar_county_tribe_allsector.zip  (contains esg_cty_sector_*.csv;
#         EPA "2020 NEI Supporting Data and Summaries", All Sectors)
# Output: prints CO by county & sector; writes nei_co_metro.csv.
#
# Reproduces E_CO_DENVER = 291.5 Gg CO/yr (seven-county metro, 2020).
# CAVEAT: the seven counties (~11,800 km2) are ~4x the analysis box, so this is
# an UPPER BOUND on box CO; a box-consistent value needs gridded (GRA2PES) CO.
# Run:  Rscript scripts/17_nei_co_metro.R /path/to/2020neiMar_county_tribe_allsector.zip
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
if (file.exists(file.path(proj, "config.R"))) source(file.path(proj, "config.R"))
OUT <- if (exists("OUT_DIR")) OUT_DIR else "."
args <- commandArgs(trailingOnly = TRUE)

# Accept the archive, the unpacked folder, or the CSV itself, and look in the places
# config.R and run_local.sh actually point at. This used to be a bare relative filename,
# so it only worked when run from whichever directory happened to hold the zip — the same
# failure that kept script 16 from finding the Vulcan raster.
src <- local({
  cands <- c(if (length(args)) args[1],
             if (exists("NEI_ZIP")) NEI_ZIP,
             if (exists("NEI_ZIP")) sub("[.]zip$", "", NEI_ZIP),
             if (exists("INV_DIR")) file.path(INV_DIR, c("2020neiMar_county_tribe_allsector.zip",
                                                        "2020neiMar_county_tribe_allsector")),
             if (exists("DATA_DIR")) file.path(dirname(DATA_DIR),
                                               "2020neiMar_county_tribe_allsector"),
             "2020neiMar_county_tribe_allsector.zip", "2020neiMar_county_tribe_allsector")
  hit <- cands[nzchar(cands) & file.exists(cands)]
  if (!length(hit))
    stop("EPA 2020 NEI county-by-sector data not found.\n",
         "Expected 2020neiMar_county_tribe_allsector.zip (or its unpacked folder) in ",
         if (exists("INV_DIR")) INV_DIR else "the inventory directory", ".\n",
         "Pass it explicitly:  Rscript scripts/17_nei_co_metro.R /path/to/it")
  hit[1]
})

if (dir.exists(src)) {                                   # unpacked folder
  csv_name <- list.files(src, pattern = "^esg_cty_sector.*[.]csv$", full.names = TRUE)[1]
  if (is.na(csv_name)) stop("no esg_cty_sector_*.csv inside ", src)
  con <- csv_name
} else if (grepl("[.]zip$", src, ignore.case = TRUE)) {  # archive
  csv_name <- grep("[.]csv$", unzip(src, list = TRUE)$Name, value = TRUE)[1]
  con <- unz(src, csv_name)
} else {                                                 # the CSV itself
  csv_name <- src
  con <- src
}
message("NEI source: ", src, "\n  table: ", basename(csv_name),
        " (large file; this read takes a few minutes)")
d <- read.csv(con, stringsAsFactors = FALSE, check.names = FALSE)

# seven Denver-metro county FIPS
metro <- c("08031"="Denver","08001"="Adams","08005"="Arapahoe","08013"="Boulder",
           "08014"="Broomfield","08035"="Douglas","08059"="Jefferson")
fips <- sprintf("%05d", as.integer(d[["fips code"]]))
co <- d[["pollutant desc"]] == "Carbon Monoxide" & fips %in% names(metro)
sub <- d[co, ]
SHORT_TO_METRIC <- 0.90718474            # NEI reports SHORT tons

by_cty <- tapply(sub[["total emissions"]], fips[co], sum)
by_sec <- sort(tapply(sub[["total emissions"]], sub[["sector"]], sum), decreasing = TRUE)
tot_short <- sum(sub[["total emissions"]])
Gg <- tot_short * SHORT_TO_METRIC / 1000

cat("EPA 2020 NEI CO, Denver-metro 7 counties (short tons/yr):\n")
for (f in names(sort(by_cty, decreasing = TRUE)))
  cat(sprintf("  %-10s %12.0f\n", metro[f], by_cty[f]))
cat(sprintf("  %-10s %12.0f short tons/yr\n", "TOTAL", tot_short))
cat("\nTop sectors (short tons/yr):\n")
for (s in names(head(by_sec, 6))) cat(sprintf("  %11.0f  %s\n", by_sec[s], s))
cat(sprintf("\nE_CO_DENVER = %.1f Gg CO/yr  (%.1f t/hr)\n", Gg, tot_short*SHORT_TO_METRIC/8766))

# First row carries the anchor itself (Gg CO/yr over the seven counties) together
# with the file and date it came from: EPA re-posts these summaries, so the number is
# only meaningful with its provenance. scripts/45_sync_anchors.R reads this row.
out <- rbind(
  data.frame(county = "SEVEN_COUNTY_TOTAL", short_tons_CO_yr = round(tot_short),
             Gg_CO_yr = round(Gg, 1), source_file = basename(src),
             retrieved = as.character(Sys.Date()), stringsAsFactors = FALSE),
  data.frame(county = unname(metro[names(by_cty)]),
             short_tons_CO_yr = round(as.numeric(by_cty)),
             Gg_CO_yr = NA_real_, source_file = basename(src),
             retrieved = as.character(Sys.Date()), stringsAsFactors = FALSE))
write.csv(out, file.path(OUT, "nei_co_metro.csv"), row.names = FALSE)
cat("wrote", file.path(OUT, "nei_co_metro.csv"), "\n")
