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
args <- commandArgs(trailingOnly = TRUE)
zip  <- if (length(args)) args[1] else "2020neiMar_county_tribe_allsector.zip"
csv_name <- grep("\\.csv$", unzip(zip, list = TRUE)$Name, value = TRUE)[1]
d <- read.csv(unz(zip, csv_name), stringsAsFactors = FALSE, check.names = FALSE)

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

out <- data.frame(county = metro[names(by_cty)],
                  short_tons_CO_yr = round(as.numeric(by_cty)))
write.csv(out, "nei_co_metro.csv", row.names = FALSE)
