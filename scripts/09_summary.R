# 09_summary.R ---------------------------------------------------------------
# EXPLORATORY / NOT USED IN THE MANUSCRIPT. Merges the whole-flight script-02
# diagnostic with the script-05 curtain "flux" (neither is a manuscript quantity),
# and it also expects the pre-rename script-02 columns (c2h6_ch4_slope, fossil_frac)
# so it will not run against the current outputs. Excluded from run_all.R; retained
# for reference only. The manuscript's per-flight table is script 21 (Table 1).
#
# Combined per-flight results sheet: joins the ethane:methane source-apportionment
# output (script 02) with the mass-balance flux output (script 05) into one table,
# so you read one file instead of cross-referencing several. Also prints campaign
# aggregate statistics.
#
# Run:  Rscript scripts/09_summary.R   (after 02 and 05 have written their CSVs)
# Out:  <OUT_DIR>/combined_flight_summary.csv
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))

em_path <- file.path(OUT_DIR, "flight_ethane_methane.csv")
fx_path <- file.path(OUT_DIR, "massbalance_flux.csv")
if (!file.exists(em_path)) stop("Run script 02 first (", em_path, " missing).")
if (!file.exists(fx_path)) stop("Run script 05 first (", fx_path, " missing).")

em <- read.csv(em_path, stringsAsFactors = FALSE)
fx <- read.csv(fx_path, stringsAsFactors = FALSE)

# Normalize a join key (strip _L1/_L2/_R0 differences are kept — join on file).
m <- merge(
  em[, c("file","date","c2h6_ch4_slope","ratio_pct","r","fossil_frac",
         "ch4_bg_med","ch4_enh_max")],
  fx[, c("flight","n_legs_total","n_legs_used","leg_ids_used","background_ppb",
         "blh_m","blh_src","wind_src","flux_kg_hr","flux_t_hr")],
  by.x = "file", by.y = "flight", all = TRUE)

m <- m[order(m$date, m$file), ]
names(m)[names(m) == "file"] <- "flight"
write.csv(m, file.path(OUT_DIR, "combined_flight_summary.csv"), row.names = FALSE)

# ---- campaign aggregate statistics ----
f <- m[is.finite(m$fossil_frac), ]
q <- function(x) round(stats::quantile(x, c(0,.25,.5,.75,1), na.rm=TRUE), 3)
cat("\n=== Combined per-flight summary (", nrow(m), " flights) ===\n", sep="")
cat("Ethane:methane slope  (min/Q1/med/Q3/max):", q(m$c2h6_ch4_slope), "\n")
cat("Fossil fraction       (min/Q1/med/Q3/max):", q(m$fossil_frac), "\n")
cat("Mass-balance flux t/hr(min/Q1/med/Q3/max):", q(m$flux_t_hr), "\n")
cat("Flights on profiler winds:", sum(m$wind_src == "profiler", na.rm=TRUE),
    " | BLH from lidar:", sum(m$blh_src == "config", na.rm=TRUE),
    " | curated legs:", sum(m$n_legs_used < m$n_legs_total, na.rm=TRUE), "\n")
cat("\nWrote ", file.path(OUT_DIR, "combined_flight_summary.csv"), "\n", sep="")
print(m[, c("flight","date","fossil_frac","flux_t_hr","wind_src","blh_src")], row.names = FALSE)
