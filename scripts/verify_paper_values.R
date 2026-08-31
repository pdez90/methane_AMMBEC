# verify_paper_values.R --------------------------------------------------------
# Reproducibility check for the emission anchors, in base R (no Python, no extra
# packages). Reads the pipeline's own outputs and confirms that every headline
# anchor matches the value the code produces, and that no number fell back to a
# hardcoded default. Run AFTER run_all.R:
#   Rscript scripts/verify_paper_values.R
# Exit status is 0 when every check passes, 1 otherwise, so it can gate a commit.
# ------------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
OUT <- OUT_DIR

# --- minimal JSON reader: pull "key": number pairs from paper_values.json -----
pv_path <- file.path(OUT, "paper_values.json")
if (!file.exists(pv_path)) stop("paper_values.json not found; run scripts/21_tables.R first.")
raw <- paste(readLines(pv_path, warn = FALSE), collapse = " ")
getnum <- function(key) {
  m <- regmatches(raw, regexpr(sprintf('"%s"\\s*:\\s*-?[0-9.]+', key), raw))
  if (!length(m)) return(NA_real_)
  as.numeric(sub('.*:\\s*', '', m))
}
getbool <- function(key) {
  m <- regmatches(raw, regexpr(sprintf('"%s"\\s*:\\s*(true|false)', key), raw))
  if (!length(m)) return(NA) else grepl("true", m)
}

# --- expected values: the anchors the manuscript quotes -----------------------
checks <- list(
  list("E_CO",           getnum("E_CO"),            121.5),
  list("E_CO2",          getnum("E_CO2"),           23622.3),
  list("E_CH4_GRA2PES",  getnum("E_CH4_GRA2PES"),   1.69),
  list("E_CO_NEI_BOX",   getnum("E_CO_NEI_BOX"),    227.1),
  list("gra_lo",         getnum("gra_lo"),          4.61),
  list("gra_hi",         getnum("gra_hi"),          10.71),
  list("gra_median",     getnum("gra_median"),      7.605),
  list("nei_box_lo",     getnum("nei_box_lo"),      8.6),
  list("nei_box_hi",     getnum("nei_box_hi"),      20),
  # Section 2.4 quotes the lidar mixing-height range across the eight urban
  # flights. Emitted by script 21 from curtain_config.csv (filled by script 08).
  list("blh_valid_lo",   getnum("blh_valid_lo"),    0.352),
  list("blh_valid_hi",   getnum("blh_valid_hi"),    3.352),
  # Section 5 endmember paragraph: campaign median and maximum urban ethane:methane
  # slope, and the median fossil fraction under the delivered-gas endmember (Plant
  # et al. 2019 six-city mean, config.R). Emitted by script 21 from script 15 slopes.
  list("median_urban_slope", getnum("median_urban_slope"), 0.0247),
  list("max_urban_slope",    getnum("max_urban_slope"),    0.0578),
  list("fossil_median_pipeline_mean", getnum("fossil_median_pipeline_mean"), 92)
)
fail <- 0L
cat("== anchor checks ==\n")
for (c in checks) {
  key <- c[[1]]; got <- c[[2]]; exp <- c[[3]]
  ok <- is.finite(got) && abs(got - exp) < 1e-6
  if (!ok) fail <- fail + 1L
  cat(sprintf("  %-4s %-16s got=%s expected=%s\n",
              if (ok) "ok" else "DIFF", key, format(got), format(exp)))
}

# --- provenance flags: nothing may have used a hardcoded fallback -------------
cat("\n== provenance (must be TRUE: value came from a script, not a default) ==\n")
for (key in c("vulcan_cells_measured", "max_ethane_slope_measured")) {
  b <- getbool(key)
  if (!isTRUE(b)) fail <- fail + 1L
  cat(sprintf("  %-4s %s = %s\n", if (isTRUE(b)) "ok" else "DIFF", key, b))
}

# --- cross-check E_CO2 against the Vulcan CSV the pipeline wrote ---------------
vp <- file.path(OUT, "vulcan_co2_boxsum.csv")
if (file.exists(vp)) {
  v <- read.csv(vp, stringsAsFactors = FALSE)
  d <- abs(v$Gg_CO2_yr[1] - E_CO2_DENVER)
  ok <- d < 0.5
  if (!ok) fail <- fail + 1L
  cat(sprintf("\n== Vulcan CSV vs config ==\n  %-4s vulcan_co2_boxsum.csv = %.1f Gg/yr, config E_CO2_DENVER = %.1f (%d cells)\n",
              if (ok) "ok" else "DIFF", v$Gg_CO2_yr[1], E_CO2_DENVER, v$box_cells_km2[1]))
} else {
  cat("\n  NOTE: vulcan_co2_boxsum.csv absent; script 16 did not run.\n"); fail <- fail + 1L
}

cat(sprintf("\n%s (%d check(s) failed)\n", if (fail == 0) "ALL CHECKS PASSED" else "CHECKS FAILED", fail))
quit(status = if (fail == 0) 0 else 1)
