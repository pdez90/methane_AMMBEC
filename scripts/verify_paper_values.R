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
# UPDATED 12 Sep 2026 with the NEI re-derivation: E_CO_NEI 291.5 -> 292.7 (script 17
# against the EPA file as posted that day), so E_CO_NEI_BOX 227.1 -> 228.0 and the
# box-consistent NEI range 8.6-20.0 -> 8.7-20.1, median 14.2 -> 14.3. The GRA2PES- and
# Vulcan-anchored numbers are unchanged. Any value edited here must also change in the
# manuscript, or this check stops being a check.
checks <- list(
  list("E_CO",           getnum("E_CO"),            121.5),
  list("E_CO2",          getnum("E_CO2"),           23622.3),
  list("E_CH4_GRA2PES",  getnum("E_CH4_GRA2PES"),   1.69),
  list("E_CO_NEI_BOX",   getnum("E_CO_NEI_BOX"),    228.0),
  list("gra_lo",         getnum("gra_lo"),          4.61),
  list("gra_hi",         getnum("gra_hi"),          10.71),
  list("gra_median",     getnum("gra_median"),      7.605),
  list("nei_box_lo",     getnum("nei_box_lo"),      8.7),
  list("nei_box_hi",     getnum("nei_box_hi"),      20.1),
  # Section 2.4 quotes the lidar mixing-height range across the eight urban
  # flights. Emitted by script 21 from curtain_config.csv (filled by script 08).
  list("blh_valid_lo",   getnum("blh_valid_lo"),    0.352),
  list("blh_valid_hi",   getnum("blh_valid_hi"),    3.352),
  # Section 5 endmember paragraph: campaign median and maximum urban ethane:methane
  # slope, and the median fossil fraction under the delivered-gas endmember (Plant
  # et al. 2019 six-city mean, config.R). Emitted by script 21 from script 15 slopes.
  # These depend on the fossil-fraction ESTIMATOR (config.R FOSSIL_ESTIMATOR), so they
  # are pinned per estimator below, not here.
  NULL
)
checks <- Filter(Negate(is.null), checks)

# --- estimator-dependent anchors ----------------------------------------------
# "pooled": the submitted numbers (one York slope per flight; 2 of 7 clamp to 0%).
# "within": the within-leg York estimator adopted 12 Sep 2026 (scripts 48/49). Its
# values are pinned from the first full run under that estimator; NA means "not yet
# pinned" and FAILS on purpose, so a number can never be quoted before it is pinned.
getstr <- function(key) {
  m <- regmatches(raw, regexpr(sprintf('"%s"\\s*:\\s*"[^"]*"', key), raw))
  if (!length(m)) return(NA_character_)
  sub('.*:\\s*"([^"]*)"', '\\1', m)
}
est_json <- getstr("fossil_estimator")
if (is.na(est_json)) est_json <- "pooled"          # paper_values.json predates the switch
if (est_json != FOSSIL_ESTIMATOR)
  cat(sprintf("\n  NOTE: paper_values.json was written under estimator '%s' but config.R now\n",
              est_json), "  selects '", FOSSIL_ESTIMATOR, "'; checking against '", est_json,
      "'. Re-run scripts 15 and 21 to switch.\n", sep = "")
est_expect <- list(
  pooled = list(median_urban_slope = 0.0247, max_urban_slope = 0.0578,
                fossil_median_pipeline_mean = 92, fossil_median = 24),
  # Pinned 13 Sep 2026 from the first full run under the within-leg estimator
  # (run_local.sh, all stages; Table 1: 29, 20, NA, 2, 40, 4, 29, 47).
  within = list(median_urban_slope = 0.0293, max_urban_slope = 0.048,
                fossil_median_pipeline_mean = 100, fossil_median = 29)
)[[est_json]]
for (k in names(est_expect))
  checks[[length(checks) + 1]] <- list(paste0(k, " [", est_json, "]"), getnum(k), est_expect[[k]])

fail <- 0L
cat("== anchor checks ==\n")
for (c in checks) {
  key <- c[[1]]; got <- c[[2]]; exp <- c[[3]]
  if (is.na(exp)) {
    fail <- fail + 1L
    cat(sprintf("  %-4s %-40s got=%s expected=UNPINNED (pin it in verify_paper_values.R after checking the run)\n",
                "DIFF", key, format(got)))
    next
  }
  ok <- is.finite(got) && abs(got - exp) < 1e-6
  if (!ok) fail <- fail + 1L
  cat(sprintf("  %-4s %-40s got=%s expected=%s\n",
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
