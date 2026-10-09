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
# Values that depend on how level legs are defined are pinned for both definitions
# (config.R LEG_METHOD): P(vspeed, transect). "transect" (constant-altitude transects,
# clipped to the box; adopted 8 Oct 2026) is the primary analysis; "vspeed" is the
# original avionics-vertical-speed detector, kept as a sensitivity.
P <- function(vspeed, transect) if (identical(LEG_METHOD, "vspeed")) vspeed else transect
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
  list("gra_lo",         getnum("gra_lo"),          P(4.61, 4.44)),
  list("gra_hi",         getnum("gra_hi"),          P(10.71, 10.27)),
  list("gra_median",     getnum("gra_median"),      P(7.605, 6.455)),
  list("nei_box_lo",     getnum("nei_box_lo"),      P(8.7, 8.3)),
  list("nei_box_hi",     getnum("nei_box_hi"),      P(20.1, 19.3)),
  # Section 2.4 quotes the lidar mixing-height range across the eight urban
  # flights. Emitted by script 21 from curtain_config.csv (filled by script 08).
  list("blh_valid_lo",   getnum("blh_valid_lo"),    0.352),
  list("blh_valid_hi",   getnum("blh_valid_hi"),    P(3.352, 4.752)),
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
  within = list(median_urban_slope = P(0.0293, 0.0211), max_urban_slope = P(0.048, 0.036), fossil_median = P(29, 21),
                fossil_min = P(2, 0), fossil_max = P(47, 35), ci_below50 = P(5, 6))
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

# --- headline values quoted in the text, pinned from their one-row summary CSVs ---------------
# (4 Oct 2026). Each entry: file, how to extract the value, expected value, tolerance.
csvchk <- list(
  list("beta_breakeven.csv",              function(d) round(d$beta_breakeven[1], 3),                   P(0.059, 0.042), 0),
  list("basin_summary.csv",               function(d) d$screened_median[1],                             P(0.03626, 0.0316), 1e-5),
  list("basin_summary.csv",               function(d) d$median_min3_legs[1],                            P(0.0314, 0.0316), 1e-5),
  list("basin_summary.csv",               function(d) round(100 * d$implied_og_share_using_ARC[1]),    P(45, 39), 0),
  list("two_endmember_flights.csv",       function(d) d$median_fossil_pct[grepl("^distribution only, DENVER", d$case)], P(27, 19), 0),
  list("two_endmember_flights.csv",       function(d) d$median_fossil_pct[grepl("2024 ARC ground ratio", d$case)],      P(36, 26), 0),
  list("two_endmember_flights.csv",       function(d) d$median_fossil_pct[grepl("assumed lean basin ratio 0.061", d$case)], P(48, 35), 0),
  list("two_endmember_flights.csv",       function(d) d$n_majority[grepl("assumed lean basin ratio 0.061", d$case)],    P(2, 3), 0),
  list("two_endmember_breakeven_eastcoast.csv", function(d) round(range(d$p_dist_breakeven[d$beta_djb > 0.07]), 2), P(c(0.36, 0.66), c(0.62, 0.91)), 0),
  list("arc_ground_summary.csv",          function(d) round(as.numeric(d$value[d$quantity == "ambient_slope_median_window_urban"]), 3), 0.023, 0),
  list("arc_ground_summary.csv",          function(d) round(as.numeric(d$value[d$quantity == "ambient_slope_median_window_basin"]), 3), 0.074, 0),
  list("spatial_match_summary.csv",       function(d) d$aircraft_median_ff[1],                           0.34, 0),
  list("spatial_match_summary.csv",       function(d) as.numeric(strsplit(d$aircraft_median_ff_lo_hi[1], " to ")[[1]]), c(0.31, 0.42), 0),
  list("wind_fossil.csv",                 function(d) round(stats::cor(d$pct_from_NE, d$fossil_pct, use = "complete.obs"), 2), P(0.35, 0.46), 0),
  list("qc_robustness.csv",               function(d) max(abs(d$delta), na.rm = TRUE), P(3, 17), 0),
  list("background_mode_sensitivity.csv", function(d) median(d$fossil_pct_track, na.rm = TRUE),           P(25, 25.5), 0),
  list("facility_separation_scan.csv",    function(d) sum(d$separable),                                   6, 0),
  list("facility_separation_scan_rolling.csv", function(d) sum(d$separable),                              3, 0),
  list("two_endmember_floor_flights.csv",  function(d) round(100 * unlist(d[d$flight == "20240713_R0_L1", c("p_dist_max_0.061_ec_lo", "p_dist_max_0.061_ec_hi", "p_dist_max_0.0813_ec_lo", "p_dist_max_0.0813_ec_hi")])), P(c(16, 27, 43, 60), c(41, 72, 60, 85)), 0),
  list("two_endmember_floor_flights.csv",  function(d) round(100 * range(d[d$flight != "20240713_R0_L1", c("p_dist_max_0.061_ec_lo", "p_dist_max_0.061_ec_hi")])), P(c(34, 100), c(42, 100)), 0),
  list("v2_pointsource_hotspots.csv",      function(d) d$carbonmapper_kg_hr[d$hotspot_id %in% 1:2],         c(102, 420), 0))
cat("\n== headline values from the summary CSVs ==\n")
for (c in csvchk) {
  f <- file.path(OUT, c[[1]])
  if (!file.exists(f)) { fail <- fail + 1L; cat(sprintf("  DIFF %-45s missing\n", c[[1]])); next }
  got <- tryCatch(c[[2]](read.csv(f, stringsAsFactors = FALSE)), error = function(e) NA)
  ok <- length(got) == length(c[[3]]) && all(is.finite(got)) && all(abs(got - c[[3]]) <= c[[4]] + 1e-9)
  if (!ok) fail <- fail + 1L
  cat(sprintf("  %-4s %-45s got=%s expected=%s\n", if (ok) "ok" else "DIFF", c[[1]],
              paste(format(got), collapse = ","), paste(format(c[[3]]), collapse = ",")))
}

cat(sprintf("\n%s (%d check(s) failed)\n", if (fail == 0) "ALL CHECKS PASSED" else "CHECKS FAILED", fail))
quit(status = if (fail == 0) 0 else 1)
