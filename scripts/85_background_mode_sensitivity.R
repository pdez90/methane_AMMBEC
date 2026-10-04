# 85_background_mode_sensitivity.R ------------------------------------------
# Sensitivity of the urban fossil fraction and the CH4:CO emission slope to how the
# aircraft background is defined (SI section S4).
#
# The paper's background is the rolling 5th percentile (300 samples) run over each
# flight's urban level legs JOINED END TO END (scripts 15 and 21): near a leg boundary
# the window spans the neighbouring leg, so the background is the cleanest air sampled
# on neighbouring urban legs and the broad urban enhancement is retained. The
# alternative evaluated here computes the same 300-sample background ALONG THE
# CONTINUOUS 1 Hz FLIGHT RECORD and then takes the urban-leg samples, which places the
# background within ~20 km of flight of every sample.
#
# For each flight with >= 3 urban legs (the Table 1 set) this writes, for both modes:
#   n_gated          urban-leg samples above the 20 ppb methane gate
#   fossil_pct       within-leg York fossil fraction at SOURCE_C2H6_CH4 (as Table 1)
#   r_co, E_co_t_hr  CH4:CO correlation and GRA2PES-anchored emission (as script 15)
#
# Out: <OUT_DIR>/background_mode_sensitivity.csv
# Run: Rscript scripts/85_background_mode_sensitivity.R      (base R, ~2 min)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths", "massbalance", "enhancements", "ratios"))
  source(file.path(proj, "R", paste0(f, ".R")))

MW_CH4 <- 16.04; MW_CO <- 28.01; GGYR_TO_THR <- 1e3 / (365.25 * 24)
GATE_PPB <- 20; CO_GATE_PPB <- 5

one_mode <- function(z) {
  k <- is.finite(z$CH4_ppb_enh) & is.finite(z$C2H6_ppb_enh) & z$CH4_ppb_enh > GATE_PPB
  s <- if (sum(k) >= 10) york_within_leg(z$CH4_ppb_enh[k], z$C2H6_ppb_enh[k], z$leg_id[k])$slope else NA_real_
  kco <- is.finite(z$CH4_ppb_enh) & is.finite(z$CO_ppb_enh) & z$CO_ppb_enh > CO_GATE_PPB
  rco <- if (sum(kco) >= 10) suppressWarnings(stats::cor(z$CO_ppb_enh[kco], z$CH4_ppb_enh[kco])) else NA_real_
  bco <- if (sum(kco) >= 10) unname(stats::coef(stats::lm(z$CH4_ppb_enh[kco] ~ z$CO_ppb_enh[kco]))[2]) else NA_real_
  c(n_gated = sum(k),
    fossil_pct = round(100 * fossil_fraction(s, SOURCE_C2H6_CH4)),
    r_co = round(rco, 2),
    E_co_t_hr = round(bco * (MW_CH4 / MW_CO) * E_CO_DENVER * GGYR_TO_THR, 2))
}

rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb", "C2H6_ppb", "CO_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  legs <- tag_region(leg_metrics(d)); urb <- legs$leg_id[legs$region == "urban"]
  du <- d[d$leg_id %in% urb, ]
  if (length(urb) < 3 || nrow(du) < 50) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  # paper: background over the joined urban legs
  a <- du; for (v in c("CH4_ppb", "C2H6_ppb", "CO_ppb")) a <- add_enhancements(a, v)
  # alternative: background along the continuous flight record, then the urban legs
  b <- d;  for (v in c("CH4_ppb", "C2H6_ppb", "CO_ppb")) b <- add_enhancements(b, v)
  b <- b[b$leg_id %in% urb, ]
  ja <- one_mode(a); tr <- one_mode(b)
  rows[[fl]] <- data.frame(flight = fl, urban_legs = length(urb),
    n_gated_joined = ja[["n_gated"]], n_gated_track = tr[["n_gated"]],
    fossil_pct_joined = ja[["fossil_pct"]], fossil_pct_track = tr[["fossil_pct"]],
    r_co_joined = ja[["r_co"]], r_co_track = tr[["r_co"]],
    E_co_joined_t_hr = ja[["E_co_t_hr"]], E_co_track_t_hr = tr[["E_co_t_hr"]],
    stringsAsFactors = FALSE)
  message(sprintf("%-18s joined: n %4d ff %3s%% r_co %5s | along-track: n %4d ff %3s%% r_co %5s",
                  fl, ja[["n_gated"]], ja[["fossil_pct"]], ja[["r_co"]], tr[["n_gated"]], tr[["fossil_pct"]], tr[["r_co"]]))
}
res <- do.call(rbind, rows)
write.csv(res, file.path(OUT_DIR, "background_mode_sensitivity.csv"), row.names = FALSE)
message(sprintf("\ncampaign median fossil fraction: joined-leg %.0f%%, along-track %.0f%% (flights with a fit: %d / %d)",
                median(res$fossil_pct_joined, na.rm = TRUE), median(res$fossil_pct_track, na.rm = TRUE),
                sum(is.finite(res$fossil_pct_joined)), sum(is.finite(res$fossil_pct_track))))
message("Wrote background_mode_sensitivity.csv")
