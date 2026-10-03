# 71_arc_ground_ethane.R ------------------------------------------------------
# NOAA ARL Air Resources Car (ARC) ground ethane:methane ratios, AMMBEC 2024.
#
# The ARC drove on 13 days (24 Jun to 15 Jul 2024) carrying an Aeris MIRA Ultra
# (CH4 + C2H6 at 1 Hz), a Picarro G2401-m (CO2/CH4/CO/H2O), a G2201i (d13C-CH4),
# and NOx/O3/NH3/BC/PM, with differential GPS and winds (PI: Xinrong Ren, NOAA
# ARL). The manuscript quotes one number from this platform -- the 0.0813 mol/mol
# production-area ratio in config.R -- taken from the AMMBEC final report to
# CDPHE rather than derived from the files. This script derives ground ratios
# from the files themselves, separately for the Denver-metro box and the DJB.
#
# Method, matching the aircraft treatment as closely as a road dataset allows:
#   1. Rolling-quantile background per day (R/enhancements.R), on CH4 and C2H6.
#   2. Plumes = contiguous runs where the CH4 enhancement exceeds both 3 sigma
#      (robust, from the day's quiet residuals) and ARC_GATE_PPB. Runs separated
#      by less than ARC_GAP_S are one plume; plumes shorter than ARC_MIN_PTS are
#      dropped. A plume is assigned to a region by its own mean position.
#   3. One within-plume (fixed-effects) York slope per day and region, with a
#      plume-block bootstrap interval -- the ground analogue of the within-leg
#      estimator used for the flights (R/ratios.R).
# A road vehicle samples individual facilities at close range, so these ratios
# describe the sources it drove past, not a boundary-layer aggregate. They are
# source-composition context, not a replacement for the airborne split.
#
# Run:  Rscript scripts/71_arc_ground_ethane.R [ARC_DIR]
#       METHANE_ARC_DIR=<dir> overrides; .zip archives are read in place.
# Out:  <OUT_DIR>/arc_ground_plumes.csv   one row per plume
#       <OUT_DIR>/arc_ground_ratios.csv   day x region slopes
#       <OUT_DIR>/arc_ground_coverage.csv where each day drove
#       <OUT_DIR>/arc_ground_summary.csv  campaign-level numbers for the text
#       <OUT_DIR>/figures/FigS15_arc_ground.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R"))
source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R"))

args <- commandArgs(trailingOnly = TRUE)
ARC_DIR <- if (length(args)) args[1] else Sys.getenv("METHANE_ARC_DIR", unset = "")
if (!nzchar(ARC_DIR)) {
  ARC_DIR <- file.path(dirname(DATA_DIR), "instruments", "ARC")
  for (.c in c(file.path(DATA_DIR, "ARC"), ARC_DIR)) if (dir.exists(.c)) { ARC_DIR <- .c; break }
}
if (!dir.exists(ARC_DIR)) stop("ARC_DIR not found: ", ARC_DIR, "  (set METHANE_ARC_DIR)")

# Tunables (env-overridable so the sensitivity can be shown without editing code).
.num <- function(v, d) { x <- suppressWarnings(as.numeric(Sys.getenv(v))); if (is.finite(x)) x else d }
ARC_BG_WINDOW <- .num("METHANE_ARC_BG_WINDOW", 300)  # s, rolling background
ARC_BG_Q      <- .num("METHANE_ARC_BG_Q", 0.05)
ARC_GATE_PPB  <- .num("METHANE_ARC_GATE_PPB", 50)    # absolute CH4 enhancement gate
ARC_NSIGMA    <- .num("METHANE_ARC_NSIGMA", 3)
ARC_GAP_S     <- .num("METHANE_ARC_GAP_S", 60)       # merge plumes closer than this
ARC_MIN_PTS   <- .num("METHANE_ARC_MIN_PTS", 10)     # points per usable plume
ARC_METRO_LAT <- .num("METHANE_ARC_METRO_LAT", 39.80)  # city proper vs northern fringe
AIRCRAFT_GATE_PPB <- .num("METHANE_ARC_AMBIENT_GATE_PPB", 20)  # the aircraft plume gate, for the ambient fit
# The AMMBEC flights ran 28 June to 13 July; every "within the flight window" number
# in this script uses these two dates and nothing else.
WINDOW <- c("20240628", "20240713")
in_window <- function(d) d >= WINDOW[1] & d <= WINDOW[2]

# CALIBRATION SCREEN. The ARC runs zero-air and span-gas cycles through the analysers
# while parked. Both analysers see the cylinder, so the CH4 difference between them is
# no discriminator; the signature is instead a stationary period during which the
# Aeris reads zero air (CH4 below ~1.5 ppm, C2H6 exactly 0) or the span cylinder
# (~3.4 ppm CH4 with ~120-140 ppb C2H6). The rolling background then drops toward
# zero and the cylinder reads as a 3,000+ ppb "plume" with a tight ethane slope, so
# a cycle left in the record is indistinguishable from a real ethane-bearing source.
# The stretch of each parked period from the first cylinder sample to the last is
# removed, plus a margin for the flush ramps on either side. The R1 files say some zeroing periods "slipped
# through R0"; this screen also catches what is left in the RB and R0 files.
CAL_STATIONARY_KMH <- .num("METHANE_ARC_CAL_SPEED_KMH", 1)
CAL_ZERO_CH4_PPM   <- .num("METHANE_ARC_CAL_ZERO_CH4_PPM", 1.5)
CAL_SPAN_CH4_PPM   <- c(.num("METHANE_ARC_CAL_SPAN_LO_PPM", 3.2), .num("METHANE_ARC_CAL_SPAN_HI_PPM", 3.6))
CAL_SPAN_C2H6_PPB  <- c(.num("METHANE_ARC_CAL_C2H6_LO_PPB", 100), .num("METHANE_ARC_CAL_C2H6_HI_PPB", 160))
CAL_MARGIN_S       <- .num("METHANE_ARC_CAL_MARGIN_S", 120)

cal_flag <- function(d) {
  tsec <- as.numeric(d$timestamp)
  stat <- is.finite(d$speed_kmh) & d$speed_kmh < CAL_STATIONARY_KMH
  zero <- (is.finite(d$CH4_ppb) & d$CH4_ppb < CAL_ZERO_CH4_PPM * 1000) |
          (is.finite(d$C2H6_ppb) & d$C2H6_ppb == 0)
  span <- is.finite(d$CH4_ppb) & d$CH4_ppb >= CAL_SPAN_CH4_PPM[1] * 1000 & d$CH4_ppb <= CAL_SPAN_CH4_PPM[2] * 1000 &
          is.finite(d$C2H6_ppb) & d$C2H6_ppb >= CAL_SPAN_C2H6_PPB[1] & d$C2H6_ppb <= CAL_SPAN_C2H6_PPB[2]
  # Zero air is never ambient, moving or not; the span signature only counts when parked.
  # The analyzer also ran cycles while driving (the transits of 24 June and 15 July), so the
  # stretch logic below is applied to every run, moving or parked, that holds a cylinder sample:
  # restricting it to parked runs lets span-gas pulses (~1.4 ppm CH4 with ~120 ppb C2H6, ratio
  # ~0.08) through as "ethane-bearing plumes", one of them inside the box on 24 June.
  hit <- zero | (span & stat)
  flag <- hit
  # Within each run (parked or moving) that contains a cycle, remove the stretch from the first
  # cylinder sample to the last, plus the flush margin either side. This catches the
  # ramps between zero air and span gas without discarding long ordinary stops (the car
  # sat 33 min near Broomfield on 9 July with an 11-s span check inside the stop).
  if (any(hit)) {
    run_id <- cumsum(c(TRUE, diff(stat) != 0 | diff(tsec) > 5))
    for (r in unique(run_id[hit])) {
      th <- tsec[hit & run_id == r]
      flag <- flag | (tsec >= min(th) - CAL_MARGIN_S & tsec <= max(th) + CAL_MARGIN_S)
    }
  }
  flag
}

# ---- read one day ------------------------------------------------------------
# Instrument serial numbers appear in the column names (ch4_aeris313_ppm), and
# they are not identical on every file, so columns are matched by pattern.
.col <- function(df, pattern) {
  hit <- grep(pattern, names(df), value = TRUE, ignore.case = TRUE)
  if (!length(hit)) NA_character_ else hit[1]
}

arc_files <- function(dir) {
  f <- list.files(dir, pattern = "ARC_\\d{8}_R.*\\.ict$", full.names = TRUE, ignore.case = TRUE)
  z <- list.files(dir, pattern = "\\.zip$", full.names = TRUE)
  list(ict = f, zip = z)
}

read_arc_day <- function(path) {
  # A zipped day is extracted to a session temp dir; the files ship one per zip.
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    td <- file.path(tempdir(), "arc", tools::file_path_sans_ext(basename(path)))
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
    inner <- utils::unzip(path, list = TRUE)$Name
    inner <- inner[grepl("\\.ict$", inner, ignore.case = TRUE)]
    if (!length(inner)) return(NULL)
    utils::unzip(path, files = inner[1], exdir = td, junkpaths = TRUE)
    path <- file.path(td, basename(inner[1]))
  }
  ic <- read_icartt(path)
  d  <- ic$data
  cn <- list(lat = .col(d, "^lat"), lon = .col(d, "^lon"),
             ch4 = .col(d, "^ch4_aeris"), c2h6 = .col(d, "^c2h6"),
             ch4p = .col(d, "^ch4_g2401"), co = .col(d, "^co_g2401"),
             co2 = .col(d, "^co2_g2401"), d13c = .col(d, "^delta13c_ch4"),
             spd = .col(d, "^speed"), ws = .col(d, "ws_m_s"), wd = .col(d, "wd_deg"))
  if (any(is.na(c(cn$lat, cn$lon, cn$ch4, cn$c2h6)))) return(NULL)
  out <- data.frame(
    date      = format(ic$meta$date, "%Y%m%d"),
    timestamp = d$timestamp,
    lat = d[[cn$lat]], lon = d[[cn$lon]],
    CH4_ppb  = d[[cn$ch4]] * 1000,          # Aeris reports ppm
    C2H6_ppb = d[[cn$c2h6]],
    CH4_picarro_ppb = if (is.na(cn$ch4p)) NA_real_ else d[[cn$ch4p]] * 1000,
    CO_ppb   = if (is.na(cn$co)) NA_real_ else d[[cn$co]] * 1000,
    CO2_ppm  = if (is.na(cn$co2)) NA_real_ else d[[cn$co2]],
    d13C_CH4 = if (is.na(cn$d13c)) NA_real_ else d[[cn$d13c]],
    speed_kmh = if (is.na(cn$spd)) NA_real_ else d[[cn$spd]],
    ws_m_s = if (is.na(cn$ws)) NA_real_ else d[[cn$ws]],
    wd_deg = if (is.na(cn$wd)) NA_real_ else d[[cn$wd]],
    stringsAsFactors = FALSE)
  out[is.finite(out$lat) & is.finite(out$lon) & out$lat != 0 & out$lon != 0, ]
}

# Region of each 1-s record. "urban" is the analysis box; "basin" is north of
# BASIN_LAT, the same split the flight legs use (config.R).
region_of <- function(lat, lon) {
  r <- rep("other", length(lat))
  r[lat >= BASIN_LAT] <- "basin"
  r[lat >= URBAN_BOX$lat_s & lat <= URBAN_BOX$lat_n &
    lon >= URBAN_BOX$lon_w & lon <= URBAN_BOX$lon_e] <- "urban"
  r
}

# ---- per-day processing ------------------------------------------------------
ff <- arc_files(ARC_DIR)
paths <- if (length(ff$ict)) ff$ict else ff$zip
if (!length(paths)) stop("No ARC .ict or .zip files in ", ARC_DIR)
paths <- sort(paths)
message("ARC files: ", length(paths), " in ", ARC_DIR)

plumes <- list(); coverage <- list(); records <- list(); ambient <- list()
for (p in paths) {
  d <- try(read_arc_day(p), silent = TRUE)
  if (inherits(d, "try-error") || is.null(d) || !nrow(d)) {
    message("  skip ", basename(p), " (no usable ethane/position record)"); next
  }
  day <- d$date[1]
  d <- d[order(d$timestamp), ]
  cal <- cal_flag(d)
  n_cal <- sum(cal)
  d <- d[!cal, ]
  if (nrow(d) < ARC_BG_WINDOW) { message("  skip ", day, " (too little record after the calibration screen)"); next }
  d$region <- region_of(d$lat, d$lon)

  # Backgrounds and the plume flag, on the day's full record.
  d <- add_enhancements(d, "CH4_ppb", window = ARC_BG_WINDOW, q = ARC_BG_Q, n_sigma = ARC_NSIGMA)
  sigma <- attr(d, "enh_sigma")
  d$C2H6_ppb_bg  <- rolling_baseline(d$C2H6_ppb, window = ARC_BG_WINDOW, q = ARC_BG_Q)
  d$C2H6_ppb_enh <- d$C2H6_ppb - d$C2H6_ppb_bg
  d$plume <- d$plume & is.finite(d$CH4_ppb_enh) & d$CH4_ppb_enh > ARC_GATE_PPB

  # Contiguous plume runs -> block ids (gaps shorter than ARC_GAP_S bridged).
  d$plume_id <- 0L
  idx <- which(d$plume)
  if (length(idx)) {
    tsec <- as.numeric(d$timestamp)
    brk <- c(TRUE, diff(tsec[idx]) > ARC_GAP_S)
    d$plume_id[idx] <- cumsum(brk)
  }

  # Coverage: where this day actually drove.
  inbox <- d$region == "urban"
  coverage[[day]] <- data.frame(
    date = day, n_records = nrow(d),
    n_urban = sum(inbox), n_urban_metro = sum(inbox & d$lat < ARC_METRO_LAT),
    n_basin = sum(d$region == "basin"), n_other = sum(d$region == "other"),
    lat_min = min(d$lat), lat_max = max(d$lat), lon_min = min(d$lon), lon_max = max(d$lon),
    ch4_sigma_ppb = round(sigma, 2), n_plumes = length(unique(d$plume_id[d$plume_id > 0])),
    n_calibration_removed = n_cal,
    stringsAsFactors = FALSE)

  # One row per plume: position, size, and its own ethane:methane slope.
  for (g in unique(d$plume_id[d$plume_id > 0])) {
    s <- d[d$plume_id == g, ]
    if (nrow(s) < ARC_MIN_PTS) next
    ok <- is.finite(s$CH4_ppb_enh) & is.finite(s$C2H6_ppb_enh)
    if (sum(ok) < ARC_MIN_PTS) next
    y <- york_slope(s$CH4_ppb_enh[ok], s$C2H6_ppb_enh[ok])
    reg <- names(which.max(table(s$region)))
    # Exploratory Keeling intercept (d13C vs 1/CH4) on the raw, uncalibrated
    # G2201i delta. Reported only where the plume is large and the fit tight.
    keel <- NA_real_
    kok <- ok & is.finite(s$d13C_CH4) & is.finite(s$CH4_ppb) & s$CH4_ppb > 0
    if (sum(kok) >= 30) {
      fit <- stats::lm(s$d13C_CH4[kok] ~ I(1 / s$CH4_ppb[kok]))
      if (summary(fit)$r.squared >= 0.5) keel <- unname(stats::coef(fit)[1])
    }
    plumes[[length(plumes) + 1]] <- data.frame(
      date = day, plume = g, region = reg,
      lat = mean(s$lat), lon = mean(s$lon),
      n = sum(ok), duration_s = as.numeric(difftime(max(s$timestamp), min(s$timestamp), units = "secs")),
      ch4_enh_max_ppb = max(s$CH4_ppb_enh, na.rm = TRUE),
      c2h6_enh_max_ppb = max(s$C2H6_ppb_enh, na.rm = TRUE),
      slope = y$slope, r = y$r,
      d13C_keeling_raw = keel,
      stringsAsFactors = FALSE)
  }
  records[[day]] <- d[d$plume_id > 0, c("date","region","plume_id","CH4_ppb_enh","C2H6_ppb_enh","lat","lon","d13C_CH4")]
  # Ambient fit: every gated sample, plume or not, in the style of an airborne
  # enhancement ratio. This is the ground analogue of the published 0.0813 mol/mol
  # production-area number -- it averages the ethane-bearing and ethane-free
  # sources the car drove through, so it sits below the plume-resolved ratios.
  amb <- d[is.finite(d$CH4_ppb_enh) & d$CH4_ppb_enh > AIRCRAFT_GATE_PPB &
             is.finite(d$C2H6_ppb_enh), ]
  for (reg in c("urban", "basin")) {
    sa <- amb[amb$region == reg, ]
    if (nrow(sa) < 30) next
    ya <- york_slope(sa$CH4_ppb_enh, sa$C2H6_ppb_enh)
    ambient[[length(ambient) + 1]] <- data.frame(
      date = day, region = reg, n = nrow(sa), slope = ya$slope, r = ya$r,
      ch4_enh_median_ppb = round(stats::median(sa$CH4_ppb_enh), 1), stringsAsFactors = FALSE)
  }
  message(sprintf("  %s: %d records (%d calibration s removed), urban %d (metro %d), basin %d, sigma %.1f ppb, %d plumes",
                  day, nrow(d), n_cal, sum(inbox), sum(inbox & d$lat < ARC_METRO_LAT),
                  sum(d$region == "basin"), sigma, coverage[[day]]$n_plumes))
}
if (!length(plumes)) stop("No usable plumes found in ", ARC_DIR)

PL <- do.call(rbind, plumes); CV <- do.call(rbind, coverage); RC <- do.call(rbind, records)
AM <- if (length(ambient)) do.call(rbind, ambient) else NULL
dir.create(file.path(OUT_DIR, "figures"), recursive = TRUE, showWarnings = FALSE)

# ---- plume classes -----------------------------------------------------------
# A road plume is one source at close range, so each plume is classified rather
# than averaged into the others. Where ethane tracks methane through the plume
# the ratio is identifiable (fossil-type); where methane rises with no ethane
# response the plume is ethane-free (biogenic-type: landfill, wastewater, dairy).
# Plumes that are neither are left unclassified rather than forced into one.
ARC_R_FOSSIL <- .num("METHANE_ARC_R_FOSSIL", 0.7)    # |r| of C2H6 on CH4
ARC_R_FREE   <- .num("METHANE_ARC_R_FREE", 0.3)
ARC_SLOPE_MIN <- .num("METHANE_ARC_SLOPE_MIN", 0.01) # mol/mol, ethane detectable
PL$class <- "unclassified"
PL$class[is.finite(PL$r) & PL$r >= ARC_R_FOSSIL & PL$slope >= ARC_SLOPE_MIN] <- "fossil-type"
PL$class[is.finite(PL$r) & abs(PL$r) < ARC_R_FREE & PL$slope < ARC_SLOPE_MIN] <- "ethane-free"
write.csv(PL, file.path(OUT_DIR, "arc_ground_plumes.csv"), row.names = FALSE)
write.csv(CV, file.path(OUT_DIR, "arc_ground_coverage.csv"), row.names = FALSE)

# Median fossil-type ratio per region, with a bootstrap interval over plumes.
boot_med <- function(v, B = 2000, seed = 42) {
  v <- v[is.finite(v)]; if (length(v) < 3) return(c(NA, NA, NA))
  old <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  m <- replicate(B, stats::median(sample(v, length(v), replace = TRUE)))
  c(stats::median(v), stats::quantile(m, 0.025, names = FALSE), stats::quantile(m, 0.975, names = FALSE))
}

# ---- day x region slopes (within-plume York + plume-block bootstrap) ---------
fit_set <- function(sub) {
  ok <- is.finite(sub$CH4_ppb_enh) & is.finite(sub$C2H6_ppb_enh)
  if (sum(ok) < ARC_MIN_PTS) return(NULL)
  b <- york_boot(sub$CH4_ppb_enh[ok], sub$C2H6_ppb_enh[ok], blocks = sub$plume_id[ok],
                 within = TRUE, B = 1000, min_pts = ARC_MIN_PTS)
  data.frame(n = b$n, n_plumes = b$n_blocks, n_plumes_fit = b$n_blocks_fit,
             slope = b$slope, lo = b$lo, hi = b$hi, stringsAsFactors = FALSE)
}

rows <- list()
for (day in sort(unique(RC$date))) for (reg in c("urban", "basin")) {
  sub <- RC[RC$date == day & RC$region == reg, ]
  f <- fit_set(sub); if (is.null(f)) next
  rows[[length(rows) + 1]] <- cbind(data.frame(date = day, region = reg, stringsAsFactors = FALSE), f)
}
for (reg in c("urban", "basin")) {
  sub <- RC[RC$region == reg, ]
  # Plume ids repeat across days, so make them unique before pooling.
  sub$plume_id <- as.integer(factor(paste(sub$date, sub$plume_id)))
  f <- fit_set(sub); if (is.null(f)) next
  rows[[length(rows) + 1]] <- cbind(data.frame(date = "ALL", region = reg, stringsAsFactors = FALSE), f)
}
RT <- do.call(rbind, rows)
RT[, c("slope","lo","hi")] <- round(RT[, c("slope","lo","hi")], 4)
write.csv(RT, file.path(OUT_DIR, "arc_ground_ratios.csv"), row.names = FALSE)

# ---- campaign summary --------------------------------------------------------
win <- in_window(CV$date)
med <- function(v) if (!length(v)) NA_real_ else round(stats::median(v, na.rm = TRUE), 4)
pl_urban <- PL[PL$region == "urban", ]; pl_basin <- PL[PL$region == "basin", ]
S <- data.frame(quantity = character(), value = character(), stringsAsFactors = FALSE)
add <- function(q, v) S[nrow(S) + 1, ] <<- list(q, as.character(v))
add("days_total", nrow(CV))
add("window", paste(WINDOW, collapse = " to "))
add("calibration_seconds_removed", sum(CV$n_calibration_removed))
add("days_in_flight_window", sum(win))
add("days_with_urban_records", sum(CV$n_urban > 0))
add("days_with_metro_records", sum(CV$n_urban_metro > 0))
add("days_in_window_with_metro_records", sum(win & CV$n_urban_metro > 0))
add("metro_days", paste(CV$date[CV$n_urban_metro > 0], collapse = " "))
add("urban_seconds_total", sum(CV$n_urban))
add("basin_seconds_total", sum(CV$n_basin))
for (reg in c("urban", "basin")) {
  s <- PL[PL$region == reg, ]
  fo <- s[s$class == "fossil-type", ]; fr <- s[s$class == "ethane-free", ]
  b <- boot_med(fo$slope)
  add(paste0("plumes_", reg), nrow(s))
  add(paste0("plumes_fossil_type_", reg), nrow(fo))
  add(paste0("plumes_ethane_free_", reg), nrow(fr))
  add(paste0("fossil_type_ratio_median_", reg), round(b[1], 4))
  add(paste0("fossil_type_ratio_ci_", reg), paste0(round(b[2], 4), " to ", round(b[3], 4)))
  add(paste0("fossil_type_ratio_iqr_", reg),
      paste(round(stats::quantile(fo$slope, c(0.25, 0.75), na.rm = TRUE), 4), collapse = " to "))
  add(paste0("ethane_free_ch4_max_median_ppb_", reg), round(med(fr$ch4_enh_max_ppb)))
  # Flight-window subset (28 Jun to 13 Jul), which is what the manuscript covers.
  fw <- fo[in_window(fo$date), ]
  add(paste0("fossil_type_ratio_median_window_", reg), med(fw$slope))
  add(paste0("plumes_fossil_type_window_", reg), nrow(fw))
}
for (reg in c("urban", "basin")) {
  r <- RT[RT$date == "ALL" & RT$region == reg, ]
  if (nrow(r)) {
    # Secondary diagnostic only: the pooled within-plume York slope is weighted by
    # methane variance, so a single very large ethane-free plume (e.g. the 73 ppm
    # feedlot plume of 4 July) pulls it toward zero. The plume-class numbers above
    # are the summary to quote.
    add(paste0("pooled_york_slope_", reg), r$slope)
    add(paste0("pooled_york_ci_", reg), paste0(r$lo, " to ", r$hi))
    add(paste0("pooled_n_plumes_", reg), r$n_plumes_fit)
  }
}
kf <- PL$d13C_keeling_raw[PL$class == "fossil-type"]; ke <- PL$d13C_keeling_raw[PL$class == "ethane-free"]
add("d13C_keeling_raw_median_fossil_type", med(kf))
add("d13C_keeling_raw_median_ethane_free", med(ke))
add("d13C_keeling_n_fossil_free", paste(sum(is.finite(kf)), sum(is.finite(ke))))
add("config_SOURCE_C2H6_CH4", SOURCE_C2H6_CH4)
add("config_SOURCE_C2H6_CH4_ARC", SOURCE_C2H6_CH4_ARC)
add("gate_ppb", ARC_GATE_PPB); add("ambient_gate_ppb", AIRCRAFT_GATE_PPB); add("bg_window_s", ARC_BG_WINDOW); add("metro_lat", ARC_METRO_LAT)
if (!is.null(AM)) {
  # medians and ranges are taken from the unrounded daily slopes; rounding is for the CSV only
  AMw <- AM; AMw$slope <- round(AMw$slope, 4); AMw$r <- round(AMw$r, 3)
  write.csv(AMw, file.path(OUT_DIR, "arc_ground_ambient.csv"), row.names = FALSE)
  for (reg in c("urban", "basin")) {
    v <- AM$slope[AM$region == reg & in_window(AM$date)]
    add(paste0("ambient_slope_median_window_", reg), if (length(v)) signif(stats::median(v), 6) else NA)
    add(paste0("ambient_slope_range_window_", reg),
        if (length(v)) paste(round(range(v), 4), collapse = " to ") else NA)
    add(paste0("ambient_n_days_", reg), length(v))
  }
}
write.csv(S, file.path(OUT_DIR, "arc_ground_summary.csv"), row.names = FALSE)

# ---- figure ------------------------------------------------------------------
png(file.path(OUT_DIR, "figures", "FigS15_arc_ground.png"), 1900, 900, res = 150)
op <- par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1))

cols <- c("fossil-type" = "#B2182B", "ethane-free" = "#1b7837", unclassified = "#BBBBBB")
# Clipped to the Front Range: the 24 June and 15 July records include long
# out-of-state transits, which would otherwise squash the panel of interest.
xl <- c(URBAN_BOX$lon_w - 0.5, URBAN_BOX$lon_e + 0.5)
yl <- c(URBAN_BOX$lat_s - 0.3, BASIN_LAT + 0.8)
inv <- PL$lon >= xl[1] & PL$lon <= xl[2] & PL$lat >= yl[1] & PL$lat <= yl[2]
plot(PL$lon[inv], PL$lat[inv], pch = 21, bg = cols[PL$class[inv]], col = "#00000066", asp = 1,
     xlim = xl, ylim = yl, cex = 0.6 + 1.6 * pmin(1, PL$ch4_enh_max_ppb[inv] / 2000),
     xlab = "Longitude", ylab = "Latitude",
     main = sprintf("ARC ground plumes (%d of %d shown)", sum(inv), nrow(PL)))
rect(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n, border = "#1b7837", lwd = 2)
abline(h = BASIN_LAT, lty = 2, col = "#2166AC")
abline(h = ARC_METRO_LAT, lty = 3, col = "#B2182B")
legend("topright", c(`fossil-type` = "ethane-bearing", `ethane-free` = "ethane-free", unclassified = "unclassified")[names(cols)], pt.bg = cols, pch = 21, bty = "n", cex = .8)

fo <- PL[PL$class == "fossil-type" & PL$region %in% c("urban", "basin"), ]
if (nrow(fo)) {
  grp <- factor(fo$region, levels = c("urban", "basin"))
  stripchart(fo$slope ~ grp, vertical = TRUE, method = "jitter", jitter = 0.12, pch = 19,
             col = c("#B2182B", "#2166AC"), ylim = range(c(0, fo$slope, SOURCE_C2H6_CH4), na.rm = TRUE),
             ylab = expression(Delta*C[2]*H[6]*":"*Delta*CH[4]~(mol~mol^-1)),
             main = "Ratio of ethane-bearing ground plumes")
  for (i in seq_along(levels(grp))) {
    v <- fo$slope[grp == levels(grp)[i]]
    segments(i - 0.25, stats::median(v), i + 0.25, stats::median(v), lwd = 3)
  }
  abline(h = SOURCE_C2H6_CH4, lty = 2); abline(h = SOURCE_C2H6_CH4_ARC, lty = 3, col = "#1b7837")
  legend("topleft", c(sprintf("adopted endmember %.3f", SOURCE_C2H6_CH4),
                      sprintf("AMMBEC production-area ratio %.4f", SOURCE_C2H6_CH4_ARC)),
         lty = c(2, 3), col = c("black", "#1b7837"), bty = "n", cex = .75)
}
par(op); dev.off()

message("\nWrote arc_ground_plumes.csv, arc_ground_ambient.csv, arc_ground_ratios.csv, arc_ground_coverage.csv, ",
        "arc_ground_summary.csv and figures/FigS15_arc_ground.png to ", OUT_DIR)
print(RT[RT$date == "ALL", ], row.names = FALSE)
