# 64_stilt_djb_exposure.R -----------------------------------------------------
# HRRR-driven STILT footprints for the 25 fitted urban legs, convolved with the
# gridded EPA GHGI by source class.  This is the transport-model upgrade of
# script 53: 53 scores each leg with a Gaussian upwind cone built from the
# aircraft's own wind; here every leg gets receptor-oriented Lagrangian
# footprints (HYSPLIT in STILT mode, 3-km HRRR meteorology, 10 h back), so the
# "exposure" to the DJ Basin is the inventory-predicted methane enhancement
# that the modelled transport delivers to the aircraft:
#
#     dCH4_class(receptor) = (1/Np) * sum_particles sum_t foot_p,t * F_class(x_p,t)
#
# foot is STILT's sensitivity (ppm per umol m-2 s-1) and F_class the GHGI flux
# (umol m-2 s-1) of a source class: DJB-type oil and gas (production,
# gathering/processing, transmission), urban gas distribution + post-meter,
# waste, livestock, and everything else.  Per leg:
#
#   f_og_stilt  = dCH4_og_basin / dCH4_total   (share of the predicted enhancement
#                                                that is basin oil and gas)
#   slope_pred  = (b_DJB dCH4_og + b_dist dCH4_ngdist) / dCH4_total
#
# Tests (Spearman, per leg): observed ethane:methane slope vs f_og_stilt, vs the
# absolute basin enhancement and vs slope_pred; observed vs predicted dCH4
# (a transport sanity check); HRRR vs aircraft wind direction; agreement with
# the script-53 cone exposure.  f_og_stilt is also recomputed with basin oil and
# gas doubled (GHGI is thought to under-count DJB production emissions).
# It REPORTS; nothing published changes.
#
# NOTE (24 Sep 2026): the macOS HYSPLIT 5.3 build on Priyanka's Mac crashes reading
# LANDUSE.ASC and then the HRRR header (Intel Fortran record/format errors). The 100
# receptors were therefore run with STILT's static Linux hycs_std (uataq/stilt bin/
# linux_x64, HYSPLIT 5.1 STILT fork) in a Linux VM on the same machine, with the
# land-use grids, reading the CONTROL/SETUP.CFG written here; this script then picks
# up the gzipped particle files. METHANE_STILT_ENGINE=external writes the run files
# without calling the local binary.
#
# Requirements (all local):
#   * HYSPLIT >= 5.2 with STILT emulation (hycs_std, xtrct_grid): ~/hysplit/exec
#     (override METHANE_HYSPLIT_EXEC; boundary files METHANE_HYSPLIT_BDY)
#   * HRRR 3-km ARL meteorology, 6-hour files YYYYMMDD_HH-HH_hrrr.  The script
#     downloads the ~15 files it needs from the NOAA ARL archive (about 3-4 GB
#     each), cuts each to a Colorado subdomain with xtrct_grid (~50 MB) and
#     deletes the full file unless METHANE_HRRR_KEEP_RAW=1.  Subgrids are kept in
#     <METHANE_HRRR_DIR>/sub (default <Methane_AMMBEC>/met/hrrr/sub) and reused.
#
#   Rscript scripts/64_stilt_djb_exposure.R --probe          # which archive URL answers?
#   Rscript scripts/64_stilt_djb_exposure.R --met-only       # download + subset only
#   Rscript scripts/64_stilt_djb_exposure.R                  # everything
#   Out: <OUT_DIR>/stilt_exposure_receptors.csv, stilt_exposure_legs.csv,
#        stilt_exposure_summary.csv, figures/FigS_stilt_exposure.png,
#        stilt_runs/<receptor>/particles.rds (resume cache)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "qc.R"))
suppressPackageStartupMessages({ library(ncdf4); library(parallel) })

ARGS <- commandArgs(trailingOnly = TRUE)
PROBE <- "--probe" %in% ARGS; MET_ONLY <- "--met-only" %in% ARGS; NO_DL <- "--no-download" %in% ARGS

## ---- settings ------------------------------------------------------------------
MIN_ENH <- 20; MIN_PTS_LEG <- 10; MIN_PTS_FLT <- 50          # identical to script 53
N_HOURS <- as.numeric(Sys.getenv("METHANE_STILT_HOURS", "-10"))
NUMPAR  <- as.integer(Sys.getenv("METHANE_STILT_NUMPAR", "500"))
N_REC   <- as.integer(Sys.getenv("METHANE_STILT_NREC", "4"))   # receptors per leg
N_CORES <- as.integer(Sys.getenv("METHANE_STILT_CORES", max(1, parallel::detectCores() - 1)))
DOMAIN  <- c(ymn = 37.5, xmn = -108.5, ymx = 42.5, xmx = -101.5)   # met subgrid + flux grid
BASIN_LAT <- 40.05                                                  # as scripts 11/53
OG_SCALE_SENS <- 2                                                  # sensitivity: basin O&G x2
B_DJB  <- SOURCE_C2H6_CH4
B_DIST <- if (exists("SOURCE_C2H6_CH4_DENVER_DELIVERED")) SOURCE_C2H6_CH4_DENVER_DELIVERED else 0.110

BASE <- Sys.getenv("METHANE_BASE", unset = dirname(normalizePath(OUT_DIR, mustWork = FALSE)))
EXEC <- path.expand(Sys.getenv("METHANE_HYSPLIT_EXEC", "~/hysplit/exec"))
BDY  <- path.expand(Sys.getenv("METHANE_HYSPLIT_BDY", file.path(dirname(EXEC), "bdyfiles")))
MET_DIR <- path.expand(Sys.getenv("METHANE_HRRR_DIR", file.path(BASE, "met", "hrrr")))
RAW_DIR <- file.path(MET_DIR, "raw"); SUB_DIR <- file.path(MET_DIR, "sub")
KEEP_RAW <- Sys.getenv("METHANE_HRRR_KEEP_RAW", "0") == "1"
RUN_DIR <- file.path(OUT_DIR, "stilt_runs")
FIG <- file.path(OUT_DIR, "figures")
for (d in c(RAW_DIR, SUB_DIR, RUN_DIR, FIG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Candidate locations of the NOAA ARL HRRR archive (first that answers is used).
# Override with METHANE_HRRR_URL=<base URL ending in />.
URL_BASES <- c(Sys.getenv("METHANE_HRRR_URL", ""),
               "https://www.ready.noaa.gov/data/archives/hrrr/",
               "ftp://ftp.arl.noaa.gov/archives/hrrr/",
               "https://noaa-oar-arl-hysplit-pds.s3.amazonaws.com/hrrr/",
               "https://noaa-oar-arl-hysplit-pds.s3.amazonaws.com/hrrr/%Y/")
URL_BASES <- URL_BASES[nzchar(URL_BASES)]

hav_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180; a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}
bearing <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180; dlon <- (lon2 - lon1) * p
  y <- sin(dlon) * cos(lat2 * p); x <- cos(lat1 * p) * sin(lat2 * p) - sin(lat1 * p) * cos(lat2 * p) * cos(dlon)
  (atan2(y, x) / p + 360) %% 360
}
ang_diff <- function(a, b) abs((a - b + 180) %% 360 - 180)
vmean_wd <- function(wd, ws) {
  ok <- is.finite(wd) & is.finite(ws); if (!any(ok)) return(c(NA, NA))
  u <- -ws[ok] * sin(wd[ok] * pi / 180); v <- -ws[ok] * cos(wd[ok] * pi / 180)
  c((atan2(-mean(u), -mean(v)) * 180 / pi + 360) %% 360, mean(ws[ok]))
}

## ---- 1. legs and receptors (same selection as script 53) --------------------------
.flights <- list_flights(DATA_DIR)
if (!length(.flights)) stop("no flight files under METHANE_DATA_DIR = ", DATA_DIR)
legs <- list(); recs <- list()
for (p in .flights) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb", "C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  lg <- tag_region(leg_metrics(d)); urb <- lg$leg_id[lg$region == "urban"]
  du <- d[d$leg_id %in% urb, ]; if (nrow(du) < MIN_PTS_FLT || length(urb) < 3) next
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  latc <- grep("^lat", names(du), ignore.case = TRUE, value = TRUE)[1]
  lonc <- grep("^lon", names(du), ignore.case = TRUE, value = TRUE)[1]
  for (g in urb) {
    s <- du[du$leg_id == g, ]
    k <- is.finite(s$CH4_ppb_enh) & is.finite(s$C2H6_ppb_enh) & s$CH4_ppb_enh > MIN_ENH
    if (sum(k) < MIN_PTS_LEG) next
    yk <- york_slope(s$CH4_ppb_enh[k], s$C2H6_ppb_enh[k], 1, 0.2)
    w  <- vmean_wd(s$WD, s$WS)
    legs[[length(legs) + 1]] <- data.frame(flight = fl, leg_id = g, n_gated = sum(k),
      agl_m = round(mean(s$ALTAGL, na.rm = TRUE)), lat = round(mean(s[[latc]], na.rm = TRUE), 4),
      lon = round(mean(s[[lonc]], na.rm = TRUE), 4), dch4_mean_ppb = round(mean(s$CH4_ppb_enh[k]), 1),
      slope = round(yk$slope, 5), r = round(suppressWarnings(cor(s$CH4_ppb_enh[k], s$C2H6_ppb_enh[k])), 2),
      wd_deg = round(w[1]), ws_ms = round(w[2], 1), stringsAsFactors = FALSE)
    # receptors: N_REC gated samples spread evenly through the gated part of the leg
    sk <- s[k & is.finite(s[[latc]]) & is.finite(s[[lonc]]) & is.finite(s$ALTAGL), ]
    idx <- unique(round(seq(1, nrow(sk), length.out = min(N_REC, nrow(sk)))))
    for (i in seq_along(idx)) {
      q <- sk[idx[i], ]
      recs[[length(recs) + 1]] <- data.frame(flight = fl, leg_id = g, rec = i,
        id = sprintf("%s_leg%02d_r%d", fl, g, i),
        run_time = as.POSIXct(round(as.numeric(q$timestamp) / 60) * 60, origin = "1970-01-01", tz = "UTC"),
        lati = round(q[[latc]], 5), long = round(q[[lonc]], 5), zagl = max(5, round(q$ALTAGL)),
        stringsAsFactors = FALSE)
    }
  }
}
L <- do.call(rbind, legs); R <- do.call(rbind, recs)
if (is.null(L) || nrow(L) < 5) stop("fewer than 5 fitted urban legs")
write.csv(transform(R, run_time = format(run_time, "%Y-%m-%d %H:%M", tz = "UTC")),
          file.path(OUT_DIR, "stilt_receptors_input.csv"), row.names = FALSE)
cat(sprintf("%d urban legs, %d receptors, %s to %s UTC\n", nrow(L), nrow(R),
            format(min(R$run_time), "%Y-%m-%d %H:%M"), format(max(R$run_time), "%Y-%m-%d %H:%M")))

## ---- 2. HRRR ARL files: which, download, subset -------------------------------------
hrrr_name <- function(t) {
  h0 <- (as.integer(format(t, "%H", tz = "UTC")) %/% 6) * 6
  sprintf("%s_%02d-%02d_hrrr", format(t, "%Y%m%d", tz = "UTC"), h0, h0 + 5)
}
files_for <- function(t) {
  tt <- seq(t + N_HOURS * 3600, t, by = 3600)
  tt <- c(tt, t, t + 3600)                       # the hour after t, for interpolation at file ends
  unique(vapply(tt, hrrr_name, ""))
}
SKIP_MISSING <- Sys.getenv("METHANE_STILT_SKIP_MISSING_MET", "0") == "1"
if (SKIP_MISSING) {        # use only receptors whose HRRR subgrids are already present (no download)
  has_met <- vapply(seq_len(nrow(R)), function(i) all(file.exists(file.path(SUB_DIR, files_for(R$run_time[i])))), TRUE)
  if (any(!has_met)) cat(sprintf("skipping %d receptors without local HRRR subgrids (flights: %s)\n", sum(!has_met),
                                 paste(unique(R$flight[!has_met]), collapse = ", ")))
  R <- R[has_met, ]; L <- L[paste(L$flight, L$leg_id) %in% paste(R$flight, R$leg_id), ]
  write.csv(transform(R, run_time = format(run_time, "%Y-%m-%d %H:%M", tz = "UTC")),
            file.path(OUT_DIR, "stilt_receptors_input.csv"), row.names = FALSE)
}
need <- sort(unique(unlist(lapply(R$run_time, files_for))))
cat(sprintf("HRRR files needed (%d): %s\n", length(need), paste(need, collapse = " ")))

url_for <- function(base, f) {
  d <- as.POSIXct(substr(f, 1, 8), format = "%Y%m%d", tz = "UTC")
  paste0(format(d, base, tz = "UTC"), f)
}
curl_head <- function(u) {
  out <- suppressWarnings(system2("curl", c("-sI", "-L", "--max-time", "30", shQuote(u)), stdout = TRUE, stderr = FALSE))
  st <- attr(out, "status"); len <- grep("^content-length", out, ignore.case = TRUE, value = TRUE)
  len <- suppressWarnings(as.numeric(gsub("\\D", "", tail(len, 1))))
  list(ok = is.null(st) && length(len) == 1 && is.finite(len) && len > 1e8, bytes = if (length(len)) len else NA)
}
if (PROBE) {
  for (b in URL_BASES) { h <- curl_head(url_for(b, need[1]))
    cat(sprintf("%-65s %s %s\n", url_for(b, need[1]), if (h$ok) "OK" else "no", if (is.finite(h$bytes)) sprintf("%.2f GB", h$bytes / 1e9) else "")) }
  quit(save = "no")
}

read_met_header <- function(path) {           # after STILT r/src/read_met_header.r (Fasoli)
  meta <- readChar(path, 166); header <- substring(meta, 51)
  g <- as.numeric(unlist(strsplit(gsub("(.{3})", "\\1_", substring(header, 94, 102)), "_")))
  list(nz = as.integer(g[3]))
}
BASE_OK <- NULL
get_raw <- function(f) {
  dest <- file.path(RAW_DIR, f)
  bases <- if (is.null(BASE_OK)) URL_BASES else c(BASE_OK, setdiff(URL_BASES, BASE_OK))
  for (b in bases) {
    u <- url_for(b, f); h <- curl_head(u); if (!h$ok) next
    if (file.exists(dest) && file.size(dest) == h$bytes) { BASE_OK <<- b; return(dest) }
    cat(sprintf("  downloading %s (%.1f GB) from %s\n", f, h$bytes / 1e9, sub("/[^/]*$", "/", u)))
    st <- system2("curl", c("-f", "-L", "-C", "-", "--retry", "5", "--retry-delay", "10", "-o", shQuote(dest), shQuote(u)))
    if (st == 0 && file.exists(dest) && file.size(dest) == h$bytes) { BASE_OK <<- b; return(dest) }
  }
  stop("could not download ", f, " from any of: ", paste(URL_BASES, collapse = ", "),
       "\n  Put the file in ", RAW_DIR, " by hand, or set METHANE_HRRR_URL.")
}
subset_met <- function(raw, sub) {
  wd <- tempfile("xtrct_"); dir.create(wd)
  file.symlink(file.path(EXEC, "xtrct_grid"), file.path(wd, "xtrct_grid"))
  stdin <- c(paste0(dirname(raw), "/"), basename(raw),
             paste(DOMAIN["ymn"], DOMAIN["xmn"]), paste(DOMAIN["ymx"], DOMAIN["xmx"]), read_met_header(raw)$nz)
  writeLines(stdin, file.path(wd, "input"))
  st <- system(paste("cd", shQuote(wd), "&& ./xtrct_grid < input > xtrct.log 2>&1"))
  if (st %in% c(137, 9)) stop("xtrct_grid was killed.", KILL_HINT)
  out <- file.path(wd, "extract.bin")
  if (!file.exists(out) || file.size(out) < 1e6) stop("xtrct_grid failed on ", raw, "; see ", file.path(wd, "xtrct.log"))
  file.copy(out, sub, overwrite = TRUE); unlink(wd, recursive = TRUE)
  invisible(sub)
}
if (!file.exists(file.path(EXEC, "hycs_std")) || !file.exists(file.path(EXEC, "xtrct_grid")))
  stop("HYSPLIT executables not found in ", EXEC, " (set METHANE_HYSPLIT_EXEC)")
# Preflight: macOS kills (SIGKILL, "Killed: 9") binaries it has quarantined or whose signature
# it rejects.  Catch that here, before any download, rather than after a 3 GB transfer.
KILL_HINT <- paste0("\n  macOS killed the HYSPLIT binary (\"Killed: 9\"). This is Gatekeeper, not memory.",
  "\n  Fix once, then rerun:\n    xattr -dr com.apple.quarantine ", dirname(EXEC),
  "\n    codesign --force -s - ", file.path(EXEC, "hycs_std"), " ", file.path(EXEC, "xtrct_grid"))
for (b in c("xtrct_grid", "hycs_std")) {
  wd <- tempfile("pre_"); dir.create(wd)
  st <- suppressWarnings(system(paste("cd", shQuote(wd), "&&", shQuote(file.path(EXEC, b)), "< /dev/null > log 2>&1"), timeout = 20))
  unlink(wd, recursive = TRUE)
  if (st %in% c(137, 9)) stop(b, " was killed on start-up.", KILL_HINT)
}
for (f in need) {
  sub <- file.path(SUB_DIR, f)
  if (file.exists(sub) && file.size(sub) > 1e6) {
    if (!KEEP_RAW && file.exists(file.path(RAW_DIR, f))) unlink(file.path(RAW_DIR, f))
    next
  }
  if (NO_DL && !file.exists(file.path(RAW_DIR, f))) stop("missing met ", f, " and --no-download set")
  raw <- if (file.exists(file.path(RAW_DIR, f)) && NO_DL) file.path(RAW_DIR, f) else get_raw(f)
  cat(sprintf("  subsetting %s -> %s\n", f, sub)); subset_met(raw, sub)
  if (!KEEP_RAW) unlink(raw)
}
cat("meteorology ready in ", SUB_DIR, "\n", sep = "")
if (MET_ONLY) quit(save = "no")

## ---- 3. STILT runs (HYSPLIT hycs_std, STILT emulation) ------------------------------
VARS <- c("time", "indx", "long", "lati", "zagl", "foot", "mlht", "dens", "samt", "sigw", "tlgr")
write_setup <- function(file) {   # STILT v2 run_stilt.r defaults (Fasoli et al. 2018; Lin et al. 2003)
  kv <- list(CAPEMIN = -1, CMASS = 0, CONAGE = 48, CPACK = 1, DELT = 1, DXF = 1, DYF = 1, DZF = 0.01,
    EFILE = "''", FRHMAX = 3, FRHS = 1, FRME = 0.1, FRMR = 0, FRTS = 0.1, FRVS = 0.01, HSCALE = 10800,
    ICHEM = 8, IDSP = 2, INITD = 0, IVMAX = length(VARS), K10M = 1, KAGL = 1, KBLS = 1, KBLT = 5, KDEF = 0,
    KHINP = 0, KHMAX = 9999, KMIX0 = 150, KMIXD = 3, KMSL = 0, KPUFF = 0, KRAND = 4, KRND = 6, KSPL = 1,
    KWET = 1, KZMIX = 0, MAXDIM = 1, MAXPAR = NUMPAR, MGMIN = 10, MHRS = 9999, NBPTYP = 1, NCYCL = 0,
    NDUMP = 0, NINIT = 1, NSTR = 0, NTURB = 0, NUMPAR = NUMPAR, NVER = 0, OUTDT = 0, P10F = 1,
    PINBC = "''", PINPF = "''", POUTF = "''", QCYCLE = 0, RHB = 80, RHT = 60, SPLITF = 1, TKERD = 0.18,
    TKERN = 0.18, TLFRAC = 0.1, TOUT = 0, TRATIO = 0.75, TVMIX = 1,
    VARSIWANT = paste0("'", paste(toupper(VARS), collapse = "', '"), "'"),
    VEGHT = 0.5, VSCALE = 200, VSCALEU = 200, VSCALES = -1, WBBH = 0, WBWF = 0, WBWR = 0, WVERT = ".FALSE.",
    WINDERRTF = 0, ZICONTROLTF = 0)
  writeLines(c("$SETUP", paste0(names(kv), "=", vapply(kv, function(v) format(v, scientific = FALSE), ""), ","), "$END"), file)
}
write_control <- function(r, met, file) {
  writeLines(c(format(r$run_time, "%y %m %d %H %M", tz = "UTC"), "1", paste(r$lati, r$long, r$zagl),
    N_HOURS, "0", "25000.0", length(met), as.vector(rbind(paste0(dirname(met), "/"), basename(met))),
    "1", "test", "1", "0.01", "00 00 00 00 00", "1", "0.0 0.0", "0.5 0.5", "30.0 30.0", "./", "cdump",
    "1", "100", "00 00 00 00 00", "00 00 00 00 00", "00 2 00", "1", "0.0 0.0 0.0",
    "0.0 0.0 0.0 0.0 0.0", "0.0 0.0 0.0", "0.0", "0.0"), file)
}
read_particles <- function(pf) {
  con <- if (grepl("\\.gz$", pf)) gzfile(pf) else file(pf); ln <- readLines(con); close(con)   # first line holds the receptor (lat lon z); keep only full records
  nf <- lengths(strsplit(trimws(ln), "[[:space:]]+"))
  if (!any(nf == length(VARS))) stop(pf, ": no line has ", length(VARS), " fields; first lines:\n", paste(head(ln, 3), collapse = "\n"))
  ln <- ln[nf == length(VARS) & !grepl("[A-DF-Za-df-z]", ln)]
  x <- read.table(text = ln, header = FALSE, colClasses = "numeric")
  if (ncol(x) != length(VARS)) stop(pf, ": ", ncol(x), " columns, expected ", length(VARS), " (", paste(VARS, collapse = " "), ")")
  names(x) <- VARS; x
}
plume_dilution <- function(p, r_zagl, veght = 0.5) {   # STILT calc_plume_dilution (hnf_plume = TRUE), base R
  p <- p[order(p$indx, -p$time), ]
  sig <- p$samt * sqrt(2) * p$sigw * sqrt(p$tlgr * abs(p$time * 60) + p$tlgr^2 * exp(-abs(p$time * 60) / p$tlgr) - 1)
  sig[!is.finite(sig)] <- 0
  plume <- r_zagl + ave(sig, p$indx, FUN = cumsum)
  near <- plume < veght * p$mlht
  p$foot[near] <- (0.02897 / (plume * p$dens) * p$samt * 60)[near]
  p
}
run_one <- function(i) {
  r <- R[i, ]; wd <- file.path(RUN_DIR, r$id); rds <- file.path(wd, "particles.rds")
  if (file.exists(rds)) return(rds)
  dir.create(wd, showWarnings = FALSE)
  # Particles computed elsewhere (e.g. the static Linux STILT build, see header note)
  # arrive as PARTICLE_STILT.DAT.gz in the run directory; use them instead of running.
  gz <- file.path(wd, "PARTICLE_STILT.DAT.gz")
  if (file.exists(gz)) {
    p <- read_particles(gz); if (is.null(p) || !nrow(p)) return(paste("EMPTY", r$id))
    p <- plume_dilution(p, r$zagl)
    saveRDS(p[, c("time", "indx", "long", "lati", "zagl", "foot", "mlht")], rds)
    unlink(gz); return(rds)
  }
  if (Sys.getenv("METHANE_STILT_ENGINE", "local") == "external") {
    write_setup(file.path(wd, "SETUP.CFG")); write_control(r, file.path(SUB_DIR, files_for(r$run_time)), file.path(wd, "CONTROL"))
    file.copy(file.path(BDY, "ASCDATA.CFG"), wd, overwrite = TRUE)
    return(paste("PENDING", r$id, "(CONTROL written; waiting for external particles)"))
  }
  # Land-use / roughness grids: the macOS HYSPLIT 5.3 build fails reading LANDUSE.ASC
  # ("non-existent record", unit 77), so by default they are NOT supplied and HYSPLIT
  # uses its documented constants (category 2, z0 = 0.2 m). HRRR supplies the surface
  # fluxes, u* and mixing depth, so transport is barely affected (tested with the Linux
  # build on 3 Jul leg 12: same particle track, footprint sum +15%). METHANE_STILT_LANDUSE=1
  # restores the grids for a build that reads them.
  asc <- if (Sys.getenv("METHANE_STILT_LANDUSE", "0") == "1") c("LANDUSE.ASC", "ROUGLEN.ASC", "TERRAIN.ASC") else character(0)
  unlink(file.path(wd, setdiff(c("LANDUSE.ASC", "ROUGLEN.ASC", "TERRAIN.ASC"), asc)))
  for (f in c("ASCDATA.CFG", asc)) file.copy(file.path(BDY, f), wd, overwrite = TRUE)
  a <- readLines(file.path(wd, "ASCDATA.CFG")); a[length(a)] <- "'./'           directory location of data files"
  writeLines(a, file.path(wd, "ASCDATA.CFG"))
  if (!file.exists(file.path(wd, "hycs_std"))) file.symlink(file.path(EXEC, "hycs_std"), file.path(wd, "hycs_std"))
  write_setup(file.path(wd, "SETUP.CFG")); write_control(r, file.path(SUB_DIR, files_for(r$run_time)), file.path(wd, "CONTROL"))
  system(paste("cd", shQuote(wd), "&& ./hycs_std > stilt.log 2>&1"), timeout = 3600)
  pf <- file.path(wd, "PARTICLE_STILT.DAT")
  if (!file.exists(pf)) return(paste("FAILED", r$id, paste(tail(readLines(file.path(wd, "stilt.log")), 3), collapse = " | ")))
  p <- read_particles(pf); if (is.null(p) || !nrow(p)) return(paste("EMPTY", r$id))
  p <- plume_dilution(p, r$zagl)
  saveRDS(p[, c("time", "indx", "long", "lati", "zagl", "foot", "mlht")], rds)
  unlink(file.path(wd, c("PARTICLE_STILT.DAT", "PARTICLE.DAT", "cdump", "LANDUSE.ASC", "ROUGLEN.ASC", "TERRAIN.ASC")))
  rds
}
cat(sprintf("running %d STILT receptors (%d particles, %g h) on %d cores ...\n", nrow(R), NUMPAR, N_HOURS, N_CORES))
t0 <- Sys.time()
res <- unlist(mclapply(seq_len(nrow(R)), function(i) tryCatch(run_one(i), error = function(e) paste("ERROR", R$id[i], conditionMessage(e))),
                       mc.cores = N_CORES, mc.preschedule = FALSE))
bad <- res[!grepl("particles\\.rds$", res)]
cat(sprintf("done in %.1f min; %d ok, %d failed\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), sum(!res %in% bad), length(bad)))
if (length(bad)) { cat(head(bad, 10), sep = "\n"); if (length(bad) > nrow(R) / 2) stop("most STILT runs failed; see the stilt.log files in ", RUN_DIR) }

## ---- 4. GHGI fluxes by class (umol m-2 s-1) on the 0.1-degree grid ------------------
nc <- nc_open(GHGI_FILE)
glat <- ncvar_get(nc, "lat"); glon <- ncvar_get(nc, "lon")
ilat <- which(glat >= DOMAIN["ymn"] & glat <= DOMAIN["ymx"]); ilon <- which(glon >= DOMAIN["xmn"] & glon <= DOMAIN["xmx"])
rd <- function(v) { a <- ncvar_get(nc, v); if (length(dim(a)) == 3) a <- a[, , 1]; a[ilon, ilat] }
to_umol <- function(molec_cm2_s) molec_cm2_s * 1e4 / 6.02214076e23 * 1e6
CLASSES <- list(
  og_basin = c("emi_ch4_1B2a_Petroleum_Systems_Exploration", "emi_ch4_1B2a_Petroleum_Systems_Production",
               "emi_ch4_1B2b_Natural_Gas_Exploration", "emi_ch4_1B2b_Natural_Gas_Production",
               "emi_ch4_1B2b_Natural_Gas_Processing", "emi_ch4_1B2b_Natural_Gas_TransmissionStorage"),
  ng_dist  = c("emi_ch4_1B2b_Natural_Gas_Distribution", "emi_ch4_Supp_1B2b_PostMeter"),
  waste    = c("emi_ch4_5A1_Landfills_MSW", "emi_ch4_5A1_Landfills_Industrial", "emi_ch4_5B1_Composting",
               "emi_ch4_5D_Wastewater_Treatment_Domestic", "emi_ch4_5D_Wastewater_Treatment_Industrial"),
  livestock = c("emi_ch4_3A_Enteric_Fermentation", "emi_ch4_3B_Manure_Management"))
allv <- grep("^emi_ch4_", names(nc$var), value = TRUE)
Fx <- lapply(CLASSES, function(vs) to_umol(Reduce(`+`, lapply(vs, rd))))
Fx$other <- to_umol(Reduce(`+`, lapply(setdiff(allv, unlist(CLASSES)), rd)))
nc_close(nc)
for (k in names(Fx)) Fx[[k]][!is.finite(Fx[[k]])] <- 0
gx <- glon[ilon]; gy <- glat[ilat]; dx <- median(diff(gx)); dy <- median(diff(gy))
basin_rows <- gy >= BASIN_LAT                      # DJB core: basin-type O&G north of 40.05 N
cell_of <- function(lon, lat) {
  ix <- round((lon - gx[1]) / dx) + 1; iy <- round((lat - gy[1]) / dy) + 1
  ok <- ix >= 1 & ix <= length(gx) & iy >= 1 & iy <= length(gy)
  list(ix = ix, iy = iy, ok = ok)
}

## ---- 4b. GRA2PES v2.0beta methane by class (umol m-2 s-1) on a regular 0.02-degree grid ----
# <OUT_DIR>/gra2pes_v2.0beta_ch4_region_weekdy.csv: 4-km cells over DOMAIN, July 2023 weekday 24-h
# mean, mol km-2 h-1, levels summed (written on the Mac from the extracted v2.0beta trees by
# scripts/95_gra2pes_v2_region.py). Each 0.02-degree node takes the flux density of the nearest cell.
V2F <- file.path(OUT_DIR, "gra2pes_v2.0beta_ch4_region_weekdy.csv")
HAVE_V2 <- file.exists(V2F)
if (HAVE_V2) {
  V <- read.csv(V2F)
  V$other <- pmax(0, V$total - V$OG - V$RES - V$WASTE - V$AG)
  gx2 <- seq(DOMAIN["xmn"] + 0.01, DOMAIN["xmx"] - 0.01, by = 0.02); gy2 <- seq(DOMAIN["ymn"] + 0.01, DOMAIN["ymx"] - 0.01, by = 0.02)
  near <- matrix(NA_integer_, length(gx2), length(gy2)); kx <- cos(40 * pi / 180)
  for (j in seq_along(gy2)) {                      # nearest cell, searching a +-0.06 degree latitude band
    cand <- which(abs(V$lat - gy2[j]) < 0.06); if (!length(cand)) next
    d2 <- outer(gx2, V$lon[cand], function(a, b) ((a - b) * kx)^2) + matrix((V$lat[cand] - gy2[j])^2, length(gx2), length(cand), byrow = TRUE)
    near[, j] <- cand[max.col(-d2, ties.method = "first")]
  }
  v2c <- c(og = "OG", postmeter = "RES", waste = "WASTE", ag = "AG", other = "other")
  Fv2 <- lapply(v2c, function(k) { m <- matrix(V[[k]][near], length(gx2), length(gy2)); m[is.na(m)] <- 0; m / 3600 })   # mol km-2 h-1 -> umol m-2 s-1
  dx2 <- 0.02
  cell2 <- function(lon, lat) { ix <- round((lon - gx2[1]) / dx2) + 1; iy <- round((lat - gy2[1]) / dx2) + 1
    list(ix = ix, iy = iy, ok = ix >= 1 & ix <= length(gx2) & iy >= 1 & iy <= length(gy2)) }
  box_t_hr <- sum(V$total[V$lat >= 39.5 & V$lat <= 39.95 & V$lon >= -105.2 & V$lon <= -104.55]) * 16 * 16.04 / 1e6
  cat(sprintf("GRA2PES v2.0beta region grid: %d cells; box total %.2f t/h (expect 16.5)\n", nrow(V), box_t_hr))
}

## ---- 5. convolution per receptor -----------------------------------------------------
foot_grid <- matrix(0, length(gx), length(gy))
rrows <- list()
for (i in seq_len(nrow(R))) {
  rds <- file.path(RUN_DIR, R$id[i], "particles.rds"); if (!file.exists(rds)) next
  p <- readRDS(rds); np <- max(p$indx)
  c0 <- cell_of(p$long, p$lati); q <- p[c0$ok & p$foot > 0, ]; ix <- c0$ix[c0$ok & p$foot > 0]; iy <- c0$iy[c0$ok & p$foot > 0]
  m <- cbind(ix, iy)
  dC <- vapply(Fx, function(Fk) sum(q$foot * Fk[m]) / np * 1000, 0)            # ppb
  dC_djb_core <- sum(q$foot * Fx$og_basin[m] * basin_rows[iy]) / np * 1000
  foot_grid <- foot_grid + as.matrix(xtabs(q$foot / np ~ factor(ix, levels = seq_along(gx)) + factor(iy, levels = seq_along(gy))))
  # HRRR transport direction: bearing from receptor to the particle-cloud centre 30 min back
  p30 <- p[p$time == -30, ]
  up_dir <- if (nrow(p30)) bearing(R$lati[i], R$long[i], mean(p30$lati), mean(p30$long)) else NA
  up_km  <- if (nrow(p30)) hav_km(R$lati[i], R$long[i], mean(p30$lati), mean(p30$long)) else NA
  dV <- if (HAVE_V2) { c2 <- cell2(p$long, p$lati); k2 <- c2$ok & p$foot > 0; m2 <- cbind(c2$ix[k2], c2$iy[k2])
    setNames(vapply(Fv2, function(Fk) sum(p$foot[k2] * Fk[m2]) / np * 1000, 0), paste0("v2_", names(Fv2))) } else NULL
  rrows[[length(rrows) + 1]] <- data.frame(R[i, ], as.list(round(dC, 3)), as.list(round(dV, 3)), dC_djb_core = round(dC_djb_core, 3),
    foot_sum = sum(q$foot) / np, foot_age_h = if (sum(q$foot) > 0) round(-sum(q$time * q$foot) / sum(q$foot) / 60, 2) else NA,
    hrrr_wd_deg = round(up_dir), hrrr_ws_ms = round(up_km * 1000 / 1800, 1), stringsAsFactors = FALSE)
}
RR <- do.call(rbind, rrows)
if (is.null(RR) || nrow(RR) < 5) stop("fewer than 5 receptors produced footprints")
RR$total <- with(RR, og_basin + ng_dist + waste + livestock + other)
if (HAVE_V2) RR$v2_total <- with(RR, v2_og + v2_postmeter + v2_waste + v2_ag + v2_other)
write.csv(RR, file.path(OUT_DIR, "stilt_exposure_receptors.csv"), row.names = FALSE)

## ---- 6. per-leg table and tests -------------------------------------------------------
num <- c("og_basin", "ng_dist", "waste", "livestock", "other", "total", "dC_djb_core", "foot_sum", "foot_age_h",
         if (HAVE_V2) c("v2_og", "v2_postmeter", "v2_waste", "v2_ag", "v2_other", "v2_total"))
LA <- aggregate(RR[, num], by = list(flight = RR$flight, leg_id = RR$leg_id), FUN = mean, na.rm = TRUE)
wdm <- do.call(rbind, lapply(split(RR, paste(RR$flight, RR$leg_id)), function(x)
  data.frame(flight = x$flight[1], leg_id = x$leg_id[1], hrrr_wd_deg = round(vmean_wd(x$hrrr_wd_deg, x$hrrr_ws_ms)[1]),
             hrrr_ws_ms = round(mean(x$hrrr_ws_ms, na.rm = TRUE), 1), n_rec = nrow(x))))
Lx <- merge(merge(L, LA, by = c("flight", "leg_id")), wdm, by = c("flight", "leg_id"))
fr <- function(num, den) ifelse(den > 0, num / den, NA)
Lx$f_og_stilt     <- round(fr(Lx$og_basin, Lx$total), 3)
Lx$f_djbcore_stilt <- round(fr(Lx$dC_djb_core, Lx$total), 3)
Lx$f_og_stilt_x2  <- round(fr(OG_SCALE_SENS * Lx$og_basin, Lx$total + (OG_SCALE_SENS - 1) * Lx$og_basin), 3)
Lx$f_fossil_stilt <- round(fr(Lx$og_basin + Lx$ng_dist, Lx$total), 3)
Lx$slope_pred     <- round(fr(B_DJB * Lx$og_basin + B_DIST * Lx$ng_dist, Lx$total), 4)
Lx$obs_over_pred  <- round(fr(Lx$dch4_mean_ppb, Lx$total), 2)
Lx$wd_diff_deg    <- round(ang_diff(Lx$wd_deg, Lx$hrrr_wd_deg))
Lx$high_leg <- Lx$agl_m > 1000
if (HAVE_V2) {
  Lx$slope_pred_v2    <- round(fr(B_DJB * Lx$v2_og + B_DIST * Lx$v2_postmeter, Lx$v2_total), 4)
  Lx$f_fossil_v2      <- round(fr(Lx$v2_og + Lx$v2_postmeter, Lx$v2_total), 3)
  Lx$f_waste_v2       <- round(fr(Lx$v2_waste, Lx$v2_total), 3)
  Lx$obs_over_pred_v2 <- round(fr(Lx$dch4_mean_ppb, Lx$v2_total), 2)
}
cone_f <- file.path(OUT_DIR, "wind_source_exposure_legs.csv")
if (file.exists(cone_f)) {
  cone <- read.csv(cone_f)[, c("flight", "leg_id", "f_og_basin", "theta_djb")]
  names(cone)[3] <- "f_og_cone"; Lx <- merge(Lx, cone, by = c("flight", "leg_id"), all.x = TRUE)
}
for (k in intersect(c("og_basin", "ng_dist", "waste", "livestock", "other", "total", "dC_djb_core", "v2_og", "v2_postmeter", "v2_waste", "v2_ag", "v2_other", "v2_total"), names(Lx))) Lx[[k]] <- round(Lx[[k]], 2)
Lx <- Lx[order(Lx$flight, Lx$leg_id), ]
write.csv(Lx, file.path(OUT_DIR, "stilt_exposure_legs.csv"), row.names = FALSE)

sp <- function(x, y, lab, sub = rep(TRUE, length(x))) {
  ok <- sub & is.finite(x) & is.finite(y)
  if (sum(ok) < 5) return(data.frame(test = lab, rho = NA, p = NA, n = sum(ok)))
  t <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  data.frame(test = lab, rho = round(unname(t$estimate), 2), p = signif(t$p.value, 2), n = sum(ok))
}
low <- !Lx$high_leg
summ <- rbind(
  sp(Lx$f_og_stilt, Lx$slope, "slope vs f_og_stilt (basin O&G share of predicted dCH4)"),
  sp(Lx$f_og_stilt, Lx$slope, "  same, legs < 1 km AGL", low),
  sp(Lx$og_basin, Lx$slope, "slope vs predicted basin O&G dCH4 (ppb)"),
  sp(Lx$f_djbcore_stilt, Lx$slope, "slope vs DJB-core share (basin O&G north of 40.05 N)"),
  sp(Lx$slope_pred, Lx$slope, "slope vs inventory-predicted slope"),
  sp(Lx$total, Lx$dch4_mean_ppb, "observed vs predicted dCH4 (transport check)"),
  sp(Lx$total, Lx$dch4_mean_ppb, "  same, legs < 1 km AGL", low),
  sp(Lx$f_og_cone, Lx$f_og_stilt, "script-53 cone f_og vs STILT f_og (method agreement)"),
  if (HAVE_V2) rbind(sp(Lx$slope_pred_v2, Lx$slope, "slope vs GRA2PES v2.0beta-predicted slope"),
                     sp(Lx$v2_total, Lx$dch4_mean_ppb, "observed vs v2.0beta-predicted dCH4"),
                     sp(Lx$v2_total, Lx$dch4_mean_ppb, "  same, legs < 1 km AGL", low)))
extra <- data.frame(
  quantity = c("median f_og_stilt (all legs)", "median f_og_stilt (legs < 1 km)", "max f_og_stilt",
               "median f_og_stilt with basin O&G x2", "median f_djbcore_stilt",
               "median predicted fossil share (og+dist)/total", "median predicted slope", "median observed slope",
               "median observed/predicted dCH4", "median |aircraft - HRRR| wind direction (deg)",
               "legs with foot_sum = 0 (receptor above the mixed layer)",
               if (HAVE_V2) c("v2: median predicted fossil share (og+postmeter)/total", "v2: median predicted waste share",
                              "v2: median predicted slope", "v2: median observed/predicted dCH4", "v2: median predicted/EPA-predicted dCH4")),
  value = c(median(Lx$f_og_stilt, na.rm = TRUE), median(Lx$f_og_stilt[low], na.rm = TRUE), max(Lx$f_og_stilt, na.rm = TRUE),
            median(Lx$f_og_stilt_x2, na.rm = TRUE), median(Lx$f_djbcore_stilt, na.rm = TRUE),
            median(Lx$f_fossil_stilt, na.rm = TRUE), median(Lx$slope_pred, na.rm = TRUE), median(Lx$slope),
            median(Lx$obs_over_pred, na.rm = TRUE), median(Lx$wd_diff_deg, na.rm = TRUE), sum(Lx$foot_sum == 0),
            if (HAVE_V2) c(median(Lx$f_fossil_v2, na.rm = TRUE), median(Lx$f_waste_v2, na.rm = TRUE), median(Lx$slope_pred_v2, na.rm = TRUE),
                           median(Lx$obs_over_pred_v2, na.rm = TRUE), median(fr(Lx$v2_total, Lx$total), na.rm = TRUE))))
extra$value <- signif(extra$value, 3)
write.csv(summ, file.path(OUT_DIR, "stilt_exposure_summary.csv"), row.names = FALSE)
write.csv(extra, file.path(OUT_DIR, "stilt_exposure_quantities.csv"), row.names = FALSE)

cat(sprintf("\nper-leg table (%d legs, %d receptors, %g h, %d particles):\n", nrow(Lx), nrow(RR), N_HOURS, NUMPAR))
print(Lx[, c("flight", "leg_id", "agl_m", "wd_deg", "hrrr_wd_deg", "slope", "slope_pred", "dch4_mean_ppb", "total",
             "og_basin", "ng_dist", "waste", "f_og_stilt", intersect("f_og_cone", names(Lx)))], row.names = FALSE)
cat("\nSpearman tests:\n"); print(summ, row.names = FALSE)
cat("\nquantities:\n"); print(extra, row.names = FALSE)

## ---- 7. figure ------------------------------------------------------------------------
png(file.path(FIG, "FigS_stilt_exposure.png"), 2100, 1400, res = 150)
par(mfrow = c(2, 2), mar = c(4.4, 4.6, 2.2, 1))
pc <- ifelse(Lx$high_leg, 24, 21); cx <- 0.7 + 1.5 * sqrt(Lx$n_gated / max(Lx$n_gated))
YL <- c(-0.05, 0.25); clamp <- function(y) pmin(pmax(y, YL[1]), YL[2]); off <- Lx$slope < YL[1] | Lx$slope > YL[2]
plot(Lx$f_og_stilt, clamp(Lx$slope), pch = pc, cex = cx, bg = ifelse(off, "white", "#4575b4"), las = 1, ylim = YL,
     xlab = "STILT: basin oil-and-gas share of predicted enhancement", ylab = "observed transect ethane:methane slope")
abline(h = 0, col = "gray70")
if (any(off)) mtext(sprintf("open symbols: %d weakly correlated transect(s) off scale, drawn at the axis limit", sum(off)), side = 3, line = 0.1, cex = 0.6)
title(sprintf("(a) rho = %.2f, p = %.2g, n = %d", summ$rho[1], summ$p[1], summ$n[1]), cex.main = 0.95)
ok <- Lx$total > 0
xl <- range(c(Lx$total[ok], if (HAVE_V2) Lx$v2_total[ok & Lx$v2_total > 0], Lx$dch4_mean_ppb[ok]), na.rm = TRUE)
plot(Lx$total[ok], Lx$dch4_mean_ppb[ok], log = "xy", pch = pc[ok], cex = cx[ok], bg = "#d73027", las = 1, xlim = xl, ylim = xl,
     xlab = "STILT x inventory predicted enhancement (ppb)", ylab = "observed mean enhancement, gated (ppb)")
if (HAVE_V2) { o2 <- ok & Lx$v2_total > 0; points(Lx$v2_total[o2], Lx$dch4_mean_ppb[o2], pch = pc[o2], cex = cx[o2], bg = "#fdae61")
  legend("topleft", bty = "n", pt.bg = c("#d73027", "#fdae61"), pch = 21, legend = c("EPA GHGI", "GRA2PES v2.0beta"), cex = 0.85) }
abline(0, 1, lty = 2, col = "gray50")
title(sprintf("(b) rho = %.2f, p = %.2g; median obs/pred %.1f", summ$rho[6], summ$p[6], median(Lx$obs_over_pred, na.rm = TRUE)), cex.main = 0.95)
fg <- foot_grid / max(1, nrow(RR)); fg[fg <= 0] <- NA
image(gx, gy, log10(fg), col = hcl.colors(40, "YlGnBu", rev = TRUE), xlab = "longitude", ylab = "latitude", las = 1,
      xlim = c(-106.5, -103.3), ylim = c(38.8, 41.3), asp = 1 / cos(40 * pi / 180))
contour(gx, gy, Fx$og_basin, levels = quantile(Fx$og_basin[Fx$og_basin > 0], 0.9), add = TRUE, drawlabels = FALSE, col = "#b2182b", lwd = 1.2)
rect(-105.20, 39.50, -104.55, 39.95, border = "black", lwd = 1.2)
points(R$long, R$lati, pch = 3, cex = 0.5)
title("(c) mean footprint (log10); red = top-decile basin O&G cells", cex.main = 0.95)
if (HAVE_V2) {
  plot(Lx$slope_pred, clamp(Lx$slope), pch = pc, cex = cx, bg = "#d73027", las = 1, xlim = c(0, 0.12), ylim = YL,
       xlab = "inventory-predicted ethane:methane slope", ylab = "observed transect ethane:methane slope")
  points(Lx$slope_pred_v2, clamp(Lx$slope), pch = pc, cex = cx, bg = "#fdae61"); abline(0, 1, lty = 2, col = "gray50")
  abline(h = median(Lx$slope), lty = 3, col = "gray40")
  legend("topleft", bty = "n", pt.bg = c("#d73027", "#fdae61"), pch = 21, legend = c("EPA GHGI", "GRA2PES v2.0beta"), cex = 0.85)
  title(sprintf("(d) median predicted %.3f (EPA), %.3f (v2.0beta); observed %.3f", median(Lx$slope_pred, na.rm = TRUE),
                median(Lx$slope_pred_v2, na.rm = TRUE), median(Lx$slope, na.rm = TRUE)), cex.main = 0.95)
}
dev.off()
cat("\nwrote stilt_exposure_receptors.csv, stilt_exposure_legs.csv, stilt_exposure_summary.csv,",
    "stilt_exposure_quantities.csv, figures/FigS_stilt_exposure.png\n")
