# 43_carbonmapper_plumes.R ----------------------------------------------------
# Pull Carbon Mapper point-source plume detections over the Denver analysis box and
# summarise them by facility cluster and IPCC sector. R port of the former
# 43_carbonmapper_plumes.sh (bash + embedded Python), same outputs and same rules.
#
#   Rscript scripts/43_carbonmapper_plumes.R                     # URBAN_BOX of config.R
#   BOX="-105.4 39.9 -104.2 40.7" Rscript scripts/43_carbonmapper_plumes.R   # the DJB
#
# Out: <OUT_DIR>/carbonmapper_denver_plumes.csv   one row per plume (overpass detection)
#      <OUT_DIR>/carbonmapper_denver_sources.csv  one row per source (persistence-weighted)
#      <OUT_DIR>/carbonmapper_raw_plumes_<date>.json, carbonmapper_raw_sources_<date>.json
#      and a summary on stdout.
#
# NOT part of run_all.R: it needs the internet, and the catalogue changes as new
# overpasses are processed and emission versions are reprocessed, so it is a snapshot
# rather than a reproducible pipeline stage. Every row records the fetch date, and the
# raw API responses are now kept beside the CSVs.
#
# THE PAPER USES THE 12 SEPTEMBER 2026 SNAPSHOT. To keep a later run from silently
# replacing it (scripts 54 and 59 read these files), an existing snapshot from another
# date is left in place and the new fetch is written with a date suffix instead; set
# METHANE_CM_OVERWRITE=1 to replace the snapshot deliberately.
#
# Offline / re-processing: METHANE_CM_PLUMES_JSON and METHANE_CM_SOURCES_JSON point the
# script at saved API responses instead of fetching.
#
# WHAT THIS IS AND IS NOT COMPARABLE TO. Carbon Mapper reports INDIVIDUAL POINT-SOURCE
# plumes seen at the moment of an overpass, above a detection limit of roughly 100 kg/hr
# for EMIT and Tanager (lower for the airborne GAO). The campaign estimate in this paper
# is a whole-city flux including diffuse and area sources. The sum of detected plumes is
# therefore a LOWER BOUND on the city total, and the absence of a detection at a facility
# is not evidence that the facility does not emit. Read it as: which discrete sources are
# big enough to see, how often, and how large they are relative to the city total.
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("package jsonlite is needed: install.packages(\"jsonlite\")")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# west south east north -- the URBAN_BOX of config.R unless BOX is set
bx <- Sys.getenv("BOX", unset = "")
WSEN <- if (nzchar(bx)) as.numeric(strsplit(trimws(bx), "\\s+")[[1]]) else
  c(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n)
stopifnot(length(WSEN) == 4, all(is.finite(WSEN)))
cat(sprintf("box: %s\n", paste(format(WSEN, nsmall = 2), collapse = " ")))
bbox_q <- paste0("bbox=", WSEN, collapse = "&")
API <- "https://api.carbonmapper.org/api/v1/catalog"
fetched <- format(Sys.Date())

# Fetch (or read a saved response) and keep the raw JSON next to the outputs.
get_json <- function(url, env_file, keep_as) {
  f <- Sys.getenv(env_file, unset = "")
  if (nzchar(f)) { txt <- readLines(f, warn = FALSE, encoding = "UTF-8"); src <- f }
  else {
    tmp <- tempfile(fileext = ".json")
    ok <- tryCatch({ utils::download.file(url, tmp, quiet = TRUE, mode = "wb"); TRUE },
                   error = function(e) { message("fetch failed: ", conditionMessage(e)); FALSE })
    if (!ok) return(NULL)
    txt <- readLines(tmp, warn = FALSE, encoding = "UTF-8"); src <- url
    writeLines(txt, file.path(OUT_DIR, keep_as))
  }
  list(json = jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE), src = src)
}

# Snapshot guard: the canonical file is replaced only if it does not exist, already holds
# today's fetch, or METHANE_CM_OVERWRITE=1; otherwise the new table gets a date suffix.
out_path <- function(name) {
  p <- file.path(OUT_DIR, name)
  if (!file.exists(p) || identical(Sys.getenv("METHANE_CM_OVERWRITE"), "1")) return(p)
  old <- tryCatch(utils::read.csv(p, stringsAsFactors = FALSE)$fetched[1], error = function(e) NA)
  if (identical(as.character(old), fetched)) return(p)
  q <- file.path(OUT_DIR, sub("\\.csv$", paste0("_", fetched, ".csv"), name))
  message("keeping the existing ", name, " (fetched ", old, "); writing this fetch to ", basename(q),
          "  (METHANE_CM_OVERWRITE=1 replaces the snapshot)")
  q
}

nz <- function(x) if (is.null(x)) NA else x
pyround <- function(x, d = 0) round(x, d)    # R and Python both round half to even
med <- function(v) stats::median(v)

# ---- plumes ------------------------------------------------------------------------------
pj <- get_json(paste0(API, "/plumes/annotated?", bbox_q, "&limit=500"), "METHANE_CM_PLUMES_JSON",
               paste0("carbonmapper_raw_plumes_", fetched, ".json"))
if (is.null(pj)) stop("could not fetch the Carbon Mapper plume catalogue")
items <- pj$json$items
if (!length(items)) stop("the API returned no plumes for this box")

P <- do.call(rbind, lapply(items, function(p) {
  xy <- unlist(p$geometry_json$coordinates)
  data.frame(plume_id = p$plume_id, gas = p$gas, sector = nz(p$sector),
             date = substr(p$scene_timestamp, 1, 10), platform = p$platform, instrument = p$instrument,
             lat = pyround(xy[2], 4), lon = pyround(xy[1], 4),
             kg_hr = if (is.null(p$emission_auto)) NA_real_ else pyround(p$emission_auto),
             kg_hr_unc = if (is.null(p$emission_uncertainty_auto)) NA_real_ else pyround(p$emission_uncertainty_auto),
             wind_m_s = if (is.null(p$wind_speed_avg_auto)) NA_real_ else p$wind_speed_avg_auto,
             fetched = fetched, stringsAsFactors = FALSE)
}))
P <- P[order(P$date, decreasing = TRUE), ]
# Shortest decimal that round-trips, as the previous Python version wrote floats (R's default
# 15 significant digits would print 5.40554904937744 for 5.405549049377441).
num_txt <- function(v) vapply(v, function(x) {
  if (!is.finite(x)) return(NA_character_)
  for (d in 15:17) { s <- sprintf(paste0("%.", d, "g"), x); if (as.numeric(s) == x) return(s) }
  sprintf("%.17g", x) }, "")
P$wind_m_s <- num_txt(P$wind_m_s)
pp <- out_path("carbonmapper_denver_plumes.csv")
utils::write.csv(P, pp, row.names = FALSE, na = "")
cat(sprintf("wrote %s  (%d plumes, in-box count reported by the API: %s)\n", pp, nrow(P),
            format(nz(pj$json$bbox_count))))

ch4 <- P[P$gas == "CH4", ]
q <- ch4$kg_hr[is.finite(ch4$kg_hr)]
cat(sprintf("\n%d CH4 plumes, %d with a quantified rate, %s to %s\n", nrow(ch4), length(q), min(P$date), max(P$date)))
if (length(q)) cat(sprintf("  rate kg/hr: min %g  median %.0f  max %g\n", min(q), med(q), max(q)))

# IPCC sector codes as Carbon Mapper tags them: 6A solid waste, 4D wastewater,
# 1B2 oil and gas fugitive, 1B1 coal, 4A/3A agriculture, 1A1 energy industries.
sec <- ifelse(is.na(ch4$sector) | ch4$sector == "", "unlabelled", ch4$sector)
cat("\nby sector:\n")
for (s in names(sort(table(sec), decreasing = TRUE))) {
  qq <- ch4$kg_hr[sec == s & is.finite(ch4$kg_hr)]
  cat(sprintf("  %-12s %3d plume(s)%s\n", s, sum(sec == s), if (length(qq)) sprintf(", median %.0f kg/hr", med(qq)) else ""))
}

# Cluster by 0.04 deg (~4 km) so repeat visits to one facility group together
# (a large landfill spreads detections over a couple of km).
key <- paste(round(ch4$lat / 0.04), round(ch4$lon / 0.04))
cat("\nby site (plumes within ~4 km grouped):\n")
for (k in names(sort(table(key), decreasing = TRUE))) {
  v <- ch4[key == k, ]; qq <- v$kg_hr[is.finite(v$kg_hr)]
  secs <- sort(unique(ifelse(is.na(v$sector) | v$sector == "", "?", v$sector)))
  cat(sprintf("  %.3f, %.3f  n=%2d  sectors=%s%s\n", mean(v$lat), mean(v$lon), nrow(v),
              paste0("[", paste0("'", secs, "'", collapse = ", "), "]"),
              if (length(qq)) sprintf("  median %.0f, max %g kg/hr", med(qq), max(qq)) else ""))
  cat(sprintf("      dates: %s\n", paste(sort(unique(v$date)), collapse = ", ")))
}

# What a single overpass sees, which is the only number comparable to a flux estimate.
byday <- tapply(ch4$kg_hr[is.finite(ch4$kg_hr) & ch4$kg_hr > 0], ch4$date[is.finite(ch4$kg_hr) & ch4$kg_hr > 0], sum)
if (length(byday)) {
  cat(sprintf("\nper-overpass detected total (t CH4/hr): min %.2f, median %.2f, max %.2f  over %d dates\n",
              min(byday) / 1000, med(byday) / 1000, max(byday) / 1000, length(byday)))
  cat("Compare with emission_estimates.csv (campaign urban flux) and with the\n")
  cat("waste sector of the inventories in inventory_sector_compare.csv.\n")
}

# ---- source-level table ------------------------------------------------------------------
# A SOURCE is a DBSCAN cluster of plumes at one location; a PLUME is one overpass of it.
# Carbon Mapper's source emission rate is persistence-weighted -- it already carries the
# overpasses that saw nothing, which a plume rate does not. For the Cherokee CO2 source
# the portal shows 26.4 t/hr against the plume's 237, a factor of exactly 9.
#
# ENDPOINT. It is catalog/sources.geojson, NOT "sources/annotated" (which does not exist).
# Parameters come from the API's own schema at /api/v1/openapi.json. `eps` and `minpoints`
# are the DBSCAN clustering radius (m) and minimum cluster size: the clustering is a QUERY
# PARAMETER, so a "source" is a spatial cluster at the radius you ask for, not a facility
# boundary. Left at the server default here; the radius used is recorded in each source id
# ({gas}_{sector}_{eps}m_{lon}_{lat}).
sj <- get_json(paste0(API, "/sources.geojson?", bbox_q), "METHANE_CM_SOURCES_JSON",
               paste0("carbonmapper_raw_sources_", fetched, ".json"))
if (is.null(sj)) { cat("(sources.geojson unavailable; plume table written anyway)\n"); quit(save = "no") }
feats <- if (!is.null(sj$json$features)) sj$json$features else sj$json

# A property value as one CSV cell. Lists are written the way the previous Python version
# wrote them (e.g. "['a', 'b']"), so the archived snapshot and a new fetch stay comparable.
cell <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (is.list(v) || length(v) > 1) {
    v <- unlist(v)
    return(paste0("[", paste(vapply(v, function(x) if (is.character(x)) paste0("'", x, "'") else
      if (is.logical(x)) (if (x) "True" else "False") else as.character(x), ""), collapse = ", "), "]"))
  }
  if (is.logical(v)) return(if (v) "True" else "False")
  if (is.numeric(v)) {                                     # shortest decimal that round-trips
    for (d in 15:17) { s <- sprintf(paste0("%.", d, "g"), v); if (as.numeric(s) == v) return(s) }
  }
  as.character(v)
}

rows <- lapply(feats, function(f) {
  pr <- f$properties; if (is.null(pr)) pr <- list()
  xy <- unlist(f$geometry$coordinates)
  if (is.null(pr$id)) pr$id <- f$id
  pr$geom_lon <- if (length(xy) >= 2 && is.numeric(xy)) round(xy[1], 5) else NULL
  pr$geom_lat <- if (length(xy) >= 2 && is.numeric(xy)) round(xy[2], 5) else NULL
  pr$fetched <- fetched
  pr
})
if (!length(rows)) { cat("\nsources.geojson returned no features for this box.\n"); quit(save = "no") }

# Every property the API returns becomes a column, in order of first appearance, so a field
# not seen before shows up rather than being silently dropped.
cols <- unique(unlist(lapply(rows, names)))
SRC <- as.data.frame(lapply(setNames(cols, cols), function(k) vapply(rows, function(r) cell(r[[k]]), "")),
                     stringsAsFactors = FALSE, check.names = FALSE)
sp <- out_path("carbonmapper_denver_sources.csv")
utils::write.csv(SRC, sp, row.names = FALSE, na = "")
cat(sprintf("\nwrote %s  (%d sources, %d columns)\nsource fields returned by the API:\n  %s\n",
            sp, nrow(SRC), length(cols), paste(cols, collapse = ", ")))

pick <- function(r, ...) { for (n in c(...)) if (!is.null(r[[n]]) && !identical(r[[n]], "")) return(r[[n]]); NULL }
numv <- function(x) suppressWarnings(as.numeric(if (is.null(x)) NA else x))
rate_of <- function(r) { x <- numv(pick(r, "emission_auto", "emission", "rate")); if (is.finite(x)) x else 0 }
cat("\nsources in the box:\n")
for (r in rows[order(-vapply(rows, rate_of, 0))]) {
  rate <- pick(r, "emission_auto", "emission", "rate"); unc <- pick(r, "emission_uncertainty_auto", "emission_uncertainty")
  npl <- pick(r, "plume_count", "n_plumes", "count"); name <- pick(r, "source_name", "name", "facility_name")
  cat(sprintf("  %-34s %-4s %-5s %s kg/hr%s  plumes=%s  %s, %s\n",
              substr(if (is.null(name)) "(unnamed)" else as.character(name), 1, 34),
              if (is.null(pick(r, "gas", "plume_gas"))) "?" else pick(r, "gas", "plume_gas"),
              if (is.null(pick(r, "sector"))) "?" else pick(r, "sector"),
              if (is.null(rate)) "        -" else sprintf("%9.0f", numv(rate)),
              if (is.null(unc)) "" else sprintf(" +/- %.0f", numv(unc)),
              if (is.null(npl)) "None" else npl, cell(r$geom_lat), cell(r$geom_lon)))
  cat(sprintf("      id: %s\n", format(nz(r$id))))
}

# ---- persistence-weighted totals ---------------------------------------------------------
# persistence = detection_date_count / observation_date_count, and emission_auto ALREADY has
# it applied: it is the mean per-overpass detected total scaled by how often the source is
# seen emitting. Verified against the plume table on 12 Sep 2026: for three single/two-plume
# sources the identity is exact (e.g. 848.0 x 1/10 = 84.80, 237493 x 1/9 = 26388.1) and for
# Tower Rd it reproduces to 0.02% using per-date sums. So these are time-averaged rates and
# are the ones to place beside a campaign flux; the per-overpass plume sums above are
# instantaneous and are NOT the same quantity.
q4 <- Filter(function(r) identical(r$gas, "CH4") && is.finite(numv(r$emission_auto)), rows)
if (length(q4)) {
  e <- vapply(q4, function(r) numv(r$emission_auto), 0); s_ <- vapply(q4, function(r) nz(r$sector), "")
  tot <- sum(e); lf <- sum(e[s_ == "6A"]); og <- sum(e[s_ == "1B2"])
  unq <- sum(vapply(rows, function(r) identical(r$gas, "CH4") && !is.finite(numv(r$emission_auto)), TRUE))
  cat(sprintf("\npersistence-weighted CH4 point sources: %.1f kg/hr = %.3f t/hr  (%d quantified, %d detected but unquantified)\n",
              tot, tot / 1000, length(q4), unq))
  if (tot > 0) cat(sprintf("  landfill (6A) %.1f kg/hr (%.0f%%)   oil and gas (1B2) %.1f kg/hr (%.0f%%)\n",
                           lf, 100 * lf / tot, og, 100 * og / tot))
  cat("  persistence by source (detections / clear observations):\n")
  pers <- vapply(q4, function(r) { x <- numv(r$persistence); if (is.finite(x)) x else 0 }, 0)
  for (i in order(-pers)) {
    r <- q4[[i]]
    cat(sprintf("    %s, %s  %-4s  %.2f  (%s/%s)  %.0f kg/hr\n", cell(r$geom_lat), cell(r$geom_lon), nz(r$sector),
                pers[i], format(nz(r$detection_date_count)), format(nz(r$observation_date_count)), e[i]))
  }
  cat("  A persistence near 1 is a chronic emitter; near 0.1 is an intermittent one.\n")
  cat("  Compare tot/1000 with the campaign flux in emission_estimates.csv, and the\n")
  cat("  6A subtotal with the waste sector in inventory_sector_compare.csv.\n")
}
