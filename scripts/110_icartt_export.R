# 110_icartt_export.R ---------------------------------------------------------
# What the archived aircraft data look like, for the Shiny explorer's "Data" tab.
#
# The NOAA Twin Otter in situ files are public ICARTT (.ict) text files on the NOAA
# CSL AMMBEC archive. Their header says "STIPULATIONS_ON_USE: Use of these data
# requires prior ok from PI", so by default this script does NOT redistribute the
# full 1 Hz records; it exports what a reader needs to understand the files:
#   icartt_index.csv       one row per flight: file name, rows, UTC span, revision,
#                          calibration note, data-interval note
#   icartt_variables.csv   every variable with units, description and the header's
#                          stated uncertainty
#   icartt_headers/<flight>.txt   the header block verbatim
#   icartt_excerpt/<flight>.csv   the first EXCERPT_S records at 1 Hz inside the
#                          Denver-metro box (or from take-off if the flight never
#                          entered it), as a sample of the record format
#   icartt_10s.csv.gz      every flight at 10 s: time, position, heights and the
#                          main species, for the time-series plot and download
# METHANE_ICARTT_FULL=1 additionally writes icartt_full/<flight>.csv.gz, the whole
# file as CSV (about 25 MB for the campaign), for the tab's full-file download:
# use it only with the PI's agreement.
#
# Run: Rscript scripts/110_icartt_export.R        (then script 86 is not needed; the
#      files are written straight into shiny/data/)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths")) source(file.path(proj, "R", paste0(f, ".R")))
EXCERPT_S <- 300; FULL <- Sys.getenv("METHANE_ICARTT_FULL", "0") == "1"
DEST <- file.path(proj, "shiny", "data")
for (d in c("icartt_headers", "icartt_excerpt", if (FULL) "icartt_full")) dir.create(file.path(DEST, d), recursive = TRUE, showWarnings = FALSE)
b <- URBAN_BOX
idx <- list(); vars <- list(); ten <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  ln <- .read_lines_latin1(p); nh <- as.integer(strsplit(ln[1], ",")[[1]][1])
  hdr <- ln[seq_len(nh)]; writeLines(hdr, file.path(DEST, "icartt_headers", paste0(fl, ".txt")))
  nv <- as.integer(hdr[10]); vdef <- hdr[12 + seq_len(nv)]
  unc <- sub("^UNCERTAINTY:\\s*", "", grep("^UNCERTAINTY:", hdr, value = TRUE)[1]); unc <- trimws(strsplit(unc, ",")[[1]])
  vv <- do.call(rbind, lapply(seq_along(vdef), function(i) { s <- trimws(strsplit(vdef[i], ",")[[1]])
    data.frame(flight = fl, variable = s[1], units = s[2], description = paste(s[-(1:2)], collapse = ", "),
               uncertainty = if (i + 1 <= length(unc)) unc[i + 1] else "", stringsAsFactors = FALSE) }))
  vars[[fl]] <- rbind(data.frame(flight = fl, variable = trimws(strsplit(hdr[9], ",")[[1]][1]), units = "seconds",
                                 description = "Start time in UTC seconds since midnight", uncertainty = if (length(unc)) unc[1] else "", stringsAsFactors = FALSE), vv)
  ic <- read_icartt(p); d <- ic$data
  g <- function(key) sub(paste0("^", key, ":\\s*"), "", grep(paste0("^", key, ":"), hdr, value = TRUE)[1])
  idx[[fl]] <- data.frame(flight = fl, file = basename(p), header_lines = nh, n_records = nrow(d), n_variables = nv + 1,
    start_utc = format(min(d$timestamp), "%Y-%m-%d %H:%M:%S"), end_utc = format(max(d$timestamp), "%Y-%m-%d %H:%M:%S"),
    revision = sub(".*_(R[0-9A-Z]+)(_L[0-9])?\\.ict$", "\\1", basename(p)), data_info = g("DATA_INFO"), other_comments = g("OTHER_COMMENTS"),
    stipulations = g("STIPULATIONS_ON_USE"), pi = g("PI_CONTACT_INFO"), stringsAsFactors = FALSE)
  inbox <- which(d$Latitude >= b$lat_s & d$Latitude <= b$lat_n & d$Longitude >= b$lon_w & d$Longitude <= b$lon_e)
  i0 <- if (length(inbox)) inbox[1] else 1
  ex <- d[i0:min(nrow(d), i0 + EXCERPT_S - 1), ]; ex$timestamp <- format(ex$timestamp, "%Y-%m-%d %H:%M:%S")
  write.csv(ex, file.path(DEST, "icartt_excerpt", paste0(fl, ".csv")), row.names = FALSE)
  i10 <- seq(1, nrow(d), by = 10); keep <- intersect(c("Latitude", "Longitude", "ALTGPS", "ALTAGL", "CH4_ppb", "C2H6_ppb", "CO_ppb", "CO2_ppm", "H2O_ppm", "WS", "WD", "Temp_True"), names(d))
  ten[[fl]] <- cbind(data.frame(flight = fl, time_utc = format(d$timestamp[i10], "%Y-%m-%d %H:%M:%S"), stringsAsFactors = FALSE), round(d[i10, keep], 3))
  if (FULL) { zz <- gzfile(file.path(DEST, "icartt_full", paste0(fl, ".csv.gz")), "w"); dd <- d; dd$timestamp <- format(dd$timestamp, "%Y-%m-%d %H:%M:%S"); write.csv(dd, zz, row.names = FALSE); close(zz) }
  message(sprintf("%-16s %6d records, %d variables, %s to %s UTC%s", fl, nrow(d), nv + 1, format(min(d$timestamp), "%H:%M"), format(max(d$timestamp), "%H:%M"), if (FULL) " (full CSV written)" else ""))
}
write.csv(do.call(rbind, idx), file.path(DEST, "icartt_index.csv"), row.names = FALSE)
write.csv(do.call(rbind, vars), file.path(DEST, "icartt_variables.csv"), row.names = FALSE)
zz <- gzfile(file.path(DEST, "icartt_10s.csv.gz"), "w"); write.csv(do.call(rbind, ten), zz, row.names = FALSE); close(zz)
message(sprintf("wrote icartt_index.csv (%d flights), icartt_variables.csv, icartt_10s.csv.gz, %d headers and %d excerpts to shiny/data/%s",
                length(idx), length(idx), length(idx), if (FULL) ", plus icartt_full/" else " (full files not exported: set METHANE_ICARTT_FULL=1 with the PI's agreement)"))
