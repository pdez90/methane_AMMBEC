# 57_gra2pes_cells.R ----------------------------------------------------------
# RUN ON YOUR OWN MACHINE (needs the extracted GRA2PES trees; see fetch_gra2pes_sectors.sh).
#
# WHY. The manuscript compares the aircraft-sampled fossil-vs-biogenic character of
# Denver's methane (Fig 4B, script 25; 0.02 deg cells) with the EPA gridded GHGI at
# 0.1 deg. That inventory is coarse (each 0.1 deg cell is ~8 x 11 km, larger than
# the whole Suncor/Metro complex), so the spatial match is weak by construction.
# GRA2PES is 4 km and, in v2.0beta, carries waste (landfill + WWTP) and post-meter
# sectors that v1.1 lacked. This script writes the per-CELL, per-sector methane (and
# total CO) of both GRA2PES versions over the analysis box, so that script 58 can
#   (a) sample each inventory at the aircraft-sampled cells and score the match, and
#   (b) draw a source-context map at 4 km.
#
# Reads, for each version:
#   <sectors>/<VER>/<SECTOR>/<MONTH>/<DAYTYPE>/*.nc   sector methane (HC01), whatever
#                                                    sectors are extracted; WASTE, OG,
#                                                    RES and AG are the ones that matter
#   <total tree>/<DAYTYPE>/*.nc                      total HC01 (methane) and CO
# Cell handling is the validated core of scripts 18/36/39/40 (molar units, vertical
# levels summed, 24 h averaged, 16 km2 cell guard). Only the day type given is used
# (weekdy by default, as in scripts 39/40); methane is nearly day-type insensitive.
#
# Run (Methane_AMMBEC layout):
#   G=../inventories/GRA2PES
#   Rscript scripts/57_gra2pes_cells.R "$G/sectors/v1.1"     "$G/ch4only/202307"    202307 weekdy   (CO found in the sibling allspec/202307)
#   Rscript scripts/57_gra2pes_cells.R "$G/sectors/v2.0beta" "$G/v2total/202307"    202307 weekdy
# Out: <OUT_DIR>/gra2pes_cells_<version>.csv     one row per box cell:
#        i, j, lon, lat, ch4_total_t_hr, co_t_hr, <SECTOR>_t_hr ..., classified sums
#      <OUT_DIR>/gra2pes_cells_<version>_summary.csv   box sums by class (a cross-check
#        against gra2pes_sector_v1_vs_v2.csv and the config anchors)
# ----------------------------------------------------------------------------
suppressMessages(library(ncdf4))
proj <- if (file.exists("config.R")) "." else ".."
if (file.exists(file.path(proj, "config.R"))) source(file.path(proj, "config.R"))
OUT <- if (exists("OUT_DIR")) OUT_DIR else path.expand("~/MethaneData_outputs")
if (!nzchar(Sys.getenv("METHANE_OUT_DIR"))) {
  sib <- normalizePath(file.path(proj, "..", "outputs"), mustWork = FALSE)
  if (dir.exists(sib)) OUT <- sib
}
BOX <- if (exists("URBAN_BOX")) URBAN_BOX else
  list(lat_s = 39.50, lat_n = 39.95, lon_w = -105.20, lon_e = -104.55)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: Rscript 57_gra2pes_cells.R <sector parent> <total month dir> [MONTH] [daytype]")
SECP <- args[1]; TOTD <- args[2]
MONTH   <- if (length(args) >= 3) args[3] else "202307"
DAYTYPE <- if (length(args) >= 4) args[4] else "weekdy"

MW <- c(CH4 = 16.04, CO = 28.01); NOMINAL_KM <- 4
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# Sector classes, matching scripts/41 (EPA GHGI classes: waste, og, postmeter, ag, other).
CLASS <- c(WASTE = "waste", OG = "og", RES = "postmeter", AG = "ag")

.gra_version <- function(d) {
  f <- list.files(d, pattern = "\\.nc$", recursive = TRUE)
  if (!length(f)) return("unknown")
  m <- regmatches(f[1], regexpr("GRA2PES(v[0-9][0-9.]*[A-Za-z]*)", f[1]))
  if (!length(m)) "unknown" else sub("^GRA2PES", "", m)
}
.cell_km2 <- function(nc, lat, lon) {
  ax <- ncatt_get(nc, 0, "DX"); ay <- ncatt_get(nc, 0, "DY")
  if (isTRUE(ax$hasatt) && isTRUE(ay$hasatt) && is.finite(ax$value) && is.finite(ay$value) &&
      ax$value > 0 && ay$value > 0) return(ax$value * ay$value / 1e6)
  hav <- function(lo1, la1, lo2, la2) { R <- 6371.0088; p <- pi / 180
    a <- sin((la2 - la1) * p / 2)^2 + cos(la1 * p) * cos(la2 * p) * sin((lo2 - lo1) * p / 2)^2
    2 * R * asin(pmin(1, sqrt(a))) }
  i <- max(1, floor(nrow(lat) / 2)); j <- max(1, floor(ncol(lat) / 2))
  hav(lon[i, j], lat[i, j], lon[i + 1, j], lat[i + 1, j]) * hav(lon[i, j], lat[i, j], lon[i, j + 1], lat[i, j + 1])
}
.set_cell <- function(nc, lat, lon) {
  km2 <- .cell_km2(nc, lat, lon); edge <- sqrt(km2)
  if (!is.finite(km2) || km2 <= 0 || abs(edge - NOMINAL_KM) > 0.2)
    stop("cell edge ", signif(edge, 4), " km, not ~", NOMINAL_KM, " km; refusing.")
  NOMINAL_KM^2
}
find_var <- function(nc, spec) {               # HC01 for methane; CO by name or long_name
  if (spec == "CH4") { if ("HC01" %in% names(nc$var)) return("HC01"); pat <- "methane" }
  else { if (spec %in% names(nc$var)) return(spec); pat <- "carbon monoxide" }
  for (vn in names(nc$var)) {
    u <- tolower(nc$var[[vn]]$units %||% ""); ln <- tolower(nc$var[[vn]]$longname %||% "")
    if (grepl("mole", u) && grepl(pat, ln) && !grepl("\\+|alkanes|lumped", ln)) return(vn)
  }
  stop("no variable for ", spec, " in ", basename(nc$filename))
}

# Per-cell 24 h mean emission (t/hr per cell) for one species in one month/daytype tree.
# Returns list(cells = data.frame(i, j, lon, lat, t_hr), n_hours).
cell_field <- function(mdir, spec) {
  files <- list.files(file.path(mdir, DAYTYPE), pattern = "\\.nc$", full.names = TRUE)
  if (!length(files)) return(NULL)
  ref <- max(file.info(files)$size); files <- files[file.info(files)$size >= 0.5 * ref]
  acc <- NULL; nt <- 0L; idx <- NULL; cell <- NA
  for (fp in files) {
    nc <- nc_open(fp)
    vn <- find_var(nc, spec)
    if (is.null(idx)) {
      lon <- ncvar_get(nc, if ("lon" %in% names(nc$var)) "lon" else "XLONG")
      lat <- ncvar_get(nc, if ("lat" %in% names(nc$var)) "lat" else "XLAT")
      if (max(lon, na.rm = TRUE) > 180) lon <- lon - 360
      mask <- lat >= BOX$lat_s & lat <= BOX$lat_n & lon >= BOX$lon_w & lon <= BOX$lon_e
      mask[is.na(mask)] <- FALSE
      stopifnot(sum(mask) > 0); cell <- .set_cell(nc, lat, lon)
      idx <- which(mask, arr.ind = TRUE)
      acc <- numeric(nrow(idx))
      cells <- data.frame(i = idx[, 1], j = idx[, 2], lon = lon[idx], lat = lat[idx])
      u <- nc$var[[vn]]$units %||% ""
      if (!grepl("mole", tolower(u))) stop(vn, " units are '", u, "', expected molar")
    }
    e <- ncvar_get(nc, vn); dd <- dim(e)
    if (length(dd) == 4) { es <- e[, , 1, ]; for (l in 2:dd[3]) es <- es + e[, , l, ]; e <- es }
    nth <- if (length(dim(e)) == 3) dim(e)[3] else 1L
    for (h in seq_len(nth)) {
      layer <- if (nth > 1) e[, , h] else e
      stopifnot(all(dim(layer) == dim(mask)))
      v <- layer[idx]; v[is.na(v)] <- 0
      acc <- acc + v; nt <- nt + 1L
    }
    nc_close(nc)
  }
  cells$t_hr <- (acc / nt) * cell * MW[[spec]] / 1e6      # mole km-2 hr-1 -> t/hr per cell
  list(cells = cells, n_hours = nt)
}

ver <- .gra_version(TOTD); if (ver == "unknown") ver <- .gra_version(SECP)
message("inventory version: ", ver, "   month ", MONTH, "   day type ", DAYTYPE)

## ---- totals (methane HC01 and CO) from the total tree ------------------------
# v1.1 ships methane and the other species in SEPARATE 'total' archives (the all-species
# files carry no HC01), so the methane total and the CO total can live in different trees.
# Try the given tree first; if a species is missing there, look in the sibling trees
# named by convention (ch4only/<MONTH> for methane, allspec/<MONTH> for CO), or in the
# directories given by METHANE_GRA2PES_CH4_DIR / METHANE_GRA2PES_CO_DIR.
.alt <- function(spec) {
  env <- Sys.getenv(if (spec == "CH4") "METHANE_GRA2PES_CH4_DIR" else "METHANE_GRA2PES_CO_DIR")
  sib <- file.path(dirname(normalizePath(TOTD, mustWork = FALSE)), if (spec == "CH4") "ch4only" else "allspec", MONTH)
  sib2 <- file.path(dirname(dirname(normalizePath(TOTD, mustWork = FALSE))), if (spec == "CH4") "ch4only" else "allspec", MONTH)
  c(if (nzchar(env)) env, sib, sib2)
}
.field_any <- function(spec) {
  r <- tryCatch(cell_field(TOTD, spec), error = function(e) NULL)
  if (!is.null(r)) return(r)
  for (d in .alt(spec)) if (dir.exists(file.path(d, DAYTYPE))) {
    r <- tryCatch(cell_field(d, spec), error = function(e) NULL)
    if (!is.null(r)) { message("  ", spec, " read from ", d); return(r) }
  }
  NULL
}
tot_ch4 <- .field_any("CH4")
if (is.null(tot_ch4)) stop("no methane (HC01) total under ", TOTD, " or its ch4only/", MONTH, " sibling")
message(sprintf("  total CH4: %d cells, %d hours, box %.3f t/hr", nrow(tot_ch4$cells), tot_ch4$n_hours, sum(tot_ch4$cells$t_hr)))
tot_co <- .field_any("CO"); if (is.null(tot_co)) message("  CO: not found; co_t_hr left NA")
if (!is.null(tot_co)) message(sprintf("  total CO : box %.3f t/hr = %.1f Gg/yr", sum(tot_co$cells$t_hr), sum(tot_co$cells$t_hr) * 8.76))

G <- tot_ch4$cells; names(G)[names(G) == "t_hr"] <- "ch4_total_t_hr"
G$co_t_hr <- if (!is.null(tot_co)) tot_co$cells$t_hr[match(paste(G$i, G$j), paste(tot_co$cells$i, tot_co$cells$j))] else NA_real_

## ---- sectors -----------------------------------------------------------------
sds <- list.dirs(SECP, recursive = FALSE)
sds <- sds[dir.exists(file.path(sds, MONTH, DAYTYPE))]
if (!length(sds)) stop("no sector subdirs with ", MONTH, "/", DAYTYPE, " under ", SECP)
have <- character(0)
for (d in sds) {
  s <- toupper(basename(d))
  f <- tryCatch(cell_field(file.path(d, MONTH), "CH4"),
                error = function(e) { message("  ", s, ": ", conditionMessage(e)); NULL })
  if (is.null(f)) next
  G[[paste0(s, "_t_hr")]] <- f$cells$t_hr[match(paste(G$i, G$j), paste(f$cells$i, f$cells$j))]
  have <- c(have, s)
  message(sprintf("  %-13s box %7.3f t/hr", s, sum(f$cells$t_hr)))
}
missing_core <- setdiff(names(CLASS), have)
if (length(missing_core))
  message("NOTE: core sector(s) not extracted, treated as absent from this version's tree: ",
          paste(missing_core, collapse = ", "),
          "\n      (run: bash fetch_gra2pes_sectors.sh ", paste(missing_core, collapse = " "), ")")

## ---- classes ----------------------------------------------------------------
z <- function(x) ifelse(is.na(x), 0, x)
for (cl in unique(CLASS)) {
  secs <- names(CLASS)[CLASS == cl & names(CLASS) %in% have]
  G[[paste0(cl, "_t_hr")]] <- if (length(secs)) rowSums(as.data.frame(lapply(secs, function(s) z(G[[paste0(s, "_t_hr")]])))) else 0
}
G$other_t_hr   <- pmax(0, G$ch4_total_t_hr - G$waste_t_hr - G$og_t_hr - G$postmeter_t_hr - G$ag_t_hr)
G$fossil_t_hr  <- G$og_t_hr + G$postmeter_t_hr           # same definition as the EPA comparison in the paper
G$biogenic_t_hr <- G$waste_t_hr + G$ag_t_hr
G$fossil_frac  <- ifelse(G$fossil_t_hr + G$biogenic_t_hr > 0,
                         G$fossil_t_hr / (G$fossil_t_hr + G$biogenic_t_hr), NA_real_)
G$inventory_version <- ver; G$month <- MONTH; G$daytype <- DAYTYPE
num <- sapply(G, is.numeric); G[num] <- lapply(G[num], function(x) signif(x, 6))

fn <- file.path(OUT, paste0("gra2pes_cells_", ver, ".csv"))
write.csv(G, fn, row.names = FALSE)
sm <- data.frame(inventory_version = ver, month = MONTH, daytype = DAYTYPE, n_cells = nrow(G),
                 ch4_total_t_hr = sum(G$ch4_total_t_hr), co_Gg_yr = sum(G$co_t_hr) * 8.76,
                 waste_t_hr = sum(G$waste_t_hr), og_t_hr = sum(G$og_t_hr), postmeter_t_hr = sum(G$postmeter_t_hr),
                 ag_t_hr = sum(G$ag_t_hr), other_t_hr = sum(G$other_t_hr),
                 fossil_frac_box = sum(G$fossil_t_hr) / (sum(G$fossil_t_hr) + sum(G$biogenic_t_hr)),
                 sectors_read = paste(have, collapse = ";"))
write.csv(sm, file.path(OUT, paste0("gra2pes_cells_", ver, "_summary.csv")), row.names = FALSE)
cat("\n"); print(t(sm)); cat("\nwrote ", fn, "\n")
