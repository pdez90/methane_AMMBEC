# 28_loop_closure_figure.R ----------------------------------------------------
# QUALITATIVE loop-geometry figure (Figure S6). For each urban flight it draws the
# flown urban legs in map coordinates against the Denver-metropolitan analysis box.
# The point is visual and qualitative: none of these flights encircles the metro
# box, so no closed-loop (box) mass balance around the city is possible. It shows
# NO numeric closure statistic and computes NO flux (see script 13); the loop-
# geometry classification is read from closeloop_diagnostic.csv.
#
# Out: <OUT_DIR>/figures/FigS6_loop_closure.png
# Run: Rscript scripts/28_loop_closure_figure.R   (after script 13)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R"))

MIN_URBAN_LEGS <- 3
diagf <- file.path(OUT_DIR, "closeloop_diagnostic.csv")
DIAG <- if (file.exists(diagf)) read.csv(diagf, stringsAsFactors = FALSE) else NULL
flights <- Filter(function(p) grepl("ARL-Suite", p), list_flights(DATA_DIR))

panels <- list()
for (p in flights) {
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb","TrackAngleTrue","ALTGPS") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  legs <- tag_region(leg_metrics(d))
  urb <- legs[legs$region == "urban" & legs$agl_m < 3000 & legs$length_km < 20, ]
  if (nrow(urb) < MIN_URBAN_LEGS) next
  s <- d[d$leg_id %in% urb$leg_id & is.finite(d$Latitude) & is.finite(d$Longitude), ]
  if (nrow(s) < 20) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub(".ict", "", basename(p)))
  encl <- if (!is.null(DIAG)) isTRUE(DIAG$encloses_metro[match(fl, DIAG$flight)] %in% c(TRUE,"TRUE")) else FALSE
  panels[[fl]] <- list(s = s, fl = fl, encl = encl)
}
if (!length(panels)) { message("No flights with an urban-leg set."); quit(save = "no") }

FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
nc <- min(4, length(panels)); nr <- ceiling(length(panels)/nc)
# common map extent: the metro box plus a margin, and all flown legs
alllon <- unlist(lapply(panels, function(z) z$s$Longitude))
alllat <- unlist(lapply(panels, function(z) z$s$Latitude))
xr <- range(c(alllon, URBAN_BOX$lon_w, URBAN_BOX$lon_e), na.rm = TRUE)
yr <- range(c(alllat, URBAN_BOX$lat_s, URBAN_BOX$lat_n), na.rm = TRUE)
png(file.path(FIG, "FigS6_loop_closure.png"), width = 380*nc, height = 380*nr, res = 130)
par(mfrow = c(nr, nc), mar = c(3.2, 3.2, 2.6, 0.6), mgp = c(1.9, 0.6, 0))
for (pn in panels) {
  s <- pn$s
  plot(NA, xlim = xr, ylim = yr, asp = 1/cos(mean(yr)*pi/180),
       xlab = "Longitude", ylab = "Latitude",
       main = sprintf("%s\n%s", pn$fl, if (pn$encl) "encircles metro box" else "does not encircle metro box"),
       col.main = if (pn$encl) "#1e8449" else "#c0392b", cex.main = 0.9)
  rect(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n,
       border = "#0a7d0a", lwd = 2)                                  # metro analysis box
  for (g in unique(s$leg_id)) { q <- s[s$leg_id == g, ]
    lines(q$Longitude, q$Latitude, col = "#1f3864", lwd = 2) }       # flown urban legs
  points(s$Longitude[1], s$Latitude[1], pch = 19, col = "#c0392b", cex = 0.8)
}
dev.off()
message(sprintf("Wrote FigS6_loop_closure.png (%d flights; %d encircle the metro box).",
                length(panels), sum(vapply(panels, function(z) isTRUE(z$encl), logical(1)))))
