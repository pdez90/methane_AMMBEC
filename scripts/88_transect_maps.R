# 88_transect_maps.R -------------------------------------------------------------
# Constant-altitude transects (R/transects.R) for every flight, from navigation data
# only, with a map and an altitude-time panel per flight so the transects can be
# checked against what was flown BEFORE any gas analysis uses them. For comparison
# the panels also mark the level legs of the current detector (detect_level_legs).
# No trace-gas variable is read or plotted.
#
# Out: <OUT_DIR>/transects.csv, <OUT_DIR>/transects_vs_legs.csv,
#      <OUT_DIR>/figures/transects/<flight>.png, <OUT_DIR>/figures/transects_all.pdf
# Run: Rscript scripts/88_transect_maps.R
# -----------------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths", "massbalance", "transects")) source(file.path(proj, "R", paste0(f, ".R")))
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
FIG <- file.path(OUT_DIR, "figures", "transects"); dir.create(FIG, recursive = TRUE, showWarnings = FALSE)
metro <- file.path(proj, "denver7_metro_outline.csv"); metro <- if (file.exists(metro)) read.csv(metro) else NULL
box <- data.frame(xmin = -105.20, xmax = -104.55, ymin = 39.50, ymax = 39.95)
PAL <- c("#2a78d6", "#eb6834", "#1baf7a", "#e34948", "#4a3aa7", "#eda100", "#e87ba4", "#008300")

TT <- list(); CMP <- list(); plots <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  d <- read_icartt(p)$data
  d <- detect_transects(d)
  old <- .detect_level_legs_vspeed(d)$leg_id
  tt <- transect_table(d); if (is.null(tt)) { message(fl, ": no transects"); next }
  tt$flight <- fl; TT[[fl]] <- tt
  reg <- sample_region(d$Latitude, d$Longitude)
  CMP[[fl]] <- data.frame(flight = fl, n_transects = nrow(tt), n_transects_urban = sum(tt$n_urban > 0),
                          urban_s_in_transects = sum(reg == "urban" & d$transect_id > 0),
                          n_old_legs = length(unique(old[old > 0])),
                          urban_s_in_old_legs = sum(reg == "urban" & old > 0),
                          old_legs_per_transect_median = {
                            m <- tapply(old[d$transect_id > 0 & old > 0], d$transect_id[d$transect_id > 0 & old > 0],
                                        function(v) length(unique(v)))
                            if (length(m)) stats::median(m) else NA })

  # ---- figure: map + altitude/time (navigation only) ----
  i10 <- seq(1, nrow(d), by = 5)
  z <- data.frame(t = d$timestamp[i10], lon = d$Longitude[i10], lat = d$Latitude[i10], alt = d$ALTGPS[i10],
                  tr = d$transect_id[i10], old = old[i10])
  z$col <- ifelse(z$tr > 0, PAL[(z$tr - 1) %% length(PAL) + 1], NA)
  lab <- aggregate(cbind(lon, lat, alt, t = as.numeric(t)) ~ tr, z[z$tr > 0, ], function(v) v[ceiling(length(v) / 2)])
  ylim <- range(c(39.4, 40.1, z$lat[z$lat < 40.7]), na.rm = TRUE); xlim <- range(c(-105.35, -104.4, z$lon[z$lon > -105.6 & z$lon < -104.0]), na.rm = TRUE)
  m <- ggplot() +
    geom_rect(data = box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA, colour = "grey20", linewidth = 0.5) +
    geom_hline(yintercept = 40.05, linetype = "22", colour = "grey40", linewidth = 0.3) +
    { if (!is.null(metro)) geom_path(data = metro, aes(lon, lat, group = if ("part" %in% names(metro)) part else 1), colour = "grey60", linewidth = 0.25) } +
    geom_path(data = z, aes(lon, lat), colour = "grey80", linewidth = 0.3) +
    geom_point(data = z[z$tr > 0, ], aes(lon, lat), colour = z$col[z$tr > 0], size = 0.35) +
    geom_label(data = lab, aes(lon, lat, label = tr), size = 2.4, label.padding = unit(0.08, "lines"), alpha = 0.8) +
    coord_quickmap(xlim = xlim, ylim = ylim) + labs(x = NULL, y = NULL, title = paste0(FLIGHT <- fl, ": ", nrow(tt), " transects")) +
    theme_minimal(base_size = 9) + theme(plot.title = element_text(face = "bold"))
  a <- ggplot(z, aes(t, alt)) + geom_line(colour = "grey75", linewidth = 0.3) +
    geom_point(data = z[z$tr > 0, ], colour = z$col[z$tr > 0], size = 0.35) +
    geom_text(data = lab, aes(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), alt + 120, label = tr), size = 2.3) +
    geom_segment(data = z[z$old > 0, ], aes(x = t, xend = t, y = min(z$alt, na.rm = TRUE) - 250, yend = min(z$alt, na.rm = TRUE) - 150),
                 colour = "black", linewidth = 0.2) +
    labs(x = "UTC", y = "GPS altitude (m)", subtitle = "coloured: constant-altitude transects; black ticks along the bottom: current level legs") +
    theme_minimal(base_size = 9)
  g <- m + a + plot_layout(widths = c(1, 1.3))
  ggsave(file.path(FIG, paste0(fl, ".png")), g, width = 11, height = 4.6, dpi = 130, bg = "white")
  plots[[fl]] <- g
  message(sprintf("%s: %d transects (%d touch the box), %d current legs", fl, nrow(tt), sum(tt$n_urban > 0), CMP[[fl]]$n_old_legs))
}
T_ <- do.call(rbind, TT); T_ <- T_[, c("flight", setdiff(names(T_), "flight"))]
write.csv(T_, file.path(OUT_DIR, "transects.csv"), row.names = FALSE)
write.csv(do.call(rbind, CMP), file.path(OUT_DIR, "transects_vs_legs.csv"), row.names = FALSE)
pdf(file.path(OUT_DIR, "figures", "transects_all.pdf"), width = 11, height = 4.6)
for (g in plots) print(g)
invisible(dev.off())
message("wrote transects.csv, transects_vs_legs.csv, figures/transects/*.png, figures/transects_all.pdf")
