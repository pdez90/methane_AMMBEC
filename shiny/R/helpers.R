# helpers.R -- data loading, the endmember arithmetic and shared plot pieces for app.R.
# Sourced automatically by Shiny from the app's R/ folder.

suppressPackageStartupMessages({ library(ggplot2) })

DATA <- "data"
rd <- function(f) { p <- file.path(DATA, f); if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) else NULL }
have <- function(f) file.exists(file.path(DATA, f))

## ---- colours (fixed roles; see the paper's figures) --------------------------------
COL <- list(fossil = "#d95926", biogenic = "#008300", mixed = "#eda100", other = "#8a8985",
            ink = "#0b0b0b", ink2 = "#52514e", grid = "#e6e5e1", surface = "#fcfcfb",
            accent = "#2a78d6", violet = "#4a3aa7")
SERIES <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#4a3aa7", "#e34948")
FLIGHT_LABEL <- function(f) {
  f <- sub("_R0", "", f); d <- substr(f, 1, 8); suf <- sub("^[0-9]+_?", "", f)
  paste0(as.integer(substr(d, 7, 8)), " ", month.abb[as.integer(substr(d, 5, 6))], ifelse(nzchar(suf), paste0(" ", suf), ""))
}

theme_app <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = COL$grid, linewidth = 0.4),
          axis.title = element_text(colour = COL$ink2), axis.text = element_text(colour = COL$ink2),
          plot.title = element_text(face = "bold", size = base + 1, colour = COL$ink),
          plot.subtitle = element_text(colour = COL$ink2), legend.position = "bottom",
          legend.title = element_text(colour = COL$ink2), strip.text = element_text(colour = COL$ink, face = "bold"),
          plot.background = element_rect(fill = "white", colour = NA))
}

## ---- endmember arithmetic ---------------------------------------------------------------
# Fossil fraction = slope / beta, in percent, clamped to [0, 100]. The leg-block bootstrap
# interval reported in Table 1 at 0.102 scales the same way (beta only rescales).
ff_pct <- function(slope, beta) pmax(0, pmin(100, 100 * slope / beta))

## ---- background (same construction as R/enhancements.R rolling_baseline) ------------------
# Low quantile q within a centred window of `window` samples; window = Inf gives one constant
# value (the quantile of the whole series). Used by the Background tab to draw the background
# under the chosen setting; the fits themselves come precomputed from script 104.
rolling_baseline <- function(x, window = 300, q = 0.05) {
  n <- length(x)
  if (!is.finite(window)) return(rep(stats::quantile(x, probs = q, na.rm = TRUE, names = FALSE), n))
  half <- window %/% 2; bg <- numeric(n)
  for (i in seq_len(n)) { lo <- max(1, i - half); hi <- min(n, i + half)
    bg[i] <- stats::quantile(x[lo:hi], probs = q, na.rm = TRUE, names = FALSE) }
  bg
}
scale_ci <- function(pct_at_adopted, beta, adopted = 0.102) pmax(0, pmin(100, pct_at_adopted * adopted / beta))

# Reference endmembers (mol/mol) offered as presets. Values and sources as in the paper.
REFS <- data.frame(
  key    = c("arc", "daley", "adopted", "psco", "psco2018", "kille", "tzompa", "fried", "ec_lo", "ec_hi", "lean"),
  label  = c("0.0813  production-area ground ratio, AMMBEC 2024 (ARL car)",
             "~0.10  2021 basin mass balance, ethane / O&G methane (Daley et al. 2026)",
             "0.102  adopted; Gilman et al. (lowest published Front Range value)",
             "0.110  gas delivered to Denver, Jun-Jul 2024 (PSCo)",
             "0.147  gas delivered to Denver, 2018 (PSCo)",
             "0.161  Kille et al. 2019",
             "0.170  Tzompa-Sosa et al.",
             "0.187  Fried et al.",
             "0.019  East Coast delivered gas, low (Plant et al. 2019)",
             "0.037  East Coast delivered gas, high (Plant et al. 2019)",
             "0.061  assumed lean basin scenario (not a literature value)"),
  value  = c(0.0813, 0.10, 0.102, 0.110, 0.147, 0.161, 0.170, 0.187, 0.0187, 0.0367, 0.061),
  group  = c("regional reference", "regional reference", "regional reference", "regional reference",
             "historical / published", "historical / published", "historical / published", "historical / published",
             "other cities", "other cities", "assumed scenario"),
  stringsAsFactors = FALSE)

## ---- shared geography -----------------------------------------------------------------
BOX <- list(lat_s = 39.50, lat_n = 39.95, lon_w = -105.20, lon_e = -104.55)
BASIN_LAT <- 40.05
box_df <- data.frame(xmin = BOX$lon_w, xmax = BOX$lon_e, ymin = BOX$lat_s, ymax = BOX$lat_n)
METRO <- rd("denver7_metro_outline.csv")
FAC <- rd("biogenic_sources.csv")

geo_layers <- function(metro = TRUE, facilities = TRUE) {
  L <- list(geom_rect(data = box_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                      fill = NA, colour = COL$ink, linewidth = 0.5, inherit.aes = FALSE))
  if (metro && !is.null(METRO) && all(c("lon", "lat") %in% names(METRO))) {
    grp <- if ("part" %in% names(METRO)) METRO$part else 1
    L <- c(L, list(geom_path(data = cbind(METRO, g = grp), aes(lon, lat, group = g), colour = COL$ink2,
                             linewidth = 0.3, linetype = "22", inherit.aes = FALSE)))
  }
  if (facilities && !is.null(FAC)) {
    F <- FAC; F$short <- c("Suncor refinery", "Metro Water Recovery", "DADS landfill")[match(F$type, c("refinery", "wastewater", "landfill"))]
    F$short[is.na(F$short)] <- F$name[is.na(F$short)]
    F$vj <- ifelse(F$type == "wastewater", 1.4, -0.5)
    L <- c(L, list(geom_point(data = F, aes(lon, lat), shape = 3, size = 3, stroke = 1.1, colour = COL$ink, inherit.aes = FALSE),
                   geom_text(data = F, aes(lon, lat, label = short, vjust = vj), size = 3, hjust = -0.1,
                             colour = COL$ink, inherit.aes = FALSE)))
  }
  L
}
coord_denver <- function(xlim = c(-105.35, -104.4), ylim = c(39.4, 40.1))
  coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE)

# diverging green -> grey -> orange-red for a fossil fraction in [0, 1]
ff_scale <- function(name = "fossil fraction", limits = c(0, 1), ...)
  scale_fill_gradient2(name, low = COL$biogenic, mid = "#d9d8d2", high = COL$fossil, midpoint = mean(limits), limits = limits, ...)
ff_colour_scale <- function(name = "fossil fraction", limits = c(0, 1), ...)
  scale_colour_gradient2(name, low = COL$biogenic, mid = "#d9d8d2", high = COL$fossil, midpoint = mean(limits), limits = limits, ...)

fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
