# AMMBEC Denver methane explorer ------------------------------------------------
# Interactive companion to "Airborne ethane reveals biogenic-dominated urban methane
# in Denver". Reads the pipeline outputs in data/ (written by scripts/86_shiny_export.R)
# and lets the reader choose the ethane endmember, look at the aircraft and ground
# data, the inventories and the STILT transport results.
#
# Run locally:   shiny::runApp("shiny")
# Deploy:        rsconnect::writeManifest("shiny"); then publish from GitHub on
#                Posit Connect Cloud (entrypoint shiny/app.R).
# -------------------------------------------------------------------------------
suppressPackageStartupMessages({ library(shiny); library(bslib); library(ggplot2); library(DT) })

## ---- data ------------------------------------------------------------------------
T1   <- rd("table1_full.csv");        T1 <- T1[is.finite(T1$ethane_slope_within), ]
EM   <- rd("emission_estimates.csv"); SA <- rd("source_attribution.csv")
BETA <- rd("beta_sensitivity.csv");   BRK <- rd("beta_breakeven.csv")
TWO  <- rd("two_endmember_sensitivity.csv")
PL   <- rd("perleg_fossil_fraction.csv")
LEGS <- rd("legs.csv"); TR <- rd("tracks.csv"); US <- rd("urban_samples.csv"); BSM <- rd("basin_samples.csv")
WF   <- rd("wind_fossil.csv"); ST <- rd("stilt_exposure_legs.csv"); STS <- rd("stilt_exposure_summary.csv")
ARC  <- rd("arc_ground_plumes.csv"); ARCR <- rd("arc_ground_ratios.csv"); ARCS <- rd("arc_ground_summary.csv")
CDH  <- rd("cdphe_hotspots.csv"); CDT <- rd("mobile_trends.csv"); CDF <- rd("mobile_trend_fits.csv")
FSG  <- rd("fossil_signature_grid.csv"); SMC <- rd("spatial_match_cells.csv")
EPA  <- rd("epa_cells.csv"); G11 <- rd("gra2pes_cells_v1.1.csv"); G20 <- rd("gra2pes_cells_v2.0beta.csv")
G11S <- rd("gra2pes_cells_v1.1_summary.csv"); G20S <- rd("gra2pes_cells_v2.0beta_summary.csv")
HOT  <- rd("v2_pointsource_hotspots.csv"); CM <- rd("carbonmapper_denver_sources.csv")
BAS  <- rd("basin_ethane_methane.csv"); BASS <- rd("basin_summary.csv")
PV   <- if (have("paper_values.json")) jsonlite::fromJSON(file.path(DATA, "paper_values.json")) else list()
LIT  <- rd("literature_basin_ratios.csv")

T1$label <- FLIGHT_LABEL(T1$flight)
FLIGHTS <- T1$flight; names(FLIGHTS) <- T1$label
ALL_FLIGHTS <- sort(unique(TR$flight)); names(ALL_FLIGHTS) <- FLIGHT_LABEL(ALL_FLIGHTS)
LEGS$label <- FLIGHT_LABEL(LEGS$flight)
SA <- SA[SA$method == "CH4:CO x GRA2PES", ]

## ---- UI --------------------------------------------------------------------------
theme <- bs_theme(version = 5, preset = "shiny", primary = "#2a78d6", base_font = "system-ui, -apple-system, \"Segoe UI\", Roboto, sans-serif",
                  font_scale = 0.95)

endmember_sidebar <- function(id = "beta") sidebar(
  width = 330,
  h6("Ethane endmember β", class = "mb-1"),
  sliderInput(id, NULL, min = 0.03, max = 0.20, value = 0.102, step = 0.0001, width = "100%"),
  radioButtons(paste0(id, "_ref"), "Reference values", width = "100%",
               choiceNames = lapply(seq_len(nrow(REFS)), function(i) tags$span(REFS$label[i], style = "font-size:0.85em")),
               choiceValues = REFS$key, selected = "adopted"),
  tags$small(class = "text-muted",
             "The adopted 0.102 and the regional reference values 0.081-0.110 are the paper's; 0.061 is an assumed ",
             "scenario, not a literature value (Daley et al. give about 0.10 for ethane against oil-and-gas methane). ",
             "The fossil fraction is the within-leg York slope divided by β, so β only rescales flights.")
)

ui <- page_navbar(
  title = "AMMBEC Denver methane explorer", theme = theme, fillable = FALSE,
  header = tags$style(HTML(".card-header{font-weight:600} .stat{font-size:1.9rem;font-weight:700;line-height:1.1}
                            .stat-label{color:#52514e;font-size:0.85rem} .small-note{font-size:0.85rem;color:#52514e}")),

  nav_panel("Overview",
    layout_columns(col_widths = c(8, 4),
      card(card_header("What this is"),
        p("A companion to the paper ", em("Airborne ethane reveals biogenic-dominated urban methane in Denver"),
          " (2024 AMMBEC campaign). Everything here is read from the published pipeline outputs ",
          "(", a("github.com/pdez90/methane_AMMBEC", href = "https://github.com/pdez90/methane_AMMBEC", target = "_blank"),
          "); nothing is re-fitted in the browser except the division by the endmember you choose."),
        p("Fossil (thermogenic) methane co-emits ethane; biogenic methane does not. For each flight the paper fits the ",
          "ethane-to-methane slope of the urban enhancements and converts it to a fossil fraction by dividing by an ",
          "ethane endmember β. That endmember is the dominant systematic uncertainty, so the ", strong("Endmember"),
          " tab lets you set it, with the literature and campaign reference values as presets, and watch every flight move."),
        tags$ul(
          tags$li(strong("Aircraft"), ": flight tracks, urban level legs and the per-leg ethane:methane scatter behind each flight's slope."),
          tags$li(strong("Ground plumes"), ": the NOAA ARL car's plume ratios (ethane-bearing vs ethane-free) and CDPHE's facility surveys."),
          tags$li(strong("Inventories"), ": EPA gridded GHGI and GRA2PES v1.1 / v2.0beta cells against the aircraft-sampled cells."),
          tags$li(strong("STILT"), ": HRRR-STILT footprint exposure of each urban leg to basin oil-and-gas, distribution gas, waste and livestock sources."))),
      card(card_header("Headline numbers (paper, β = 0.102)"),
        layout_columns(col_widths = c(6, 6),
          div(div(class = "stat", paste0(PV$fossil_median, "%")), div(class = "stat-label", "campaign-median fossil fraction")),
          div(div(class = "stat", paste0(PV$fossil_min, "-", PV$fossil_max, "%")), div(class = "stat-label", "range across 7 flights")),
          div(div(class = "stat", paste0(fmt(PV$gra_lo, 1), "-", fmt(PV$gra_hi, 1))), div(class = "stat-label", "t CH4/h, CH4:CO x GRA2PES (4 flights)")),
          div(div(class = "stat", "0.059"), div(class = "stat-label", "β below which the median would exceed 50%"))),
        tags$small(class = "text-muted", "27-36% across the regional reference endmembers 0.081-0.110; 48% under an assumed 0.061."))),
    card(card_header("Caveats"),
      tags$ul(class = "small-note",
        tags$li("The flights were designed for a basin mass balance, not a metropolitan one; the emission rates are enhancement-ratio estimates scaled by inventory CO and CO2 and are less certain than the source mix."),
        tags$li("GRA2PES v2.0beta is a beta product (NOAA CSL, June 2026) whose methods are still being documented; only its 4-km box cells are shown."),
        tags$li("The CDPHE surveys (2023-2025) are not coincident with the flights; the ARL car drove during the campaign but mostly along the northern edge of the box."),
        tags$li("Summer conditions only (28 June to 13 July 2024).")))),

  nav_panel("Endmember",
    layout_sidebar(sidebar = endmember_sidebar("beta"),
      layout_columns(col_widths = c(4, 4, 4),
        value_box("Campaign median", textOutput("vb_median"), showcase = NULL, theme = "primary"),
        value_box("Flights at or above 50%", textOutput("vb_majority")),
        value_box("Range across flights", textOutput("vb_range"))),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Fossil fraction by flight at the chosen β"), plotOutput("p_flights", height = 380)),
        card(card_header("Median across the sensitivity range"), plotOutput("p_sweep", height = 380))),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Two-endmember mixture: distribution gas + basin gas"),
          layout_columns(col_widths = c(4, 4, 4),
            selectInput("bdist", "Distribution-gas ratio", choices = c("0.110 Denver delivered gas (PSCo 2024)" = 0.1101,
                        "0.037 East Coast high" = 0.0367, "0.027 East Coast mean" = 0.027, "0.019 East Coast low" = 0.0187)),
            selectInput("bdjb", "Basin ratio", choices = c("0.0813 production-area" = 0.0813, "0.102 adopted" = 0.102, "0.061 assumed lean" = 0.061)),
            sliderInput("pdist", "Share p of urban fossil methane that is distribution gas", 0, 1, 0.67, 0.01)),
          plotOutput("p_two", height = 300),
          tags$small(class = "text-muted", "β_eff = p β_dist + (1 - p) β_basin. The vertical line marks the gridded inventory's share of distribution and post-meter gas in the box's fossil methane (0.67).")),
        card(card_header("Emission split at the chosen β"), plotOutput("p_split", height = 300),
          tags$small(class = "text-muted", "Top-down: each screen-passing flight's CH4:CO emission split by its fossil fraction at β; the bar is the paired median across the four flights (Figure 4A, section 3.5). Inventories for comparison."))),
      card(card_header("Per-flight table"), DTOutput("t_flights")))),

  nav_panel("Aircraft",
    layout_sidebar(sidebar = sidebar(width = 300,
        selectInput("flight", "Flight", choices = ALL_FLIGHTS, selected = "20240710_R0_L1"),
        checkboxInput("show_track", "Show the full 10-s track", TRUE),
        checkboxInput("show_basin", "Show basin legs too", FALSE),
        sliderInput("beta_air", "β for the per-leg fossil fractions", 0.03, 0.20, 0.102, 0.0001),
        tags$small(class = "text-muted", "Urban legs are level legs inside the box (39.50-39.95°N, 105.20-104.55°W); basin legs lie north of 40.05°N. ",
                   "Enhancements use the paper's rolling 5th-percentile background over the flight's urban legs joined end to end. Points above the 20 ppb gate enter the fits.")),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Track and level legs"), plotOutput("p_map", height = 520)),
        card(card_header("Urban legs: ethane vs methane enhancement (gated samples)"), plotOutput("p_scatter", height = 520))),
      card(card_header("Level legs on this flight"), DTOutput("t_legs")))),

  nav_panel("Ground plumes",
    navset_card_tab(
      nav_panel("NOAA ARL car (24 Jun - 15 Jul 2024)",
        layout_columns(col_widths = c(6, 6),
          card(card_header("Plumes crossed, by class"), plotOutput("p_arc_map", height = 600)),
          card(card_header("Ethane:methane ratio of ethane-bearing plumes"), plotOutput("p_arc_hist", height = 600),
            tags$small(class = "text-muted", "Plume slopes are York fits on the plume's samples. 'Ethane-bearing' needs r ≥ 0.7 and slope ≥ 0.01; 'ethane-free' |r| < 0.3 and slope < 0.01."))),
        card(card_header("Per-day ambient fits (gated construction, as for the aircraft)"), DTOutput("t_arc"))),
      nav_panel("CDPHE facility surveys (2023-2025)",
        layout_sidebar(sidebar = sidebar(width = 260,
            selectInput("site", "Site", choices = c("Suncor P66 refinery" = "SuncorP66", "HEP terminal" = "HEPTerminal", "Collins Aerospace" = "CollinsAerospace")),
            tags$small(class = "text-muted", "Vehicle-based CH4 surveys repeated over 2023-2025. Plume samples (enhancement above a 2-min rolling background) are binned to ~50 m; colour is the 95th percentile of the enhancement in the bin.")),
          layout_columns(col_widths = c(6, 6),
            card(card_header("Where the plumes are"), plotOutput("p_cd_map", height = 460)),
            card(card_header("Survey-to-survey trend of the 95th-percentile enhancement"), plotOutput("p_cd_trend", height = 460))))))),

  nav_panel("Inventories",
    layout_sidebar(sidebar = sidebar(width = 300,
        selectInput("inv", "Inventory", choices = c("EPA gridded GHGI 2020 (0.1°)" = "epa", "GRA2PES v1.1 (4 km, Jul 2023)" = "g11", "GRA2PES v2.0beta (4 km, Jul 2023)" = "g20")),
        radioButtons("inv_metric", "Show", choices = c("fossil fraction" = "ff", "total CH4 (t/h per cell)" = "tot")),
        sliderInput("beta_inv", "β for the aircraft cells", 0.03, 0.20, 0.102, 0.0001),
        tags$small(class = "text-muted", "Aircraft cells are the 0.02° cells with at least 12 in-plume boundary-layer samples spanning 50 ppb, coloured by their local slope / β (Figure 4B). ",
                   "Fossil fraction is fossil / (fossil + biogenic); GRA2PES fossil = oil-and-gas + post-meter, biogenic = waste + agriculture.")),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Inventory cells with the aircraft cells overlaid"), plotOutput("p_inv_map", height = 600)),
        card(card_header("Cell by cell: aircraft vs inventory"), plotOutput("p_inv_scatter", height = 380), uiOutput("inv_stats"))),
      card(card_header("Box totals"), DTOutput("t_inv")))),

  nav_panel("STILT",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Predicted enhancement by source class, per urban leg"), plotOutput("p_st_bars", height = 440),
        tags$small(class = "text-muted", "HRRR-driven STILT footprints (10 h back, 500 particles from 4 receptors per leg) convolved with the gridded EPA GHGI, by source class.")),
      card(card_header("Observed slope vs modelled basin oil-and-gas share"), plotOutput("p_st_scatter", height = 440),
        uiOutput("st_stats"))),
    card(card_header("Leg table"), DTOutput("t_st"))),

  nav_spacer(),
  nav_item(a("Code and data", href = "https://github.com/pdez90/methane_AMMBEC", target = "_blank"))
)

## ---- server ----------------------------------------------------------------------
server <- function(input, output, session) {

  ## endmember presets drive the slider
  observeEvent(input$beta_ref, {
    v <- REFS$value[REFS$key == input$beta_ref]
    updateSliderInput(session, "beta", value = v)
  }, ignoreInit = TRUE)

  flights_at <- reactive({
    b <- input$beta
    data.frame(flight = T1$flight, label = T1$label, legs = T1$urban_legs, slope = T1$ethane_slope_within, r = T1$ethane_r,
               ff = ff_pct(T1$ethane_slope_within, b),
               lo = scale_ci(T1$fossil_lo, b), hi = scale_ci(T1$fossil_hi, b), stringsAsFactors = FALSE)
  })
  output$vb_median <- renderText(sprintf("%.0f%%", median(flights_at()$ff)))
  output$vb_majority <- renderText({ f <- flights_at(); sprintf("%d of %d", sum(f$ff >= 50), nrow(f)) })
  output$vb_range <- renderText({ f <- flights_at(); sprintf("%.0f to %.0f%%", min(f$ff), max(f$ff)) })

  output$p_flights <- renderPlot({
    f <- flights_at(); f <- f[order(f$ff), ]; f$label <- factor(f$label, levels = f$label)
    ggplot(f, aes(label, ff)) +
      geom_hline(yintercept = 50, linetype = "22", colour = COL$ink2) +
      geom_hline(yintercept = median(f$ff), colour = COL$accent, linewidth = 0.8) +
      geom_linerange(aes(ymin = lo, ymax = hi), colour = COL$ink2, linewidth = 0.6, na.rm = TRUE) +
      geom_point(aes(colour = ff), size = 4) + ff_colour_scale(limits = c(0, 100), guide = "none") +
      geom_text(aes(label = sprintf("%.0f", ff)), vjust = -1.1, size = 3.3, colour = COL$ink) +
      annotate("text", x = 0.6, y = median(f$ff), label = sprintf("median %.0f%%", median(f$ff)), hjust = 0, vjust = -0.5, colour = COL$accent, size = 3.5) +
      scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
      labs(x = NULL, y = "fossil fraction (%)", subtitle = sprintf("β = %.4g; lines: 95%% leg-block bootstrap intervals", input$beta)) +
      theme_app() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }, res = 96)

  output$p_sweep <- renderPlot({
    bg <- seq(0.03, 0.20, by = 0.001)
    med <- sapply(bg, function(b) median(ff_pct(T1$ethane_slope_within, b)))
    lo  <- sapply(bg, function(b) min(ff_pct(T1$ethane_slope_within, b))); hi <- sapply(bg, function(b) max(ff_pct(T1$ethane_slope_within, b)))
    d <- data.frame(bg, med, lo, hi); be <- if (!is.null(BRK)) BRK$beta_breakeven[1] else NA
    refs <- REFS[REFS$group == "regional reference", ]
    ggplot(d, aes(bg, med)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = COL$accent, alpha = 0.12) +
      geom_hline(yintercept = 50, linetype = "22", colour = COL$ink2) +
      geom_line(colour = COL$accent, linewidth = 1) +
      geom_vline(xintercept = refs$value, colour = COL$ink2, linewidth = 0.3, linetype = "13") +
      geom_vline(xintercept = input$beta, colour = COL$fossil, linewidth = 0.9) +
      annotate("point", x = input$beta, y = median(ff_pct(T1$ethane_slope_within, input$beta)), colour = COL$fossil, size = 3.5) +
      annotate("text", x = be, y = 97, label = sprintf("break-even %.3f", be), hjust = -0.05, size = 3.2, colour = COL$ink2) +
      geom_vline(xintercept = be, colour = COL$ink2, linewidth = 0.4) +
      scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
      labs(x = "ethane endmember β (mol/mol)", y = "campaign median (%); band: flight range",
           subtitle = "dotted: reference values") + theme_app()
  }, res = 96)

  output$p_two <- renderPlot({
    bd <- as.numeric(input$bdist); bj <- as.numeric(input$bdjb); p <- seq(0, 1, 0.01)
    beff <- p * bd + (1 - p) * bj
    med <- sapply(beff, function(b) median(ff_pct(T1$ethane_slope_within, b)))
    nmaj <- sapply(beff, function(b) sum(ff_pct(T1$ethane_slope_within, b) >= 50))
    d <- data.frame(p, beff, med, nmaj); here <- d[which.min(abs(d$p - input$pdist)), ]
    ggplot(d, aes(p, med)) + geom_hline(yintercept = 50, linetype = "22", colour = COL$ink2) +
      geom_vline(xintercept = 0.67, colour = COL$ink2, linewidth = 0.4) +
      geom_line(colour = COL$violet, linewidth = 1) +
      geom_point(data = here, colour = COL$fossil, size = 3.5) +
      annotate("label", x = here$p, y = here$med, label = sprintf("β_eff = %.3f\nmedian %.0f%%, %d flight(s) ≥ 50%%", here$beff, here$med, here$nmaj),
               hjust = if (here$p > 0.5) 1.1 else -0.1, vjust = -0.3, size = 3.2, fill = "white", label.size = 0) +
      scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
      labs(x = "share p of urban fossil methane that is distribution gas", y = "campaign median fossil fraction (%)") + theme_app()
  }, res = 96)

  output$p_split <- renderPlot({
    b <- input$beta
    sa <- merge(SA, T1[, c("flight", "ethane_slope_within")], by = "flight")
    ffb <- ff_pct(sa$ethane_slope_within, b) / 100
    # campaign-representative split: paired medians across the screen-passing flights (script 23)
    td <- data.frame(src = "Top-down\n(CH4:CO x GRA2PES CO)", cls = c("fossil", "biogenic"),
                     val = c(median(sa$E_total_t_hr * ffb), median(sa$E_total_t_hr * (1 - ffb))))
    inv <- rbind(
      data.frame(src = "EPA GHGI\n2020", cls = c("fossil", "biogenic", "other"), val = c(PV$epa_fossil, PV$epa_biogenic, PV$epa_combustion + PV$epa_other)),
      if (!is.null(G11S)) data.frame(src = "GRA2PES\nv1.1", cls = c("fossil", "biogenic", "other"),
                                     val = c(G11S$og_t_hr + G11S$postmeter_t_hr, G11S$waste_t_hr + G11S$ag_t_hr, G11S$other_t_hr)),
      if (!is.null(G20S)) data.frame(src = "GRA2PES\nv2.0beta", cls = c("fossil", "biogenic", "other"),
                                     val = c(G20S$og_t_hr + G20S$postmeter_t_hr, G20S$waste_t_hr + G20S$ag_t_hr, G20S$other_t_hr)))
    d <- rbind(td, inv); d$cls <- factor(d$cls, c("other", "biogenic", "fossil")); d$src <- factor(d$src, unique(d$src))
    tot <- aggregate(val ~ src, d, sum); fos <- aggregate(val ~ src, d[d$cls == "fossil", ], sum); fb <- aggregate(val ~ src, d[d$cls != "other", ], sum)
    lab <- merge(merge(tot, fos, by = "src", suffixes = c("", "_f")), fb, by = "src", suffixes = c("", "_fb"))
    lab$txt <- sprintf("%.1f t/h\n%.0f%% fossil", lab$val, 100 * lab$val_f / lab$val_fb)
    ggplot(d, aes(src, val, fill = cls)) + geom_col(width = 0.7, colour = "white", linewidth = 0.6) +
      geom_text(data = lab, aes(src, val, label = txt), inherit.aes = FALSE, vjust = -0.3, size = 3.2, colour = COL$ink) +
      scale_fill_manual(NULL, values = c(fossil = COL$fossil, biogenic = COL$biogenic, other = COL$other),
                        labels = c(fossil = "fossil (gas system)", biogenic = "biogenic (waste, agriculture)", other = "combustion / other"), breaks = c("fossil", "biogenic", "other")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + guides(fill = guide_legend(nrow = 3)) +
      labs(x = NULL, y = "urban CH4 emission (t/h)") + theme_app()
  }, res = 96)

  output$t_flights <- renderDT({
    f <- flights_at(); e <- EM[match(f$flight, EM$flight), ]
    out <- data.frame(Flight = f$label, `Urban legs` = f$legs, `Within-leg slope` = signif(f$slope, 3), r = round(f$r, 2),
                      `Fossil % at β` = round(f$ff), `95% interval` = ifelse(is.finite(f$lo), sprintf("%.0f-%.0f", f$lo, f$hi), "one qualifying leg"),
                      `CH4:CO x GRA2PES (t/h)` = e$CH4CO_GRA2PES_t_hr, `CH4:CO2 x Vulcan (t/h)` = e$CH4CO2_Vulcan_t_hr, check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(dom = "t", pageLength = 10), class = "compact stripe")
  })

  ## ---- aircraft -------------------------------------------------------------------
  output$p_map <- renderPlot({
    fl <- input$flight; tr <- TR[TR$flight == fl, ]; lg <- LEGS[LEGS$flight == fl, ]
    us <- US[US$flight == fl, ]; bs <- if (input$show_basin) BSM[BSM$flight == fl, ] else US[0, ]
    g <- ggplot() + geo_layers()
    if (input$show_track) g <- g + geom_path(data = tr, aes(lon, lat), colour = "#b8b7b1", linewidth = 0.5)
    pts <- rbind(cbind(us, region = "urban"), if (nrow(bs)) cbind(bs, region = "basin"))
    if (nrow(pts)) g <- g + geom_point(data = pts[order(pts$dch4_ppb), ], aes(lon, lat, colour = pmin(dch4_ppb, 150)), size = 1.1) +
      scale_colour_gradient("ΔCH4 (ppb, leg samples)", low = "#dbe9f8", high = "#0b3d7a", limits = c(0, 150))
    if (nrow(lg)) g <- g + geom_text(data = lg[lg$region %in% c("urban", if (input$show_basin) "basin"), ], aes(lon, lat, label = leg_id),
                                     size = 3, fontface = "bold", colour = COL$ink)
    xl <- range(c(tr$lon, BOX$lon_w, BOX$lon_e), na.rm = TRUE) + c(-0.05, 0.05); yl <- range(c(tr$lat, BOX$lat_s, BOX$lat_n), na.rm = TRUE) + c(-0.03, 0.03)
    g + coord_denver(xl, yl) + labs(x = NULL, y = NULL, subtitle = sprintf("%s: %d urban and %d basin legs (numbers: leg ids)", FLIGHT_LABEL(fl), sum(lg$region == "urban"), sum(lg$region == "basin"))) +
      theme_app() + theme(panel.grid = element_blank(), axis.text = element_text(size = 9))
  }, res = 96)

  output$p_scatter <- renderPlot({
    fl <- input$flight; us <- US[US$flight == fl & US$gated, ]
    validate(need(nrow(us) >= 10, "This flight has fewer than ten gated urban samples (no urban fit in the paper)."))
    pl <- PL[PL$flight == fl, ]; b <- input$beta_air
    us$leg <- factor(us$leg_id); n_leg <- table(us$leg); keep <- names(n_leg)[n_leg >= 10]
    us <- us[us$leg %in% keep, ]
    lab <- data.frame(leg = factor(pl$leg_id, levels = levels(us$leg)),
                      txt = sprintf("slope %.3f, r %.2f\n%.0f%% at β", pl$leg_slope, pl$leg_r, ff_pct(pl$leg_slope, b)))
    lab <- lab[!is.na(lab$leg) & lab$leg %in% keep, ]
    sl <- T1$ethane_slope_within[T1$flight == fl]
    ggplot(us, aes(dch4_ppb, dc2h6_ppb)) +
      geom_point(colour = COL$accent, alpha = 0.55, size = 1.3) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = COL$ink, linewidth = 0.7) +
      geom_abline(slope = b, intercept = 0, colour = COL$fossil, linetype = "22", linewidth = 0.5) +
      geom_text(data = lab, aes(x = -Inf, y = Inf, label = txt), hjust = -0.05, vjust = 1.2, size = 2.9, colour = COL$ink2) +
      facet_wrap(~ leg, labeller = labeller(leg = function(x) paste("leg", x))) +
      labs(x = "ΔCH4 (ppb)", y = "ΔC2H6 (ppb)",
           subtitle = sprintf("black: OLS per leg; dashed: pure fossil at β = %.4g; flight York slope %s", b, if (length(sl)) sprintf("%.4f", sl) else "n/a")) +
      theme_app(12)
  }, res = 96)

  output$t_legs <- renderDT({
    lg <- LEGS[LEGS$flight == input$flight & LEGS$region %in% c("urban", "basin"), ]
    out <- data.frame(Leg = lg$leg_id, Region = lg$region, Samples = lg$n, `Length (km)` = lg$length_km, `AGL (m)` = lg$agl_m,
                      `Track (°)` = lg$track_deg, `Mean ΔCH4 (ppb)` = lg$ch4_enh_mean_ppb, `Gated samples` = lg$n_gated,
                      `Leg slope` = lg$leg_slope, r = lg$leg_r, `Fossil % at β` = round(ff_pct(lg$leg_slope, input$beta_air)), check.names = FALSE)
    datatable(out[order(out$Region != "urban", out$Leg), ], rownames = FALSE, options = list(dom = "tp", pageLength = 12), class = "compact stripe")
  })

  ## ---- ground ---------------------------------------------------------------------
  output$p_arc_map <- renderPlot({
    a <- ARC[is.finite(ARC$lat) & ARC$lat > 39.3 & ARC$lat < 40.8, ]
    a$class <- factor(a$class, c("fossil-type", "ethane-free", "unclassified"))
    ggplot() + geo_layers() + geom_hline(yintercept = BASIN_LAT, linetype = "22", colour = COL$ink2, linewidth = 0.4) +
      geom_point(data = a[order(a$class, decreasing = TRUE), ], aes(lon, lat, colour = class, size = ch4_enh_max_ppb), alpha = 0.8) +
      scale_colour_manual("plume class", values = c(`fossil-type` = COL$fossil, `ethane-free` = COL$biogenic, unclassified = COL$other),
                          labels = c("ethane-bearing (fossil-type)", "ethane-free", "unclassified")) +
      scale_size_area(max_size = 7, guide = "none") + coord_denver(c(-105.35, -104.3), c(39.45, 40.75)) +
      guides(colour = guide_legend(nrow = 3, override.aes = list(size = 4))) +
      labs(x = NULL, y = NULL, subtitle = "symbol size: peak ΔCH4; dashed: 40.05°N") + theme_app() + theme(panel.grid = element_blank())
  }, res = 96)
  output$p_arc_hist <- renderPlot({
    a <- ARC[ARC$class == "fossil-type" & ARC$region %in% c("urban", "basin"), ]
    a$region <- factor(a$region, c("basin", "urban"), c("basin (DJB)", "urban (inside the box)"))
    med <- aggregate(slope ~ region, a, median)
    ggplot(a, aes(slope)) + geom_histogram(binwidth = 0.01, boundary = 0, fill = COL$fossil, colour = "white") +
      geom_vline(data = med, aes(xintercept = slope), colour = COL$ink, linewidth = 0.7) +
      geom_text(data = med, aes(x = slope, y = Inf, label = sprintf("median %.3f", slope)), hjust = -0.1, vjust = 1.5, size = 3.3) +
      geom_vline(xintercept = 0.0813, linetype = "22", colour = COL$ink2) +
      facet_wrap(~ region, ncol = 1, scales = "free_y") +
      labs(x = "plume ΔC2H6 / ΔCH4 (mol/mol)", y = "plumes", subtitle = "dashed: 0.0813 production-area ambient ratio") + theme_app()
  }, res = 96)
  output$t_arc <- renderDT({
    r <- ARCR; r$date <- as.Date(r$date, "%Y%m%d")
    out <- data.frame(Date = r$date, Region = r$region, `Samples (gated)` = r$n, Plumes = r$n_plumes, `Plumes fitted` = r$n_plumes_fit,
                      `Pooled slope` = r$slope, `95% CI` = ifelse(is.finite(r$lo), sprintf("%s to %s", r$lo, r$hi), ""), check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(dom = "tp", pageLength = 12), class = "compact stripe")
  })
  output$p_cd_map <- renderPlot({
    h <- CDH[CDH$site == input$site, ]; validate(need(nrow(h) > 0, "no data"))
    h <- h[order(h$enh_p95_ppmv), ]
    ctr <- c(median(h$lon), median(h$lat)); h <- h[abs(h$lon - ctr[1]) < 0.06 & abs(h$lat - ctr[2]) < 0.05, ]
    ggplot(h, aes(lon, lat, colour = pmin(enh_p95_ppmv, 1))) + geom_point(alpha = 0.8, size = 1.6) +
      scale_colour_gradient("95th-pct ΔCH4 (ppmv, ≤1)", low = "#dbe9f8", high = "#0b3d7a") +
      geom_point(data = FAC, aes(lon, lat), shape = 3, size = 3, stroke = 1.1, colour = COL$ink, inherit.aes = FALSE) +
      coord_quickmap(xlim = range(h$lon) + c(-0.005, 0.005), ylim = range(h$lat) + c(-0.004, 0.004)) +
      labs(x = NULL, y = NULL, subtitle = sprintf("%s plume samples in %s bins", fmt(sum(h$n)), fmt(nrow(h)))) +
      theme_app() + theme(panel.grid = element_blank(), legend.key.width = unit(1.6, "cm"))
  }, res = 96)
  output$p_cd_trend <- renderPlot({
    t <- CDT[CDT$site == input$site, ]; t$date <- as.Date(t$date); f <- CDF[CDF$site == input$site, ]
    ggplot(t, aes(date, enh_p95)) + geom_point(colour = COL$accent, size = 2) +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = COL$ink, fill = COL$grid, linewidth = 0.7) +
      labs(x = NULL, y = "95th-percentile ΔCH4 per survey (ppmv)",
           subtitle = if (nrow(f)) sprintf("%d surveys over %d days\ntrend %+.3f ppmv/yr, p = %.2f", f$n_surveys, f$span_days, f$slope_ppmv_per_yr, f$p_value) else NULL) + theme_app()
  }, res = 96)

  ## ---- inventories ----------------------------------------------------------------
  inv_cells <- reactive({
    switch(input$inv,
      epa = if (!is.null(EPA)) data.frame(lon = EPA$lon, lat = EPA$lat, w = 0.1, h = 0.1, ff = EPA$fossil_frac, tot = EPA$fossil_t_hr + EPA$biogenic_t_hr),
      g11 = if (!is.null(G11)) data.frame(lon = G11$lon, lat = G11$lat, w = 0.047, h = 0.036, ff = G11$fossil_frac, tot = G11$ch4_total_t_hr),
      g20 = if (!is.null(G20)) data.frame(lon = G20$lon, lat = G20$lat, w = 0.047, h = 0.036, ff = G20$fossil_frac, tot = G20$ch4_total_t_hr))
  })
  air_cells <- reactive({
    a <- FSG[is.finite(FSG$slope), ]
    sl <- ifelse(is.finite(a$slope_york) & a$estimator == "york", a$slope_york, a$slope)   # unrounded York slope where available
    a$ff <- round(pmin(1, pmax(0, sl / input$beta_inv)), 2); a })   # 2 decimals, as the pipeline's fossil_frac
  output$p_inv_map <- renderPlot({
    ic <- inv_cells(); validate(need(!is.null(ic), "This inventory's cells are not in the exported data.")); a <- air_cells()
    g <- ggplot()
    if (input$inv_metric == "ff") g <- g + geom_tile(data = ic, aes(lon, lat, width = w, height = h, fill = ff), colour = "white", linewidth = 0.3) + ff_scale("inventory fossil fraction")
    else g <- g + geom_tile(data = ic, aes(lon, lat, width = w, height = h, fill = pmax(tot, 1e-4)), colour = "white", linewidth = 0.3) +
      scale_fill_gradient("CH4 (t/h per cell, log)", low = "#eef3fa", high = "#0b3d7a", trans = "log10")
    g + geo_layers() +
      geom_point(data = a, aes(lon, lat), shape = 21, size = 5.2, fill = "white", colour = "white") +
      geom_point(data = a, aes(lon, lat, colour = ff), size = 4) +
      ff_colour_scale("aircraft cell at β") +
      coord_denver(c(-105.3, -104.45), c(39.42, 40.0)) +
      labs(x = NULL, y = NULL, subtitle = sprintf("dots: %d aircraft cells with a local slope, at β = %.4g", nrow(a), input$beta_inv)) +
      theme_app() + theme(panel.grid = element_blank(), legend.box = "vertical", legend.key.width = unit(1.2, "cm"),
                          legend.key.height = unit(0.35, "cm"), legend.spacing.y = unit(0, "cm"))
  }, res = 96)
  matched <- reactive({
    a <- air_cells(); s <- SMC[match(paste(a$lon, a$lat), paste(SMC$lon, SMC$lat)), ]
    inv <- switch(input$inv, epa = s$epa_ff, g11 = s$gra_v11_ff, g20 = s$gra_v20beta_ff)
    d <- data.frame(air = a$ff, inv = inv); d[is.finite(d$air) & is.finite(d$inv), ]
  })
  output$p_inv_scatter <- renderPlot({
    d <- matched(); validate(need(nrow(d) >= 4, "no matched cells"))
    ggplot(d, aes(air, inv)) + geom_abline(linetype = "22", colour = COL$ink2) +
      geom_hline(yintercept = 0.5, colour = COL$grid) + geom_vline(xintercept = 0.5, colour = COL$grid) +
      geom_point(colour = COL$accent, size = 2.6, alpha = 0.8) + coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
      labs(x = sprintf("aircraft cell at β = %.4g", input$beta_inv), y = "inventory cell at the same place") + theme_app()
  }, res = 96)
  output$inv_stats <- renderUI({
    d <- matched(); if (nrow(d) < 4) return(NULL)
    cls <- function(x) ifelse(x < 1/3, "biogenic", ifelse(x > 2/3, "fossil", "mixed"))
    ct <- suppressWarnings(cor.test(d$air, d$inv, method = "spearman"))
    tags$small(class = "text-muted", sprintf("%d cells; aircraft median %.2f, inventory median %.2f; Spearman ρ = %.2f (p = %.2f); class agreement %.0f%%; inventory more fossil in %d of %d cells.",
      nrow(d), median(d$air), median(d$inv), ct$estimate, ct$p.value, 100 * mean(cls(d$air) == cls(d$inv)), sum(d$inv > d$air), nrow(d)))
  })
  output$t_inv <- renderDT({
    rows <- list(data.frame(Inventory = "EPA gridded GHGI 2020", `Total CH4 (t/h)` = PV$epa_total, `Fossil (t/h)` = PV$epa_fossil, `Biogenic (t/h)` = PV$epa_biogenic,
                            `Fossil fraction` = round(PV$epa_fossil / (PV$epa_fossil + PV$epa_biogenic), 2), check.names = FALSE))
    for (gs in list(G11S, G20S)) if (!is.null(gs)) rows[[length(rows) + 1]] <- data.frame(
      Inventory = paste("GRA2PES", gs$inventory_version, "(Jul 2023 weekday)"), `Total CH4 (t/h)` = round(gs$ch4_total_t_hr, 2),
      `Fossil (t/h)` = round(gs$og_t_hr + gs$postmeter_t_hr, 2), `Biogenic (t/h)` = round(gs$waste_t_hr + gs$ag_t_hr, 2),
      `Fossil fraction` = round(gs$fossil_frac_box, 2), check.names = FALSE)
    datatable(do.call(rbind, rows), rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  ## ---- STILT ----------------------------------------------------------------------
  output$p_st_bars <- renderPlot({
    s <- ST; s$leg <- paste(FLIGHT_LABEL(s$flight), "leg", s$leg_id)
    long <- do.call(rbind, lapply(c("og_basin", "ng_dist", "waste", "livestock", "other"), function(k) data.frame(leg = s$leg, cls = k, val = s[[k]], slope = s$slope)))
    long$cls <- factor(long$cls, c("other", "livestock", "waste", "ng_dist", "og_basin"))
    long$leg <- factor(long$leg, levels = s$leg[order(s$slope)])
    ggplot(long, aes(leg, val, fill = cls)) + geom_col(width = 0.75, colour = "white", linewidth = 0.4) + coord_flip() +
      scale_fill_manual(NULL, values = c(og_basin = COL$fossil, ng_dist = "#eda100", waste = COL$biogenic, livestock = "#1baf7a", other = COL$other),
                        labels = c(og_basin = "basin oil & gas", ng_dist = "gas distribution + post-meter", waste = "waste", livestock = "livestock", other = "other"),
                        breaks = c("og_basin", "ng_dist", "waste", "livestock", "other")) +
      guides(fill = guide_legend(nrow = 2)) +
      labs(x = NULL, y = "inventory-predicted ΔCH4 at the leg (ppb)", subtitle = "legs ordered by their observed ethane:methane slope (low at the bottom)") + theme_app(11)
  }, res = 96)
  output$p_st_scatter <- renderPlot({
    s <- ST
    ggplot(s, aes(100 * f_og_stilt, slope)) + geom_point(aes(shape = high_leg), colour = COL$accent, size = 3, alpha = 0.85) +
      scale_shape_manual("leg above 1 km AGL", values = c(`FALSE` = 16, `TRUE` = 17), labels = c("no", "yes")) +
      geom_hline(yintercept = 0.102, linetype = "22", colour = COL$fossil) +
      annotate("text", x = Inf, y = 0.102, label = "0.102", hjust = 1.1, vjust = -0.4, colour = COL$fossil, size = 3.2) +
      labs(x = "basin oil-and-gas share of the predicted enhancement (%)", y = "observed leg ΔC2H6/ΔCH4 (mol/mol)") + theme_app()
  }, res = 96)
  output$st_stats <- renderUI({
    if (is.null(STS)) return(NULL)
    tags$small(class = "text-muted", tags$ul(lapply(seq_len(nrow(STS)), function(i) tags$li(sprintf("%s: ρ = %.2f, p = %.2f, n = %d", sub("script-53 cone", "wind-cone", STS$test[i]), STS$rho[i], STS$p[i], STS$n[i])))))
  })
  output$t_st <- renderDT({
    s <- ST
    out <- data.frame(Flight = FLIGHT_LABEL(s$flight), Leg = s$leg_id, `AGL (m)` = s$agl_m, `Observed slope` = s$slope, r = s$r,
                      `Predicted ΔCH4 (ppb)` = round(s$total, 1), `Observed ΔCH4 (ppb)` = s$dch4_mean_ppb, `Basin O&G share` = round(s$f_og_stilt, 2),
                      `Predicted slope` = s$slope_pred, `HRRR vs aircraft wind (°)` = s$wd_diff_deg, check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(dom = "tp", pageLength = 25), class = "compact stripe")
  })
}

shinyApp(ui, server)
