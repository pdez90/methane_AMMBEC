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
EPA  <- rd("epa_cells.csv"); G20 <- rd("gra2pes_cells_v2.0beta.csv"); G20S <- rd("gra2pes_cells_v2.0beta_summary.csv")   # GRA2PES v1.1 methane is not used
HOT  <- rd("v2_pointsource_hotspots.csv"); CM <- rd("carbonmapper_denver_sources.csv")
BAS  <- rd("basin_ethane_methane.csv"); BASS <- rd("basin_summary.csv")
MED <- function(b) round(stats::median(ff_pct(T1$ethane_slope_within, b)))
N_CO <- if (!is.null(EM)) sum(is.finite(EM$CH4CO_GRA2PES_t_hr)) else NA
PV   <- if (have("paper_values.json")) jsonlite::fromJSON(file.path(DATA, "paper_values.json")) else list()
LIT  <- rd("literature_basin_ratios.csv")
DIU  <- rd("co_diurnal_sensitivity.csv")   # time-of-day CO anchor sensitivity (script 94)
BGG  <- rd("background_grid_sensitivity.csv"); BGS <- rd("background_grid_summary.csv")   # script 104
BGA  <- rd("background_aloft_sensitivity.csv"); BGM <- rd("background_mode_sensitivity.csv") # scripts 100, 85
HAVE_BG <- !is.null(BGG) && all(c("ch4_ppb", "c2h6_ppb") %in% names(US))
STP  <- rd("stilt_particles.csv.gz"); STC <- rd("stilt_centroids.csv"); STR <- rd("stilt_exposure_receptors.csv")   # script 107 / 64
HAVE_STP <- !is.null(STP) && !is.null(STC) && !is.null(STR) && !is.null(ST)
if (HAVE_STP) {
  ST_LEGS <- ST[order(ST$flight, ST$leg_id), ]
  ST_LEGS$key <- paste(ST_LEGS$flight, ST_LEGS$leg_id, sep = "|")
  ST_LEGS$label <- sprintf("%s, transect %d (basin O&G share %.0f%%)", FLIGHT_LABEL(ST_LEGS$flight), ST_LEGS$leg_id, 100 * ST_LEGS$f_og_stilt)
  ST_CHOICES <- setNames(ST_LEGS$key, ST_LEGS$label)
  STR$run_time <- as.POSIXct(STR$run_time, tz = "UTC")
}

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
             "The fossil fraction is the within-transect York slope divided by β, so β only rescales flights.")
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
          "); nothing is re-fitted in the browser except the division by the endmember you choose; the Background tab reads fits precomputed for each setting."),
        p("Fossil (thermogenic) methane co-emits ethane; biogenic methane does not. For each flight the paper fits the ",
          "ethane-to-methane slope of the urban enhancements and converts it to a fossil fraction by dividing by an ",
          "ethane endmember β. That endmember is the dominant systematic uncertainty, so the ", strong("Endmember"),
          " tab lets you set it, with the literature and campaign reference values as presets, and watch every flight move."),
        tags$ul(
          tags$li(strong("Background"), ": how the background definition (rolling window and percentile, a constant per flight, the air above the boundary layer, or the window run along the whole flight record) moves each flight's fossil fraction and the campaign median; every setting is a re-run of the paper's gate and fit."),
          tags$li(strong("Aircraft"), ": flight tracks, constant-altitude transects and the per-transect ethane:methane scatter behind each flight's slope."),
          tags$li(strong("Ground plumes"), ": the NOAA ARL car's plume ratios (ethane-bearing vs ethane-free) and CDPHE's facility surveys."),
          tags$li(strong("Inventories"), ": EPA gridded GHGI and GRA2PES v2.0beta cells next to the aircraft-sampled cells (descriptive; a cell-matched comparison does not account for transport)."),
          tags$li(strong("STILT"), ": HRRR-STILT footprints of the urban transects convolved with the EPA gridded GHGI and GRA2PES v2.0beta: predicted enhancements by source class and predicted against observed ethane:methane slopes; the particle clouds of any transect played backward in time, and how STILT was run."))),
      card(card_header("Headline numbers (paper, β = 0.102)"),
        layout_columns(col_widths = c(6, 6),
          div(div(class = "stat", paste0(PV$fossil_median, "%")), div(class = "stat-label", "campaign-median fossil fraction")),
          div(div(class = "stat", paste0(PV$fossil_min, "-", PV$fossil_max, "%")), div(class = "stat-label", paste0("range across ", nrow(T1), " flights"))),
          div(div(class = "stat", paste0(fmt(PV$gra_lo, 1), "-", fmt(PV$gra_hi, 1))), div(class = "stat-label", paste0("t CH4/h, CH4:CO x GRA2PES CO for the flight's day of the week (", N_CO, " flights)"))),
          div(div(class = "stat", if (!is.null(BRK)) sprintf("%.3f", BRK$beta_breakeven[1]) else "NA"), div(class = "stat-label", "β below which the median would exceed 50%"))),
        tags$small(class = "text-muted", sprintf("%d-%d%% across the regional reference endmembers 0.081-0.110; %d%% under an assumed 0.061.", MED(0.110), MED(0.0813), MED(0.061))))),
    card(card_header("Caveats"),
      tags$ul(class = "small-note",
        tags$li("The flights were designed for a basin mass balance, not a metropolitan one; the emission rates are enhancement-ratio estimates scaled by inventory CO and are less certain than the source mix."),
        tags$li(if (!is.null(DIU)) sprintf("The CO anchor is the 24-hour mean GRA2PES CO of the flight's day type (weekday or Saturday). Matching the hours of the flights instead would raise the emissions to %.1f-%.1f t/h (or %.1f-%.1f t/h including the 3 h before sampling), because GRA2PES CO peaks in the afternoon.",
                                           min(DIU$E_window_t_hr), max(DIU$E_window_t_hr), min(DIU$E_window3h_t_hr), max(DIU$E_window3h_t_hr)) else "The CO anchor is the 24-hour mean GRA2PES CO of the flight's day type."),
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
          tags$small(class = "text-muted", "Top-down: each screen-passing flight's CH4:CO emission split by its fossil fraction at β; the bar is the paired median across the screen-passing flights, and the error bar spans the flight-level totals (Figure 4, section 3.5). Inventories for comparison."))),
      card(card_header("Per-flight table"), DTOutput("t_flights")))),

  nav_panel("Background",
    layout_sidebar(sidebar = sidebar(width = 340,
        h6("Background definition", class = "mb-1"),
        selectInput("bg_kind", NULL, width = "100%",
                    choices = c("Rolling percentile over the joined urban transects (paper)" = "rolling",
                                "Constant: percentile of all the flight's urban-transect samples" = "constant",
                                "Constant: median of the air above the boundary layer" = "aloft",
                                "Constant: 5th percentile of the air above the boundary layer" = "aloft_p05",
                                "Rolling 5th percentile along the continuous flight record" = "track")),
        conditionalPanel("input.bg_kind == 'rolling' || input.bg_kind == 'constant'",
          selectInput("bg_window", "Window (samples; the aircraft records one per second)", choices = c(60, 120, 180, 300, 450, 600, 900, 1800), selected = 300),
          selectInput("bg_q", "Percentile treated as background (%)", choices = c(1, 2, 5, 10, 25), selected = 5)),
        sliderInput("beta_bg", "Ethane endmember β", 0.03, 0.20, 0.102, 0.0001, width = "100%"),
        selectInput("bg_flight", "Flight for the methane panel", choices = FLIGHTS, selected = "20240708_R0_L1"),
        tags$small(class = "text-muted",
          "The paper subtracts, from each species, the rolling 5th percentile within 300 samples (about 5 min, 20 km of flight) run over the flight's urban constant-altitude transects joined end to end, ",
          "then keeps samples with ΔCH4 above 20 ppb and fits one York slope with a separate intercept per transect. ",
          "Because of those intercepts, a background that only shifts a whole transect cannot change the slope; the background acts through which samples pass the gate and through along-transect structure longer than the window. ",
          "Every setting here reruns that gate and fit (scripts 85, 100 and 104); the constant window is the same percentile of all of the flight's urban-transect samples.")),
      layout_columns(col_widths = c(3, 3, 3, 3),
        value_box("Campaign median at this background", textOutput("bg_median"), theme = "primary"),
        value_box("Paper's setting (300 samples, 5th percentile)", textOutput("bg_paper")),
        value_box("Flights at or above 50%", textOutput("bg_majority")),
        value_box("Flights with a fit", textOutput("bg_n"))),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Campaign median over the window × percentile grid (at the chosen β)"), plotOutput("p_bg_grid", height = 380)),
        card(card_header("Each flight: paper's background → chosen background"), plotOutput("p_bg_flights", height = 380))),
      card(card_header("Methane along the joined urban transects, with the chosen background"), plotOutput("p_bg_series", height = 330),
           tags$small(class = "text-muted", "Grey: 1 Hz methane on the flight's urban transects in time order (vertical lines separate transects). Blue: the background. Orange points: samples above the 20 ppb gate that enter the fit. The along-track variant runs the same window over the whole flight record, so its background can sit below the cleanest air of the transect itself.")),
      card(card_header("Per-flight table at the chosen background"), DTOutput("t_bg")),
      accordion(open = FALSE, id = "bg_help",
        accordion_panel("How the background is computed, and what this tab recomputes",
          tags$div(class = "small-note", style = "font-size:0.9rem; line-height:1.45",
            h6("1. The paper's construction (scripts 15 and 21; section 3.2)"),
            p("For each flight, the 1 Hz samples of its urban constant-altitude transects are joined end to end in time order (basin transects for the basin ratio). ",
              "For each species X (CH4, C2H6, CO) the background at sample i is the 5th percentile of X over the 300 samples centered on i (about 5 min, 20 km of flight), ",
              "and the enhancement is ΔX = X − background. Near a transect boundary the window reaches into the neighboring transect, so the background is the cleanest air sampled on neighboring urban transects; ",
              "this keeps enhancements that extend over a whole transect (an urban-scale plume), which a window run along the whole flight would partly remove."),
            h6("2. The gate and the fit (section 3.3)"),
            p("Samples with ΔCH4 > 20 ppb enter the fit. One York slope of ΔC2H6 on ΔCH4 is fitted with a separate intercept for each transect (enhancements centered on each transect's mean; ",
              "transects with fewer than ten gated samples dropped), with the instrument precisions 1 ppb (CH4) and 0.2 ppb (C2H6). The fossil fraction is that slope divided by the endmember β, clamped to 0–100%. ",
              "Because each transect has its own intercept, a background that only shifts a whole transect up or down cannot change the slope: the background acts through which samples pass the gate and through structure along a transect on scales longer than the window."),
            h6("3. The five constructions offered here"),
            tags$ul(
              tags$li(strong("Rolling percentile over the joined urban transects"), ": the paper's construction with the window (60 to 1,800 samples) and percentile (1 to 25%) of your choice, applied to CH4, C2H6 and CO alike (script 104)."),
              tags$li(strong("Constant: percentile of all the flight's urban-transect samples"), ": one number per species and flight, the chosen percentile of every urban-transect sample, so no along-transect structure is removed (script 104, window = Inf)."),
              tags$li(strong("Constant: median of the air above the boundary layer"), ": one number per species and flight, the median of the constant-altitude transect samples at least 100 m above the airborne-lidar boundary-layer height at that sample and at least 1 km above ground (transects over the analysis box when they give at least 60 samples, otherwise anywhere on the flight). 12 July L1 has no such samples (script 100; SI S4)."),
              tags$li(strong("Constant: 5th percentile of the air above the boundary layer"), ": as above with the 5th percentile instead of the median (script 100)."),
              tags$li(strong("Rolling 5th percentile along the continuous flight record"), ": the paper's window and percentile run over the whole 1 Hz record of the flight (all transects, climbs and turns), after which the urban-transect samples are taken; this places the background within about 20 km of flight of every sample and removes more of the broad urban enhancement (script 85; SI S4).")),
            h6("4. What is precomputed and what is live"),
            p("Every slope, gated count and fossil fraction shown here was computed by the pipeline scripts above with the paper's gate and fit; the browser only divides the slope by β and summarizes. ",
              "The methane panel is the one live computation: it redraws the chosen background from the exported 1 Hz methane of the selected flight (rounded to 0.1 ppb) with the same centered-window quantile as the pipeline, and marks the samples above the gate; ",
              "its count matches the table's gated count for every flight and setting. The along-track background is read from the export because it needs the full flight record. ",
              "The CH4:CO emission slopes of the paper are also recomputed under each setting (scripts 85, 100, 104 write them) but are not shown here."),
            p("Outputs: background_grid_sensitivity.csv, background_grid_summary.csv (script 104), background_aloft_sensitivity.csv (script 100), background_mode_sensitivity.csv (script 85); ",
              "the paper reports the sweep in SI section S4 and Figure S8.")))))),

  nav_panel("Aircraft",
    layout_sidebar(sidebar = sidebar(width = 300,
        selectInput("flight", "Flight", choices = ALL_FLIGHTS, selected = "20240710_R0_L1"),
        checkboxInput("show_track", "Show the full 10-s track", TRUE),
        checkboxInput("show_basin", "Show basin transects too", FALSE),
        sliderInput("beta_air", "β for the per-transect fossil fractions", 0.03, 0.20, 0.102, 0.0001),
        tags$small(class = "text-muted", "Transects are constant-altitude, straight segments found from GPS altitude and ground track only; each is clipped by region, so an urban transect is its part inside the box (39.50-39.95°N, 105.20-104.55°W) and a basin transect its part north of 40.05°N. ",
                   "Enhancements use the paper's rolling 5th-percentile background over the flight's urban transects joined end to end. Points above the 20 ppb gate enter the fits.")),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Track and constant-altitude transects"), plotOutput("p_map", height = 520)),
        card(card_header("Urban transects: ethane vs methane enhancement (gated samples)"), plotOutput("p_scatter", height = 520))),
      card(card_header("Transects on this flight"), DTOutput("t_legs")))),

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
        selectInput("inv", "Inventory", choices = c("EPA gridded GHGI 2020 (0.1°)" = "epa", "GRA2PES v2.0beta (4 km, Jul 2023)" = "g20")),
        radioButtons("inv_metric", "Show", choices = c("fossil fraction" = "ff", "total CH4 (t/h per cell)" = "tot")),
        sliderInput("beta_inv", "β for the aircraft cells", 0.03, 0.20, 0.102, 0.0001),
        tags$small(class = "text-muted", "Aircraft cells are the 0.02° cells with at least 12 in-plume boundary-layer samples spanning 50 ppb, coloured by their local slope / β (Figure S11). Each aircraft sample integrates emissions carried from upwind, so a cell-by-cell match is descriptive, not a test of the inventory (the STILT tab accounts for transport). ",
                   "Fossil fraction is fossil / (fossil + biogenic); GRA2PES fossil = oil-and-gas + post-meter, biogenic = waste + agriculture.")),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Inventory cells with the aircraft cells overlaid"), plotOutput("p_inv_map", height = 600)),
        card(card_header("Cell by cell: aircraft vs inventory"), plotOutput("p_inv_scatter", height = 380), uiOutput("inv_stats"))),
      card(card_header("Box totals"), DTOutput("t_inv")))),

  nav_panel("STILT",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Predicted enhancement by source class, per urban transect"),
        radioButtons("st_inv", NULL, inline = TRUE, choices = c("EPA gridded GHGI" = "epa", "GRA2PES v2.0beta" = "v2")),
        plotOutput("p_st_bars", height = 440),
        tags$small(class = "text-muted", "HRRR-driven STILT footprints (10 h back, 500 particles from 4 receptors per transect) convolved with each inventory, by source class. Flights without archived HRRR meteorology for the full 10 h (5 and 12 July, and some 13 July receptors) are not included.")),
      card(card_header("Observed vs inventory-predicted ethane:methane slope"), plotOutput("p_st_scatter", height = 440),
        uiOutput("st_stats"))),
    card(card_header("Transect table"), DTOutput("t_st")),
    card(card_header("Where the sampled air came from: the STILT particles, played backward in time"),
      layout_sidebar(sidebar = sidebar(width = 320,
          uiOutput("st_leg_ui"),
          sliderInput("st_hours", "Hours before the aircraft sampled the air", min = 0, max = 10, value = 0, step = 0.25, width = "100%",
                      animate = animationOptions(interval = 400, loop = FALSE)),
          checkboxInput("st_trail", "Draw the centroid trail of each receptor's particle cloud", TRUE),
          radioButtons("st_zoom", "Map extent", choices = c("Denver and the basin" = "region", "Wherever the particles went" = "auto"), selected = "auto"),
          tags$small(class = "text-muted",
            "Press the play button under the slider to run the 10 h backward. Each receptor (black cross) released 500 particles; 25 of them per receptor are drawn, ",
            "coloured by whether the particle is in the lower half of the mixed layer at that moment and so adds to the surface footprint (orange) or is above it (grey). ",
            "The trail is the mean position of all 500 particles. The air the aircraft sampled came from where the orange particles have been.")),
        plotOutput("p_st_particles", height = 600), uiOutput("st_movies"))),
    accordion(open = FALSE, id = "st_help",
      accordion_panel("How STILT was run (script 64 in the code repository)",
        tags$div(class = "small-note", style = "font-size:0.9rem; line-height:1.45",
          tags$ol(
            tags$li(strong("Receptors."), " For each fitted urban transect of a Table 1 flight, four gated samples (ΔCH4 > 20 ppb) spread evenly along the gated part of the transect become receptors, each at the aircraft's position and height above ground at that sample, with the release time rounded to the minute: 112 receptors on 28 transects of 7 flights (5 July L1, 12 July L1 and one transect on each 13 July flight have no archived HRRR meteorology for the full 10 h)."),
            tags$li(strong("Meteorology."), " NOAA ARL's archive of HRRR analyses (3 km, hourly, in 6-h files) is downloaded for each flight and cut to 37.5-42.5°N, 108.5-101.5°W with HYSPLIT's xtrct_grid."),
            tags$li(strong("Model."), " HYSPLIT in STILT mode (the build distributed with STILT v2) with the STILT v2 defaults: 500 particles per receptor followed 10 h backward at a 1-min time step, Hanna turbulence, the mixed-layer depth from a modified Richardson-number profile with a 150 m minimum, and the surface sensitivity (footprint, ppm per μmol m⁻² s⁻¹) accumulated while a particle is in the lower half of the mixed layer. The CONTROL and SETUP.CFG files are written by the script for every receptor."),
            tags$li(strong("Post-processing."), " STILT v2's near-field plume-dilution correction is applied to the particle output; the footprints are summed over the 10 h."),
            tags$li(strong("Convolution."), " The footprint of each particle step is multiplied by the inventory flux of the cell it is in: the 0.1° EPA gridded GHGI (2020 Express Extension) by source class (basin-type oil and gas; distribution and post-meter; waste; livestock; other) and the GRA2PES v2.0beta July 2023 weekday methane (4-km cells on a 0.02° grid). Summing over particles and steps gives the inventory-predicted enhancement of each class at the receptor, the basin oil-and-gas share, and a predicted ethane:methane slope (0.102 for oil and gas, 0.110 for distribution and post-meter gas); receptors are averaged to the transect."),
            tags$li(strong("What is compared."), " The observed transect slope against the predicted basin share and the predicted slope (Spearman, upper cards), and the observed against the predicted enhancement as a transport check. The predicted enhancement has no background of its own, whereas the observed one is relative to the rolling background, so the observed-to-predicted ratios are not read as emission scalings.")),
          p("Outputs: stilt_exposure_receptors.csv, stilt_exposure_legs.csv, stilt_exposure_summary.csv (script 64); stilt_particles.csv.gz and stilt_centroids.csv for this animation (script 107); figures/stilt_particles_*.gif (script 108). Paper: SI section S9 and Figure S16."))))),

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
      labs(x = NULL, y = "fossil fraction (%)", subtitle = sprintf("β = %.4g; lines: 95%% transect-block bootstrap intervals", input$beta)) +
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
      if (!is.null(G20S)) data.frame(src = "GRA2PES\nv2.0beta", cls = c("fossil", "biogenic", "other"),
                                     val = c(G20S$og_t_hr + G20S$postmeter_t_hr, G20S$waste_t_hr + G20S$ag_t_hr, G20S$other_t_hr)))
    d <- rbind(td, inv); d$cls <- factor(d$cls, c("other", "biogenic", "fossil")); d$src <- factor(d$src, unique(d$src))
    tot <- aggregate(val ~ src, d, sum); fos <- aggregate(val ~ src, d[d$cls == "fossil", ], sum); fb <- aggregate(val ~ src, d[d$cls != "other", ], sum)
    lab <- merge(merge(tot, fos, by = "src", suffixes = c("", "_f")), fb, by = "src", suffixes = c("", "_fb"))
    lab$txt <- sprintf("%.1f t/h\n%.0f%% fossil", lab$val, 100 * lab$val_f / lab$val_fb)
    rng <- range(sa$E_total_t_hr); lab$val[lab$src == td$src[1]] <- max(lab$val[lab$src == td$src[1]], rng[2])
    ggplot(d, aes(src, val, fill = cls)) + geom_col(width = 0.7, colour = "white", linewidth = 0.6) +
      annotate("errorbar", x = 1, ymin = rng[1], ymax = rng[2], width = 0.15, colour = COL$ink) +
      geom_text(data = lab, aes(src, val, label = txt), inherit.aes = FALSE, vjust = -0.3, size = 3.2, colour = COL$ink) +
      scale_fill_manual(NULL, values = c(fossil = COL$fossil, biogenic = COL$biogenic, other = COL$other),
                        labels = c(fossil = "fossil (gas system)", biogenic = "biogenic (waste, agriculture)", other = "combustion / other"), breaks = c("fossil", "biogenic", "other")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + guides(fill = guide_legend(nrow = 3)) +
      labs(x = NULL, y = "urban CH4 emission (t/h)") + theme_app()
  }, res = 96)

  output$t_flights <- renderDT({
    f <- flights_at(); e <- EM[match(f$flight, EM$flight), ]
    out <- data.frame(Flight = f$label, `Urban transects` = f$legs, `Within-transect slope` = signif(f$slope, 3), r = round(f$r, 2),
                      `Fossil % at β` = round(f$ff), `95% interval` = ifelse(is.finite(f$lo), sprintf("%.0f-%.0f", f$lo, f$hi), "one qualifying transect"),
                      `CH4:CO x GRA2PES (t/h)` = e$CH4CO_GRA2PES_t_hr, `CH4:CO2 x Vulcan (t/h)` = e$CH4CO2_Vulcan_t_hr, check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(dom = "t", pageLength = 10), class = "compact stripe")
  })

  ## ---- background -----------------------------------------------------------------
  bg_paper_rows <- function() { if (is.null(BGG)) return(NULL)
    g <- BGG[BGG$window_samples == 300 & BGG$percentile == 5, ]; g[g$flight %in% FLIGHTS, c("flight", "slope", "n_gated", "n_legs")] }
  bg_rows <- reactive({
    validate(need(HAVE_BG, "Run scripts 85, 100, 104 and 86 to add the background sensitivity data."))
    k <- input$bg_kind
    r <- switch(k,
      rolling  = { g <- BGG[BGG$window_samples == as.numeric(input$bg_window) & BGG$percentile == as.numeric(input$bg_q), ]
                   data.frame(flight = g$flight, slope = g$slope, n_gated = g$n_gated, n_legs = g$n_legs) },
      constant = { g <- BGG[!is.finite(BGG$window_samples) & BGG$percentile == as.numeric(input$bg_q), ]
                   data.frame(flight = g$flight, slope = g$slope, n_gated = g$n_gated, n_legs = g$n_legs) },
      aloft    = { validate(need(!is.null(BGA), "background_aloft_sensitivity.csv is missing"))
                   data.frame(flight = BGA$flight, slope = BGA$slope_aloft, n_gated = BGA$n_gated_aloft, n_legs = BGA$n_legs_aloft) },
      aloft_p05 = { validate(need(!is.null(BGA) && "slope_aloft_p05" %in% names(BGA), "re-run script 100 for the 5th-percentile aloft slopes"))
                   data.frame(flight = BGA$flight, slope = BGA$slope_aloft_p05, n_gated = BGA$n_gated_aloft_p05, n_legs = NA) },
      track    = { validate(need(!is.null(BGM) && "slope_track" %in% names(BGM), "re-run script 85 for the along-track slopes"))
                   g <- BGM[BGM$flight %in% FLIGHTS, ]; data.frame(flight = g$flight, slope = g$slope_track, n_gated = g$n_gated_track, n_legs = NA) })
    r <- r[r$flight %in% FLIGHTS, ]; r <- r[match(FLIGHTS, r$flight), ]; r$flight <- FLIGHTS
    r$ff <- ff_pct(r$slope, input$beta_bg)
    p <- bg_paper_rows(); r$ff_paper <- ff_pct(p$slope[match(r$flight, p$flight)], input$beta_bg)
    r$n_gated_paper <- p$n_gated[match(r$flight, p$flight)]; r$label <- FLIGHT_LABEL(r$flight); r
  })
  output$bg_median <- renderText({ r <- bg_rows(); sprintf("%.0f%%", median(r$ff, na.rm = TRUE)) })
  output$bg_paper <- renderText({ r <- bg_rows(); sprintf("%.0f%%", median(r$ff_paper, na.rm = TRUE)) })
  output$bg_majority <- renderText({ r <- bg_rows(); sprintf("%d of %d", sum(r$ff >= 50, na.rm = TRUE), sum(is.finite(r$ff))) })
  output$bg_n <- renderText({ r <- bg_rows(); sprintf("%d of %d", sum(is.finite(r$ff)), nrow(r)) })

  output$p_bg_grid <- renderPlot({
    validate(need(HAVE_BG, ""))
    g <- BGG[BGG$flight %in% FLIGHTS, ]; g$ff <- ff_pct(g$slope, input$beta_bg)
    m <- aggregate(ff ~ window_samples + percentile, g, function(v) median(v, na.rm = TRUE), na.action = na.pass)
    m$w <- factor(ifelse(is.finite(m$window_samples), m$window_samples, "const"), levels = c(60, 120, 180, 300, 450, 600, 900, 1800, "const"))
    m$q <- factor(m$percentile, levels = c(1, 2, 5, 10, 25))
    sel <- if (input$bg_kind == "rolling") data.frame(w = factor(input$bg_window, levels = levels(m$w)), q = factor(input$bg_q, levels = levels(m$q)))
           else if (input$bg_kind == "constant") data.frame(w = factor("const", levels = levels(m$w)), q = factor(input$bg_q, levels = levels(m$q))) else NULL
    pp <- ggplot(m, aes(w, q, fill = ff)) + geom_tile(colour = "white") +
      geom_text(aes(label = round(ff)), size = 3.6, colour = COL$ink) +
      geom_tile(data = data.frame(w = factor(300, levels = levels(m$w)), q = factor(5, levels = levels(m$q))), aes(w, q), fill = NA, colour = COL$ink, linewidth = 1.1, inherit.aes = FALSE) +
      scale_fill_gradient(low = "#fff3e0", high = COL$fossil, limits = c(0, 50), oob = scales::squish, name = "median fossil %") +
      labs(x = "window (samples; const = whole flight)", y = "percentile (%)", subtitle = "black box: the paper's setting; blue box: the chosen one") + theme_app(12)
    if (!is.null(sel)) pp <- pp + geom_tile(data = sel, aes(w, q), fill = NA, colour = COL$accent, linewidth = 1.1, inherit.aes = FALSE)
    pp
  }, res = 96)

  output$p_bg_flights <- renderPlot({
    r <- bg_rows(); r <- r[order(r$ff_paper), ]; r$label <- factor(r$label, levels = r$label)
    ggplot(r) + geom_hline(yintercept = 50, linetype = "33", colour = COL$ink2) +
      geom_segment(aes(x = label, xend = label, y = ff_paper, yend = ff), colour = COL$ink2, linewidth = 0.6,
                   arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
      geom_point(aes(label, ff_paper), colour = COL$ink2, size = 2.6) +
      geom_point(aes(label, ff), colour = COL$accent, size = 3) +
      geom_text(aes(label, pmax(ff, ff_paper, na.rm = TRUE), label = ifelse(is.finite(ff), sprintf("n %d", n_gated), "no fit")), vjust = -0.8, size = 2.9, colour = COL$ink2) +
      scale_y_continuous(limits = c(0, 105)) +
      labs(x = NULL, y = "fossil fraction (%)", subtitle = "grey: paper's background; blue: chosen; n = samples above the 20 ppb gate") + theme_app(12) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }, res = 96)

  output$p_bg_series <- renderPlot({
    validate(need(HAVE_BG, ""))
    fl <- input$bg_flight; u <- US[US$flight == fl, ]; u$i <- seq_len(nrow(u))
    k <- input$bg_kind
    bg <- switch(k,
      rolling  = rolling_baseline(u$ch4_ppb, as.numeric(input$bg_window), as.numeric(input$bg_q) / 100),
      constant = rolling_baseline(u$ch4_ppb, Inf, as.numeric(input$bg_q) / 100),
      aloft    = rep(BGA$bg_ch4_aloft[BGA$flight == fl], nrow(u)),
      aloft_p05 = rep(if ("bg_ch4_aloft_p05" %in% names(BGA)) BGA$bg_ch4_aloft_p05[BGA$flight == fl] else NA, nrow(u)),
      track    = if ("bg_ch4_track" %in% names(u)) u$bg_ch4_track else rep(NA_real_, nrow(u)))
    if (!length(bg) || all(!is.finite(bg))) bg <- rep(NA_real_, nrow(u))
    u$bg <- bg; u$gated_now <- is.finite(bg) & (u$ch4_ppb - bg) > 20
    cuts <- u$i[c(FALSE, diff(u$leg_id) != 0)] - 0.5
    pp <- ggplot(u, aes(i, ch4_ppb)) + geom_vline(xintercept = cuts, colour = COL$grid, linewidth = 0.5) +
      geom_line(colour = COL$other, linewidth = 0.4) +
      geom_point(data = u[u$gated_now, ], colour = COL$fossil, size = 0.9, alpha = 0.7) +
      geom_line(aes(y = bg), colour = COL$accent, linewidth = 0.9, na.rm = TRUE) +
      labs(x = "sample along the joined urban transects", y = "CH4 (ppb)",
           subtitle = sprintf("%s: %d samples above the gate with this background (paper: %d)", FLIGHT_LABEL(fl), sum(u$gated_now, na.rm = TRUE), sum(u$gated, na.rm = TRUE))) + theme_app(12)
    if (k == "track" && all(!is.finite(bg))) pp <- pp + annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5, label = "re-run script 86 for the along-track background", colour = COL$ink2)
    pp
  }, res = 96)

  output$t_bg <- renderDT({
    r <- bg_rows()
    out <- data.frame(Flight = r$label, `Within-transect slope` = signif(r$slope, 3), `Fossil % at β` = round(r$ff),
                      `Fossil %, paper's background` = round(r$ff_paper), `Gated samples` = r$n_gated, `Gated samples, paper` = r$n_gated_paper,
                      `Transects in fit` = r$n_legs, check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(dom = "t", pageLength = 12), class = "compact stripe")
  })

  ## ---- aircraft -------------------------------------------------------------------
  output$p_map <- renderPlot({
    fl <- input$flight; tr <- TR[TR$flight == fl, ]; lg <- LEGS[LEGS$flight == fl, ]
    us <- US[US$flight == fl, ]; bs <- if (input$show_basin) BSM[BSM$flight == fl, ] else US[0, ]
    g <- ggplot() + basemap_layer("region") + geo_layers()
    if (input$show_track) g <- g + geom_path(data = tr, aes(lon, lat), colour = "#8a8985", linewidth = 0.5)
    tag <- function(d, r) { d$region <- rep(r, nrow(d)); d }   # works for zero-row frames (flights with no urban transect)
    pts <- rbind(tag(us, "urban"), tag(bs, "basin"))
    if (nrow(pts)) g <- g + geom_point(data = pts[order(pts$dch4_ppb), ], aes(lon, lat, colour = pmin(dch4_ppb, 150)), size = 1.1) +
      scale_colour_gradient("ΔCH4 (ppb, transect samples)", low = "#dbe9f8", high = "#0b3d7a", limits = c(0, 150))
    if (nrow(lg)) g <- g + geom_text(data = lg[lg$region %in% c("urban", if (input$show_basin) "basin"), ], aes(lon, lat, label = leg_id),
                                     size = 3, fontface = "bold", colour = COL$ink)
    xl <- range(c(tr$lon, BOX$lon_w, BOX$lon_e), na.rm = TRUE) + c(-0.05, 0.05); yl <- range(c(tr$lat, BOX$lat_s, BOX$lat_n), na.rm = TRUE) + c(-0.03, 0.03)
    g + coord_denver(xl, yl) + labs(x = NULL, y = NULL, subtitle = sprintf("%s: %d urban and %d basin transect segments (numbers: ids)", FLIGHT_LABEL(fl), sum(lg$region == "urban"), sum(lg$region == "basin")), caption = basemap_credit("region")) +
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
      facet_wrap(~ leg, labeller = labeller(leg = function(x) paste("transect", x))) +
      labs(x = "ΔCH4 (ppb)", y = "ΔC2H6 (ppb)",
           subtitle = sprintf("black: OLS per transect; dashed: pure fossil at β = %.4g; flight York slope %s", b, if (length(sl)) sprintf("%.4f", sl) else "n/a")) +
      theme_app(12)
  }, res = 96)

  output$t_legs <- renderDT({
    lg <- LEGS[LEGS$flight == input$flight & LEGS$region %in% c("urban", "basin"), ]
    out <- data.frame(Transect = lg$leg_id, Region = lg$region, Samples = lg$n, `Length (km)` = lg$length_km, `AGL (m)` = lg$agl_m,
                      `Track (°)` = lg$track_deg, `Mean ΔCH4 (ppb)` = lg$ch4_enh_mean_ppb, `Gated samples` = lg$n_gated,
                      `Transect slope` = lg$leg_slope, r = lg$leg_r, `Fossil % at β` = round(ff_pct(lg$leg_slope, input$beta_air)), check.names = FALSE)
    datatable(out[order(out$Region != "urban", out$Transect), ], rownames = FALSE, options = list(dom = "tp", pageLength = 12), class = "compact stripe")
  })

  ## ---- ground ---------------------------------------------------------------------
  output$p_arc_map <- renderPlot({
    a <- ARC[is.finite(ARC$lat) & ARC$lat > 39.3 & ARC$lat < 40.8, ]
    a$class <- factor(a$class, c("fossil-type", "ethane-free", "unclassified"))
    ggplot() + basemap_layer("region") + geo_layers() + geom_hline(yintercept = BASIN_LAT, linetype = "22", colour = COL$ink2, linewidth = 0.4) +
      geom_point(data = a[order(a$class, decreasing = TRUE), ], aes(lon, lat, colour = class, size = ch4_enh_max_ppb), alpha = 0.8) +
      scale_colour_manual("plume class", values = c(`fossil-type` = COL$fossil, `ethane-free` = COL$biogenic, unclassified = COL$other),
                          labels = c("ethane-bearing (fossil-type)", "ethane-free", "unclassified")) +
      scale_size_area(max_size = 7, guide = "none") + coord_denver(c(-105.35, -104.3), c(39.45, 40.75)) +
      guides(colour = guide_legend(nrow = 3, override.aes = list(size = 4))) +
      labs(x = NULL, y = NULL, subtitle = "symbol size: peak ΔCH4; dashed: 40.05°N", caption = basemap_credit("region")) + theme_app() + theme(panel.grid = element_blank())
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
    ggplot(h, aes(lon, lat, colour = pmin(enh_p95_ppmv, 1))) + basemap_layer(paste0("site_", input$site)) + geom_point(alpha = 0.8, size = 1.6) +
      scale_colour_gradient("95th-pct ΔCH4 (ppmv, ≤1)", low = "#dbe9f8", high = "#0b3d7a") +
      geom_point(data = FACL, aes(lon, lat), shape = 3, size = 3, stroke = 1.1, colour = COL$ink, inherit.aes = FALSE) +
      geom_text(data = FACL, aes(lon, lat, label = short), size = 3, colour = COL$ink, hjust = -0.15, vjust = -0.4, fontface = "bold", inherit.aes = FALSE) +
      coord_quickmap(xlim = range(h$lon) + c(-0.005, 0.005), ylim = range(h$lat) + c(-0.004, 0.004)) +
      labs(x = NULL, y = NULL, subtitle = sprintf("%s plume samples in %s bins; + = facility", fmt(sum(h$n)), fmt(nrow(h))), caption = basemap_credit(paste0("site_", input$site))) +
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
    inv <- switch(input$inv, epa = s$epa_ff, g20 = s$gra_v20beta_ff)
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
    for (gs in list(G20S)) if (!is.null(gs)) rows[[length(rows) + 1]] <- data.frame(
      Inventory = paste("GRA2PES", gs$inventory_version, "(Jul 2023 weekday)"), `Total CH4 (t/h)` = round(gs$ch4_total_t_hr, 2),
      `Fossil (t/h)` = round(gs$og_t_hr + gs$postmeter_t_hr, 2), `Biogenic (t/h)` = round(gs$waste_t_hr + gs$ag_t_hr, 2),
      `Fossil fraction` = round(gs$fossil_frac_box, 2), check.names = FALSE)
    datatable(do.call(rbind, rows), rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  ## ---- STILT ----------------------------------------------------------------------
  output$p_st_bars <- renderPlot({
    s <- ST; validate(need(!is.null(s) && nrow(s) > 0, "no STILT results"))
    s$leg <- paste(FLIGHT_LABEL(s$flight), "transect", s$leg_id)
    if (input$st_inv == "v2") {
      validate(need("v2_total" %in% names(s), "GRA2PES v2.0beta convolution not in the exported data"))
      ks <- c(v2_og = "oil & gas", v2_postmeter = "post-meter", v2_waste = "waste", v2_ag = "agriculture", v2_other = "other")
    } else ks <- c(og_basin = "basin oil & gas", ng_dist = "gas distribution + post-meter", waste = "waste", livestock = "livestock", other = "other")
    long <- do.call(rbind, lapply(names(ks), function(k) data.frame(leg = s$leg, cls = ks[[k]], val = s[[k]], slope = s$slope)))
    long$cls <- factor(long$cls, rev(unname(ks)))
    long$leg <- factor(long$leg, levels = s$leg[order(s$slope)])
    pal <- setNames(c(COL$fossil, "#eda100", COL$biogenic, "#1baf7a", COL$other), unname(ks))
    ggplot(long, aes(leg, val, fill = cls)) + geom_col(width = 0.75, colour = "white", linewidth = 0.4) + coord_flip() +
      scale_fill_manual(NULL, values = pal, breaks = unname(ks)) + guides(fill = guide_legend(nrow = 2)) +
      labs(x = NULL, y = "inventory-predicted ΔCH4 at the transect (ppb)", subtitle = "transects ordered by their observed ethane:methane slope (low at the bottom)") + theme_app(11)
  }, res = 96)
  output$p_st_scatter <- renderPlot({
    s <- ST; validate(need(!is.null(s) && nrow(s) > 0, "no STILT results"))
    d <- data.frame(obs = s$slope, pred = s$slope_pred, inv = "EPA gridded GHGI", high = s$high_leg)
    if ("slope_pred_v2" %in% names(s)) d <- rbind(d, data.frame(obs = s$slope, pred = s$slope_pred_v2, inv = "GRA2PES v2.0beta", high = s$high_leg))
    lim <- c(-0.05, 0.15)                     # as in Figure S15d: weakly correlated transects lie far off scale
    d$off <- d$obs < lim[1] | d$obs > lim[2]; d$obs <- pmin(pmax(d$obs, lim[1]), lim[2])
    ggplot(d, aes(pred, obs, colour = inv, shape = high)) + geom_abline(linetype = "22", colour = COL$ink2) +
      geom_hline(yintercept = median(s$slope, na.rm = TRUE), linetype = "13", colour = COL$ink2) +
      geom_point(size = 3, alpha = 0.85) + coord_equal(xlim = lim, ylim = lim, expand = TRUE) +
      labs(subtitle = sprintf("dotted: observed median; %d off-scale slopes at the edge", sum(d$off[d$inv == "EPA gridded GHGI"]))) +
      scale_colour_manual(NULL, values = c(`EPA gridded GHGI` = COL$fossil, `GRA2PES v2.0beta` = "#eda100")) +
      scale_shape_manual("transect above 1 km AGL", values = c(`FALSE` = 16, `TRUE` = 17), labels = c("no", "yes")) +
      labs(x = "inventory-predicted ΔC2H6/ΔCH4 (STILT)", y = "observed transect ΔC2H6/ΔCH4 (mol/mol)") + theme_app()
  }, res = 96)
  output$st_stats <- renderUI({
    if (is.null(STS)) return(NULL)
    tags$small(class = "text-muted", tags$ul(lapply(seq_len(nrow(STS)), function(i) tags$li(sprintf("%s: ρ = %.2f, p = %.2f, n = %d", sub("legs <", "transects <", sub("script-53 cone", "wind-cone", STS$test[i])), STS$rho[i], STS$p[i], STS$n[i])))))
  })
  output$st_leg_ui <- renderUI({
    if (!HAVE_STP) return(tags$small("Run scripts 64, 107 and 86 to add the particle data."))
    selectInput("st_leg", "Transect", choices = ST_CHOICES, selected = ST_CHOICES[which.max(ST_LEGS$dC_djb_core / ST_LEGS$total)], width = "100%")   # default: largest share from the basin north of 40.05°N
  })
  output$st_movies <- renderUI({
    f <- list.files("www", pattern = "^stilt_particles_.*\\.(gif|mp4)$")
    if (!length(f)) return(NULL)
    lab <- sub("^stilt_particles_(\\d{8})_R0_?(L\\d)?_leg(\\d+)\\.(gif|mp4)$", "\\1 \\2 transect \\3 (\\4)", f)
    lab <- sub("^(\\d{4})(\\d{2})(\\d{2}) ", "\\3 Jul ", lab)
    tags$small(class = "text-muted", "Download the same animation as a movie (script 108): ",
               lapply(seq_along(f), function(i) tagList(a(lab[i], href = f[i], target = "_blank"), if (i < length(f)) " · ")))
  })
  st_sel <- reactive({ req(HAVE_STP, input$st_leg); k <- strsplit(input$st_leg, "|", fixed = TRUE)[[1]]; list(flight = k[1], leg = as.integer(k[2])) })
  output$p_st_particles <- renderPlot({
    s <- st_sel(); mb <- if (input$st_hours <= 0) 1 else round(input$st_hours * 60 / 15) * 15
    pp <- STP[STP$flight == s$flight & STP$leg_id == s$leg, ]; cc <- STC[STC$flight == s$flight & STC$leg_id == s$leg, ]
    rr <- STR[STR$flight == s$flight & STR$leg_id == s$leg, ]
    validate(need(nrow(pp) > 0, "no particles for this transect"))
    now <- pp[pp$min_back == mb, ]; trail <- cc[cc$min_back <= mb, ]
    if (input$st_zoom == "auto") { xl <- quantile(pp$lon, c(0.02, 0.98)) + c(-0.1, 0.1); yl <- quantile(pp$lat, c(0.02, 0.98)) + c(-0.08, 0.08)
      xl <- c(min(xl[1], -105.35), max(xl[2], -104.3)); yl <- c(min(yl[1], 39.45), max(yl[2], 40.75)) } else { xl <- c(-105.35, -104.3); yl <- c(39.45, 40.75) }
    t_air <- rr$run_time[1] - mb * 60
    g <- ggplot() + basemap_layer("region") + geo_layers(facilities = FALSE) +
      geom_hline(yintercept = BASIN_LAT, linetype = "22", colour = COL$ink2, linewidth = 0.4)
    if (input$st_trail && mb > 1) g <- g + geom_path(data = trail[order(trail$rec, trail$min_back), ], aes(lon, lat, group = rec), colour = COL$ink2, linewidth = 0.5, alpha = 0.8)
    rlab <- data.frame(lon = mean(rr$long), lat = mean(rr$lati), txt = sprintf("receptors: %d aircraft samples\non transect %d, %s UTC", nrow(rr), s$leg, format(rr$run_time[1], "%H:%M")))
    g <- g + geom_point(data = now[order(now$in_foot), ], aes(lon, lat, colour = in_foot), size = 1.6, alpha = 0.75) +
      geom_point(data = rr, aes(long, lati), shape = 3, size = 3.5, stroke = 1.2, colour = COL$ink) +
      geom_label(data = rlab, aes(lon, lat, label = txt), nudge_x = 0.04 * diff(xl), nudge_y = -0.05 * diff(yl), hjust = 0, size = 3.1,
                 colour = COL$ink, fill = "white", alpha = 0.85, label.size = 0.2, lineheight = 0.9) +
      scale_colour_manual(NULL, values = c(`TRUE` = COL$fossil, `FALSE` = COL$other), breaks = c(TRUE, FALSE),
                          labels = c("in the lower half of the mixed layer (adds to the footprint)", "above it"), drop = FALSE) +
      coord_denver(xl, yl) + guides(colour = guide_legend(override.aes = list(size = 3))) +
      labs(x = NULL, y = NULL, caption = basemap_credit("region"),
           subtitle = sprintf("%s, transect %d: %.2g h back (%s UTC); %d%% of the drawn particles in the footprint layer",
                              FLIGHT_LABEL(s$flight), s$leg, mb / 60, format(t_air, "%d %b %H:%M"), round(100 * mean(now$in_foot)))) +
      theme_app() + theme(panel.grid = element_blank())
    g
  }, res = 96)

  output$t_st <- renderDT({
    s <- ST
    out <- data.frame(Flight = FLIGHT_LABEL(s$flight), Transect = s$leg_id, `AGL (m)` = s$agl_m, `Observed slope` = s$slope, r = s$r,
                      `Observed ΔCH4 (ppb)` = s$dch4_mean_ppb, `Predicted ΔCH4, EPA (ppb)` = round(s$total, 1),
                      `Predicted ΔCH4, v2.0beta (ppb)` = if ("v2_total" %in% names(s)) round(s$v2_total, 1) else NA,
                      `Basin O&G share (EPA)` = round(s$f_og_stilt, 2), `Predicted slope, EPA` = s$slope_pred,
                      `Predicted slope, v2.0beta` = if ("slope_pred_v2" %in% names(s)) s$slope_pred_v2 else NA,
                      `HRRR vs aircraft wind (°)` = s$wd_diff_deg, check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(dom = "tp", pageLength = 25), class = "compact stripe")
  })
}

shinyApp(ui, server)
