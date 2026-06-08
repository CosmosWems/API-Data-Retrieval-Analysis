# ================================================================
#  ui.R  –  OpenAQ Explorer & OpenAir Analysis Dashboard
#
#  Note: sec_hdr / scroll_box / status_html are defined in
#  R/ui_helpers.R which is sourced by global.R.
# ================================================================

# ================================================================
#  TAB 1  LOCATION EXPLORER
# ================================================================
tab_explorer <- tabItem("explorer",
  sec_hdr("map", "Location Explorer"),

  fluidRow(
    box(title = "\U0001f50d Search", status = "primary",
        solidHeader = TRUE, width = 4,
        tabsetPanel(id = "search_mode",

          tabPanel("City / Name", br(),
            textInput("sn_city", "City or station name:",
                      placeholder = "Accra, Delhi, London \u2026"),
            numericInput("sn_limit", "Max results", 30, 5, 200, 5),
            uiOutput("sn_param_ui"),
            actionButton("btn_search_name", "\U0001f50d  Search",
                         class = "btn-primary btn-block")
          ),

          tabPanel("Coordinates", br(),
            fluidRow(
              column(6, numericInput("sc_lat", "Latitude",   5.6037, step = .001)),
              column(6, numericInput("sc_lon", "Longitude", -0.1870, step = .001))
            ),
            div(class = "coord-pill",
                icon("crosshairs"), uiOutput("coord_display_ui")),
            br(),
            sliderInput("sc_radius", "Radius (km):", 1, 25, 10),
            helpText("\U0001f4a1 Click the map to update lat/lon."),
            helpText(icon("info-circle"), " Max radius = 25 km (API limit)"),
            actionButton("btn_search_coord", "\U0001f50d  Search Nearby",
                         class = "btn-primary btn-block")
          ),

          tabPanel("Bounding Box", br(),
            helpText("Draw rectangle on map, or type manually:"),
            fluidRow(
              column(6, numericInput("bb_minlon", "Min Lon (W)", -2,  step = .5)),
              column(6, numericInput("bb_maxlon", "Max Lon (E)",  2,  step = .5))
            ),
            fluidRow(
              column(6, numericInput("bb_minlat", "Min Lat (S)",  4,  step = .5)),
              column(6, numericInput("bb_maxlat", "Max Lat (N)", 12,  step = .5))
            ),
            actionButton("btn_search_bbox", "\U0001f50d  Search Box",
                         class = "btn-primary btn-block")
          ),

          tabPanel("Country", br(),
            uiOutput("country_select_ui"),
            actionButton("btn_search_country", "\U0001f50d  Search Country",
                         class = "btn-primary btn-block")
          )
        )
    ),

    box(title = "\u2699\ufe0f Map Options & Parameter Reference",
        status = "info", solidHeader = TRUE, width = 4, collapsible = TRUE,
        selectInput("map_tile", "Map style:", MAP_TILES,
                    selected = "CartoDB.Positron"),
        checkboxInput("map_cluster", "Cluster markers", TRUE),
        hr(),
        tags$b("Parameter Reference:"), br(), br(),
        div(style = "max-height:220px;overflow-y:auto;",
          tags$table(
            class = "table table-condensed table-hover",
            style = "font-size:12px;",
            tags$thead(tags$tr(
              tags$th("ID"), tags$th("Parameter"), tags$th("Units"))),
            tags$tbody(lapply(seq_len(nrow(OPENAQ_PARAMS)), function(i)
              tags$tr(
                tags$td(tags$span(class = "param-id", OPENAQ_PARAMS$id[i])),
                tags$td(OPENAQ_PARAMS$label[i]),
                tags$td(tags$code(OPENAQ_PARAMS$units[i])))))
          )
        )
    ),

    box(title = "\U0001f4a1 Quick Start", status = "warning",
        solidHeader = TRUE, width = 4, collapsible = TRUE,
        tags$ol(style = "font-size:13px;line-height:2.4;padding-left:16px;",
          tags$li("Get a free API key at ",
            tags$a("explore.openaq.org.",
                   href = "https://explore.openaq.org", target = "_blank"),
				   tags$b("Exceeding API rate limits repeatedly can lead to a temporary or permanent ban from the OpenAQ API."),
						tags$a("\u2192 Read more here.",
                   href = "https://docs.openaq.org/using-the-api/rate-limits#rate-limit-headers", target = "_blank"), 
				   "To maintain uninterrupted access, plan your API requests carefully."),
          tags$li("Paste key in sidebar \u2192 click ", tags$b("Connect")),
          tags$li("Search a city or click the map"),
          tags$li("Click a map marker or location card to select a station"),
          tags$li("Go to ", tags$b("Fetch Data"),
                  " \u2192 tick sensors + date range \u2192 Fetch"),
          tags$li("Go to ", tags$b("Analysis"),
                  " \u2192 plot type \u2192 Generate"),
          tags$li("Or upload your own CSV in the Analysis tab")
        ),
        hr(),
        tags$small(icon("info-circle"), " API docs: ",
          tags$a("docs.openaq.org",
                 href = "https://docs.openaq.org", target = "_blank"))
    )
  ),

  fluidRow(
    box(title = "\U0001f5fa Map \u2014 click to set coordinates \u00b7 click markers to select",
        status = "primary", solidHeader = TRUE, width = 8,
        div(style = "position:relative;",
          leafletOutput("main_map", height = "500px"),
          absolutePanel(top = 10, right = 10, width = 150,
            div(style = "background:rgba(255,255,255,.93);border-radius:8px;
                         padding:8px 10px;font-size:11px;",
              tags$b("Legend"),
              tags$p(style = "margin:4px 0;",
                tags$span(style = "display:inline-block;width:11px;height:11px;
                  background:#2471a3;border-radius:50%;vertical-align:middle;"),
                " Stations"),
              tags$p(style = "margin:4px 0;",
                tags$span(style = "display:inline-block;width:11px;height:11px;
                  background:#27ae60;border-radius:50%;vertical-align:middle;"),
                " Selected"),
              tags$p(style = "margin:4px 0;",
                tags$span(style = "display:inline-block;width:11px;height:11px;
                  background:#e74c3c;border-radius:50%;vertical-align:middle;"),
                " Clicked point")
            )
          )
        )
    ),
    box(title = "\U0001f4cd Found Locations",
        status = "success", solidHeader = TRUE, width = 4,
        scroll_box(uiOutput("loc_cards"), height = "510px"))
  ),

  fluidRow(
    box(title = "\U0001f4cb Locations Table",
        status = "primary", solidHeader = TRUE, width = 12,
        collapsible = TRUE, collapsed = TRUE,
        DTOutput("loc_table"))
  )
)

# ================================================================
#  TAB 2  FETCH DATA
# ================================================================
tab_fetch <- tabItem("fetch",
  sec_hdr("download", "Fetch Sensor Data"),

  fluidRow(
    box(title = "\U0001f4cd Selected Location",
        status = "info", solidHeader = TRUE, width = 4,
        uiOutput("sel_loc_ui"), hr(), uiOutput("latest_ui")),

    box(title = "\U0001f4e1 Sensors \u2014 tick to fetch",
        status = "primary", solidHeader = TRUE, width = 4,
        uiOutput("sensors_chk_ui"), hr(), uiOutput("sensor_info_ui")),

    box(title = "\u2699\ufe0f Fetch Controls",
        status = "warning", solidHeader = TRUE, width = 4,
        selectInput("fetch_agg", "Temporal aggregation:",
          choices = c("Raw measurements" = "raw",
                      "Hourly averages"  = "hourly",
                      "Daily averages"   = "daily"),
          selected = "hourly"),
        dateRangeInput("date_range", "Date Range:",
                       start = Sys.Date() - 30, end = Sys.Date(),
                       max   = Sys.Date()),
        numericInput("fetch_limit", "Max records per sensor **DO NOT exceed API rate limits:",
                     1000, 100, 10000, 500),
        hr(),
        actionButton("btn_fetch", "\U0001f680  Fetch Data",
                     class = "btn-success btn-lg btn-block",
                     icon  = icon("download")),
        br(), uiOutput("fetch_status_ui"))
  ),

  fluidRow(
    box(title = "\U0001f4ca Summary & Completeness",
        status = "success", solidHeader = TRUE, width = 4,
        uiOutput("fetch_summary_ui")),
    box(title = "\U0001f4c9 Data Completeness",
        status = "primary", solidHeader = TRUE, width = 8,
        uiOutput("completeness_ui"))
  ),

  fluidRow(
    box(title = "\U0001f4cb Data Preview (openair-ready wide format)",
        status = "primary", solidHeader = TRUE, width = 12,
        DTOutput("data_preview_tbl"),
        hr(),
        fluidRow(
          column(3, downloadButton("dl_csv",  "  CSV",
                 icon = icon("file-csv"),     class = "btn-default btn-block")),
          column(3, downloadButton("dl_rds",  "  RDS",
                 icon = icon("r-project"),    class = "btn-default btn-block")),
          column(3, downloadButton("dl_xlsx", "  Excel",
                 icon = icon("file-alt"),     class = "btn-default btn-block")),
          column(3, downloadButton("dl_zip",  "  ZIP",
                 icon = icon("file-archive"), class = "btn-default btn-block"))
        )
    )
  )
)

# ================================================================
#  TAB 3  ANALYSIS
# ================================================================
tab_analysis <- tabItem("analysis",
  sec_hdr("bar-chart", "OpenAir Analysis Dashboard"),

  fluidRow(
    box(title = "\U0001f5c4 Data Source", status = "info",
        solidHeader = TRUE, width = 12, collapsible = TRUE,
        fluidRow(
          column(3,
            radioButtons("data_src", "Source:",
              choices = c("Use fetched OpenAQ data" = "fetched",
                          "Upload CSV file"         = "upload"))
          ),
          column(4,
            conditionalPanel("input.data_src=='upload'",
              fileInput("upload_csv", "Upload CSV:",
                        accept = c(".csv",".txt",".tsv")),
              uiOutput("map_date_ui"),
              uiOutput("map_ws_ui"),
              uiOutput("map_wd_ui"),
              selectInput("date_fmt", "Date format:",
                choices = c("Auto-detect"      = "auto",
                            "YYYY-MM-DD HH:MM" = "ymd HM",
                            "DD/MM/YYYY HH:MM" = "dmy HM",
                            "MM/DD/YYYY HH:MM" = "mdy HM",
                            "YYYY-MM-DD"       = "ymd",
                            "DD/MM/YYYY"       = "dmy"))
            )
          ),
          column(3,
            uiOutput("analysis_src_status_ui"), br(),
            actionButton("btn_prepare", "\u2699\ufe0f  Prepare Data",
                         class = "btn-primary btn-block",
                         icon  = icon("cogs")),
            br(), uiOutput("data_info_ui")
          )
        )
    )
  ),

  fluidRow(
    box(title = "\U0001f39b Plot Controls",
        status = "warning", solidHeader = TRUE, width = 4,
        selectInput("plot_type", "Plot type:", choices = plot_choices()),
        uiOutput("plot_desc_ui"),
        hr(),
        uiOutput("dyn_controls_ui"),
        hr(),
        actionButton("btn_plot", "\u25b6  Generate Plot",
                     class = "btn-success btn-lg btn-block",
                     icon  = icon("chart-bar")),
        br(),
        div(style = "display:flex;gap:5px;",
          downloadButton("dl_png", "PNG",
                         class = "btn-default btn-sm", style = "flex:1;"),
          downloadButton("dl_pdf", "PDF",
                         class = "btn-default btn-sm", style = "flex:1;"),
          downloadButton("dl_svg", "SVG",
                         class = "btn-default btn-sm", style = "flex:1;")
        )
    ),
    box(title = "Plot Output", status = "primary",
        solidHeader = TRUE, width = 8,
        plotOutput("openair_plot", height = "570px"),
        uiOutput("plot_info_ui"))
  ),

  fluidRow(
    box(title = "\U0001f4d0 Model Evaluation Statistics",
        status = "primary", solidHeader = TRUE, width = 12,
        collapsible = TRUE, collapsed = TRUE,
        fluidRow(
          column(3,
            uiOutput("mod_obs_ui"), uiOutput("mod_mod_ui"),
            selectInput("mod_type", "Facet by:",
                        choices = c("default","month","season","weekday")),
            actionButton("btn_modstats", "Calculate",
                         class = "btn-primary btn-block",
                         icon  = icon("calculator"))
          ),
          column(9, verbatimTextOutput("modstats_out"))
        )
    )
  )
)

# ================================================================
#  TAB 4  EXPORT
# ================================================================
tab_export <- tabItem("export",
  sec_hdr("file-export", "Export & Reports"),

  fluidRow(
    box(title = "\U0001f4e6 Data Exports", status = "primary",
        solidHeader = TRUE, width = 6,
        fluidRow(
          column(6,
            tags$b("Dataset:"), br(), br(),
            downloadButton("exp_csv",  "  CSV",
              icon = icon("file-csv"),     class = "btn-default btn-block"), br(),
            downloadButton("exp_rds",  "  RDS",
              icon = icon("r-project"),    class = "btn-default btn-block"), br(),
            downloadButton("exp_xlsx", "  Excel",
              icon = icon("file-alt"),     class = "btn-default btn-block"), br(),
            downloadButton("exp_zip",  "  ZIP (all)",
              icon = icon("file-archive"), class = "btn-default btn-block")
          ),
          column(6,
            tags$b("Metadata:"), br(), br(),
            downloadButton("exp_loc",     "  Locations",
              icon = icon("map"),              class = "btn-default btn-block"), br(),
            downloadButton("exp_sensors", "  Sensors",
              icon = icon("broadcast-tower"),  class = "btn-default btn-block"), br(),
            downloadButton("exp_meta",    "  Fetch Summary",
              icon = icon("file-alt"),         class = "btn-default btn-block")
          )
        )
    ),
    box(title = "\U0001f4cb Dataset Summary Card",
        status = "info", solidHeader = TRUE, width = 6,
        uiOutput("export_card_ui"))
  )
)

# ================================================================
#  TAB 5  HELP
# ================================================================
tab_help <- tabItem("help",
  sec_hdr("question-circle", "Help & Guide"),

  fluidRow(
    box(title = "\U0001f680 Getting Started", status = "primary",
        solidHeader = TRUE, width = 6,
        tags$ol(style = "font-size:13.5px;line-height:2.4;padding-left:18px;",
          tags$li("Register at ",
            tags$a("explore.openaq.org",
                   href = "https://explore.openaq.org", target = "_blank"),
            " (free) to get your API key."),
          tags$li("Paste key in sidebar \u2192 ", tags$b("Connect")),
          tags$li("Search by city, click the map, or draw a bounding box"),
          tags$li("Click a map marker or location card to select a station."),
          tags$li("In Fetch Data: tick sensors, choose dates, click Fetch.",
		  tags$b("Exceeding API rate limits repeatedly can lead to a temporary or permanent ban from the OpenAQ API."),
			tags$a("\u2192 Read more here.",
                   href = "https://docs.openaq.org/using-the-api/rate-limits#rate-limit-headers", target = "_blank"), 
				   "To maintain uninterrupted access, plan your API requests carefully."), 
          tags$li("In Analysis: pick a plot type, set controls, Generate"),
          tags$li("Download plot (PNG/PDF/SVG) and data (CSV/Excel/ZIP)"),
          tags$li("Or upload your own CSV directly in the Analysis tab")
        )
    ),
    box(title = "\U0001f4da Resources", status = "info",
        solidHeader = TRUE, width = 6,
        div(class = "help-dl",
          tags$dl(
            tags$dt("API Documentation"),
            tags$dd(tags$a("docs.openaq.org",
                           href = "https://docs.openaq.org", target = "_blank")),
            tags$dt("OpenAQ Explorer (web)"),
            tags$dd(tags$a("explore.openaq.org",
                           href = "https://explore.openaq.org", target = "_blank")),
            tags$dt("openaq R package"),
            tags$dd(tags$a("github.com/openaq/openaq-r",
                           href = "https://github.com/openaq/openaq-r", target = "_blank")),
            tags$dt("openair R package"),
            tags$dd(tags$a("davidcarslaw.github.io/openair",
                           href = "https://davidcarslaw.github.io/openair/", target = "_blank"))
          )
        )
    )
  ),

  fluidRow(
    box(title = "\U0001f4ca Plot Descriptions", status = "success",
        solidHeader = TRUE, width = 12,
        fluidRow(lapply(names(PLOT_CATALOGUE), function(nm) {
          info <- PLOT_CATALOGUE[[nm]]
          column(4,
            div(style = "background:#f8fafb;border:1px solid #d5dde5;
                border-left:4px solid #2471a3;border-radius:8px;
                padding:12px;margin-bottom:14px;min-height:95px;",
              tags$p(tags$b(info$label),
                     tags$small(style = "color:#888;",
                                paste0("  (", nm, ")"))),
              tags$p(style = "font-size:12px;color:#555;margin:0;", info$desc),
              if (isTRUE(info$needs_wind))
                tags$small(style = "color:#b7770d;",
                           icon("wind"), " Requires ws & wd")
            )
          )
        }))
    )
  ),

  fluidRow(
    box(title = "\U0001f4c1 CSV Upload Format", status = "warning",
        solidHeader = TRUE, width = 6,
        tags$p("Required columns:"),
        tags$ul(
          tags$li(tags$code("date"), " \u2014 any standard datetime format"),
          tags$li("At least one pollutant column (pm25, no2, o3 \u2026)"),
          tags$li(tags$code("ws"), " \u2014 wind speed m/s (for wind/polar plots)"),
          tags$li(tags$code("wd"), " \u2014 wind direction 0\u2013360\u00b0")
        ),
        tags$p("Column names can differ \u2014 use the mapping panel in Analysis."),
        tags$pre(style = "font-size:11px;background:#f4f6f7;padding:10px;
                          border-radius:6px;",
"date,pm25,pm10,no2,ws,wd,temp,rh
2024-01-01 00:00,45.2,82.1,18.3,2.1,180,28.5,72
2024-01-01 01:00,48.7,91.3,21.0,1.8,195,27.9,75")
    ),
    box(title = "\u2753 Troubleshooting", status = "danger",
        solidHeader = TRUE, width = 6,
        div(class = "help-dl",
          tags$dl(
            tags$dt("401 Invalid API key"),
            tags$dd("Re-copy the full key from explore.openaq.org."),
            tags$dt("422 Validation error"),
            tags$dd("Usually out-of-range radius. Max = 25 km."),
            tags$dt("429 Rate limit"),
            tags$dd("Free tier ~60 req/min. Wait 60 s and retry."),
            tags$dt("No locations found"),
            tags$dd("Try different spelling, larger bbox, or coordinate search."),
            tags$dt("Empty measurement fetch"),
            tags$dd("Widen the date range; some sensors report infrequently."),
            tags$dt("Wind rose blank"),
            tags$dd("Data needs both ws and wd columns with valid values."),
            tags$dt("Calendar wrong year"),
            tags$dd("Set the Year control to a year present in your data.")
          )
        )
    )
  )
)


# ================================================================
#  TAB 6  ABOUT
# ================================================================
tab_about <- tabItem("about",
  sec_hdr("info-circle", "About This Application"),

  fluidRow(

    # ---- Identity card ----------------------------------------
    box(title = "API Data Retrieval & Analysis", status = "primary",
        solidHeader = TRUE, width = 4,
        div(style = "text-align:center;padding:20px 10px 14px;",
          div(style = "display:inline-flex;align-items:center;justify-content:center;
                       width:72px;height:72px;border-radius:6px;
                       border:1px solid var(--border-mid);
                       background:var(--bg-surface);margin-bottom:14px;",
            icon("satellite", style = "font-size:32px;color:var(--accent-cyan);")
          ),
          tags$h3(style = "font-family:var(--font-mono);color:var(--accent-cyan);
                           margin:0 0 4px;letter-spacing:.06em;font-size:16px;",
                  "API Data Retrieval & Analysis"),
          tags$p(style = "font-size:12px;color:var(--text-muted);
                          font-family:var(--font-mono);margin:0 0 16px;",
                 paste0("v", APP_VERSION, "  //  R Shiny + openair")),
          tags$hr(),
          tags$p(style = "font-size:13px;color:var(--text-base);line-height:1.7;
                          text-align:left;",
            "A web application for exploring real-time and historical air quality
             data from the global OpenAQ sensor network, with integrated
             atmospheric data analysis powered by the openair R package."
          )
        )
    ),

    # ---- What it does ----------------------------------------
    box(title = "What This App Does", status = "success",
        solidHeader = TRUE, width = 4,
        tags$ul(style = "font-size:13px;line-height:2.2;
                         padding-left:16px;color:var(--text-base);",
          tags$li(tags$b("Discover"), " air quality monitoring stations worldwide
                  by name, coordinates, bounding box, or country"),
          tags$li(tags$b("Fetch"), " raw, hourly, or daily sensor measurements
                  directly from the OpenAQ v3 REST API"),
          tags$li(tags$b("Inspect"), " data completeness and multi-sensor
                  previews in an openair-ready wide-format table"),
          tags$li(tags$b("Analyse"), " with 15 openair plot types - time series,
                  calendar, wind/polar, trend, scatter, and more"),
          tags$li(tags$b("Upload"), " your own CSV and run the same analysis
                  pipeline on any compatible dataset"),
          tags$li(tags$b("Export"), " data as CSV, RDS, Excel, or ZIP,
                  and plots as PNG, PDF, or SVG")
        )
    ),

    # ---- Data source -----------------------------------------
    box(title = "Data Source", status = "info",
        solidHeader = TRUE, width = 4,
        div(class = "help-dl",
          tags$p(style = "font-size:13px;color:var(--text-base);line-height:1.7;",
            "Air quality data is retrieved live from the ",
            tags$a("OpenAQ", href = "https://openaq.org", target = "_blank"),
            " open-data platform, which aggregates measurements from government
             reference monitors, low-cost sensors, and research instruments
             across 100+ countries."
          ),
          tags$dl(
            tags$dt("API Version"),
            tags$dd("OpenAQ v3  (api.openaq.org/v3)"),
            tags$dt("Parameters covered"),
            tags$dd("PM2.5, PM10, NO2, O3, SO2, CO, NO, BC, Temperature,
                     Relative Humidity, and more"),
            tags$dt("Temporal coverage"),
            tags$dd("Varies by station - from a single day to 10+ years"),
            tags$dt("Licence"),
            tags$dd(tags$a("CC BY 4.0",
                    href = "https://creativecommons.org/licenses/by/4.0/",
                    target = "_blank"),
                    " - free to use with attribution")
          )
        )
    )
  ),

  fluidRow(

    # ---- Technology stack ------------------------------------
    box(title = "Technology Stack", status = "warning",
        solidHeader = TRUE, width = 6,
        tags$table(
          class = "table table-condensed",
          style = "font-size:12.5px;",
          tags$thead(tags$tr(
            tags$th(style = "color:var(--accent-cyan);", "Package / Tool"),
            tags$th(style = "color:var(--accent-cyan);", "Role")
          )),
          tags$tbody(
            tags$tr(tags$td(tags$code("shiny")),
                    tags$td("Web application framework")),
            tags$tr(tags$td(tags$code("shinydashboard")),
                    tags$td("Dashboard layout & boxes")),
            tags$tr(tags$td(tags$code("openair")),
                    tags$td("Atmospheric data analysis & visualisation")),
            tags$tr(tags$td(tags$code("leaflet")),
                    tags$td("Interactive station map")),
            tags$tr(tags$td(tags$code("httr + jsonlite")),
                    tags$td("OpenAQ REST API client")),
            tags$tr(tags$td(tags$code("dplyr / tidyr")),
                    tags$td("Data wrangling & reshaping")),
            tags$tr(tags$td(tags$code("lubridate")),
                    tags$td("Datetime parsing & rounding")),
            tags$tr(tags$td(tags$code("ggplot2 + gridExtra")),
                    tags$td("Summary plot fallback renderer")),
            tags$tr(tags$td(tags$code("DT")),
                    tags$td("Interactive data tables")),
            tags$tr(tags$td(tags$code("openxlsx / zip")),
                    tags$td("Excel and archive export"))
          )
        )
    ),

    # ---- Workflow diagram ------------------------------------
    box(title = "Typical Workflow", status = "primary",
        solidHeader = TRUE, width = 6,
        div(style = "font-family:var(--font-mono);font-size:12px;
                     color:var(--text-base);line-height:1;",
          div(style = "display:grid;gap:0;",

            lapply(list(
              list(n="01", icon="key",           label="Connect",
                   desc="Paste OpenAQ API key and authenticate"),
              list(n="02", icon="map-marker-alt",label="Discover",
                   desc="Search stations by name, coords, or country"),
              list(n="03", icon="mouse-pointer", label="Select",
                   desc="Click a map marker or location card"),
              list(n="04", icon="download",      label="Fetch",
                   desc="Choose sensors, date range and aggregation"),
              list(n="05", icon="table",         label="Inspect",
                   desc="Review the wide-format data table"),
              list(n="06", icon="bar-chart",     label="Analyse",
                   desc="Pick a plot type, adjust controls, generate"),
              list(n="07", icon="save",          label="Export",
                   desc="Download data (CSV/Excel) and plots (PNG/PDF)")
            ), function(s) {
              div(style = "display:flex;align-items:flex-start;gap:12px;
                           padding:8px 0;border-bottom:1px solid var(--border-dim);",
                div(style = "min-width:28px;text-align:center;
                             color:var(--accent-cyan);font-weight:700;
                             font-size:13px;padding-top:1px;",
                    s$n),
                div(style = "min-width:16px;color:var(--text-muted);
                             padding-top:2px;",
                    icon(s$icon)),
                div(
                  div(style = "color:var(--text-bright);font-weight:700;
                               letter-spacing:.04em;", s$label),
                  div(style = "color:var(--text-muted);font-size:11.5px;
                               margin-top:1px;", s$desc)
                )
              )
            })
          )
        )
    )
  ),

  fluidRow(

    # ---- API key instructions --------------------------------
    box(title = "Getting an API Key", status = "danger",
        solidHeader = TRUE, width = 6,
        tags$ol(style = "font-size:13px;line-height:2.3;padding-left:18px;
                         color:var(--text-base);",
          tags$li("Visit ",
            tags$a("explore.openaq.org",
                   href = "https://explore.openaq.org", target = "_blank"),
            " and create a free account"),
          tags$li("Navigate to ", tags$b("Account Settings"),
                  " → ", tags$b("API Keys")),
          tags$li("Click ", tags$b("Generate New Key"),
                  " and copy the full string"),
          tags$li("Paste it into the sidebar API key field and click ",
                  tags$b("Connect")),
          tags$li("The free tier supports ~60 requests/minute - sufficient
                  for interactive exploration"),
          tags$li("For bulk downloads or automated pipelines, consider the
                  paid tier at ",
                  tags$a("openaq.org/developers",
                         href = "https://openaq.org/developers",
                         target = "_blank"))
        )
    ),

    # ---- Citation / acknowledgements -------------------------
    box(title = "Acknowledgements & Citation", status = "primary",
        solidHeader = TRUE, width = 6,
        div(class = "help-dl",
          tags$p(style = "font-size:13px;color:var(--text-base);line-height:1.7;",
            "When publishing results derived from OpenAQ data, please cite
             the data platform and this application:"
          ),
          tags$dt("Air quality data"),
          tags$dd(
            tags$em("OpenAQ. (2024). Open air quality data platform."),
            " Retrieved from ",
            tags$a("https://openaq.org", href = "https://openaq.org",
                   target = "_blank")
          ),
          tags$dt("Analysis methods"),
          tags$dd(
            "Carslaw, D.C. and Ropkins, K. (2012). ",
            tags$em("openair - An R package for air quality data analysis."),
            " Environmental Modelling & Software, 27-28, 52-61."
          ),
          tags$dt("This application"),
          tags$dd(
            tags$em("API Data Retrieval & Analysis"),
            paste0(" (v", APP_VERSION, "). "),
            "R Shiny application built with openair, leaflet, and httr."
          ),
          br(),
          tags$p(style = "font-size:11.5px;color:var(--text-muted);
                          font-family:var(--font-mono);",
            "OpenAQ data is provided under the CC BY 4.0 licence.
             Measurements are sourced from national monitoring agencies
             and research networks worldwide. Quality and completeness
             vary by station and parameter."
          )
        )
    ),
	
    # ---- Developers -------------------------
    box(title = "About Developers", status = "primary",
        solidHeader = TRUE, width = 12,
        div(class = "help-dl",
          tags$p(style = "font-size:13px;color:var(--text-base);line-height:1.7;",
            "The API Data Retrieval & ANalysis platform was developed based on a combination of R codes used for training during workshops such as;"
          ),
		  br(),

		  tags$dd(" 1. Air quality summer school in KNUST (2023)"),
		  tags$dd(" 2. West African Air quality summer school in KNUST (2024)"),
		  tags$dd(" 3. Air quality data analysis, visualization and understanding air quality with R in Nigeria (2024)"),
		  tags$dd(" 4. Afriset Air quality training (2025)"),
		  tags$dd(" 5. Africa Air quality summer school (2025)."),		  
          tags$dt("Cosmos Senyo Wemegah, PhD."),
          tags$dd("Research Fellow at Earth Observation Research Innovation Centre (EORIC) & Lecturer, Department of Atmospheric and Climate Science",
            tags$em("University of Energy and Natural Resources (UENR), Sunyani-Ghana."),
            "Cosmos drives urban climate resilience in West Africa, advancing citizen science and predictive frameworks to tackle heatwaves, pollution,
			and floods through innovative monitoring, modeling, anaysis and early‑warning systems."),
			
			

		  
		  tags$dd(
            tags$a("|UENR", href = "https://uenr.edu.gh/staff/cosmos-senyo-wemegah-phd/",
                   target = "_blank"),
			tags$a("|EORIC", href = "https://eoric.uenr.edu.gh/?bunch_team=mr-cosmos-senyo-wemegah",
                   target = "_blank"),
			tags$a("|Google Scholar", href = "https://scholar.google.com/citations?user=e5_WEhEAAAAJ&hl=en",
                   target = "_blank"),
			tags$a("|LinkedIn", href = "https://www.linkedin.com/in/cosmos-wemegah/",
                   target = "_blank"),
			tags$a("|github", href = "https://github.com/CosmosWems",
                   target = "_blank"),
			tags$a("|Ecobreath", href = "https://www.ecobreathforcities.org/our-team/",
                   target = "_blank")
          ),

          tags$dt("Victoria Owusu Tawiah, PhD."),
          tags$dd("Air Quality Program Lead",
            tags$em("Kigali Collaborative Research Centre (KCRC), Kigali-Rwanda."),
            "Victoria bridge science and policy through sensor networks, transparent data, and capacity building, advancing Africa’s monitoring infrastructure and delivering actionable insights for resilient urban planning and public health."),
		  
		  tags$dd(
            tags$a("|KCRC", href = "https://www.kcrc.rw/?page_id=106",
                   target = "_blank"),
			tags$a("|LinkedIn", href = "https://gh.linkedin.com/in/victoria-owusu-tawiah-32029547",
                   target = "_blank"),
			tags$a("|Ecobreath", href = "https://www.ecobreathforcities.org/our-team/",
                   target = "_blank")
          ),		  
		  


          br(),
          tags$p(style = "font-size:11.5px;color:var(--text-muted);
                          font-family:var(--font-mono);",
            "This platform is not for sale in any form. It is basically for research and educational purposes to help retrieve openaq data and analyse air quality data."
          )
        )
    )	
  )
)

# ================================================================
#  SIDEBAR
# ================================================================
sidebar <- dashboardSidebar(width = 260,
  useShinyjs(),

  div(class = "api-key-panel",
    tags$label("\U0001f511  OpenAQ API Key"),
    passwordInput("api_key", label = NULL,
                  placeholder = "Paste your key here \u2026"),
    div(style = "display:flex;gap:6px;margin-top:7px;",
      actionButton("btn_connect", "Connect",
                   icon = icon("plug"), class = "btn-success btn-sm",
                   style = "flex:1;"),
      actionButton("btn_disconnect", "",
                   icon = icon("times"), class = "btn-danger btn-sm",
                   title = "Disconnect")
    )
  ),

  sidebarMenu(id = "main_menu",
    menuItem("Location Explorer", tabName = "explorer",
             icon = icon("map-marker-alt")),
    menuItem("Fetch Data",        tabName = "fetch",
             icon = icon("download")),
    menuItem("Analysis",          tabName = "analysis",
             icon = icon("bar-chart")),
    menuItem("Export",            tabName = "export",
             icon = icon("file-export")),
    menuItem("Help & Guide",   tabName = "help",
             icon = icon("question-circle")),
    menuItem("About",          tabName = "about",
             icon = icon("info-circle"))
  ),

  div(class = "status-footer", uiOutput("conn_status_ui"))
)

# ================================================================
#  MAIN UI
# ================================================================
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = tags$span(
      style = "font-family:\'Share Tech Mono\',monospace;font-size:12px;letter-spacing:.06em;",
      icon("satellite"), " API Data Retrieval & Analysis"
    ),
    titleWidth = 260,
    tags$li(class = "dropdown",
      tags$button(
        id = "theme-toggle-btn",
        onclick = "toggleTheme()",
        HTML("&#9790; Dark"),
        title = "Switch to Light theme"
      )
    ),
    tags$li(class = "dropdown",
      tags$a(href = "https://docs.openaq.org", target = "_blank",
             style = "padding:15px 8px;font-size:11.5px;font-family:\'Share Tech Mono\',monospace;",
             icon("book"), " Docs")),
    tags$li(class = "dropdown",
      tags$a(href = "https://explore.openaq.org", target = "_blank",
             style = "padding:15px 8px;font-size:11.5px;font-family:\'Share Tech Mono\',monospace;",
             icon("globe"), " OpenAQ"))
  ),

  sidebar,

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$script(src = "theme.js"),
      tags$script(HTML('
        $(document).on("shiny:notification", function(e) {
          setTimeout(function() { $(".shiny-notification").remove(); }, 9000);
        });
      '))
    ),
    tabItems(tab_explorer, tab_fetch, tab_analysis, tab_export, tab_help, tab_about),
    tags$footer(class = "main-footer",
      paste0("API Data Retrieval & Analysis v", APP_VERSION,
             "  \u00b7  R Shiny + openair  \u00b7  "),
      tags$a("openaq.org", href = "https://openaq.org", target = "_blank")
    )
  )
)