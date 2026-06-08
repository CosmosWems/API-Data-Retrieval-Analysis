# ================================================================
#  global.R  –  loaded once at app start
#  Packages · helper sources · all global constants
#
#  Fix: status_html and %||% are defined HERE (before server.R
#  and ui.R are evaluated) so both files can call them freely.
# ================================================================

# ---- Silent auto-install -----------------------------------
.auto_install <- function(pkgs) {
  for (p in pkgs)
    if (!requireNamespace(p, quietly = TRUE))
      tryCatch(install.packages(p, quiet = TRUE,
                                repos = "https://cloud.r-project.org"),
               error = function(e) NULL)
}

.auto_install(c(
  "shiny","shinydashboard","shinyjs","shinyWidgets",
  "leaflet","leaflet.extras","DT","openair",
  "dplyr","tidyr","lubridate","httr","jsonlite",
  "RColorBrewer","ggplot2","scales","remotes","zip",
  "gridExtra"   # used by summaryPlot fallback
))

if (!requireNamespace("openaq", quietly = TRUE))
  tryCatch(remotes::install_github("openaq/openaq-r", quiet = TRUE),
           error = function(e) NULL)

# ---- Load packages -----------------------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(shinyWidgets)
  library(leaflet)
  library(leaflet.extras)
  library(DT)
  library(openair)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(httr)
  library(jsonlite)
  library(RColorBrewer)
  library(ggplot2)
  library(scales)
  library(remotes)
  library(zip)
})
tryCatch(suppressPackageStartupMessages(library(openaq)),
         error = function(e) NULL)

# ---- Early definitions (used by both server.R and ui.R) -----
`%||%` <- function(a, b)
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# ---- Source R/ helpers (order matters) ----------------------
source("R/ui_helpers.R",   local = FALSE)
source("R/api_helpers.R",  local = FALSE)
source("R/data_helpers.R", local = FALSE)
source("R/plot_helpers.R", local = FALSE)

# ================================================================
#  CONSTANTS
# ================================================================
APP_VERSION <- "1.0.0"

# OpenAQ parameter reference table
OPENAQ_PARAMS <- data.frame(
  id    = c(1L, 2L, 7L, 8L, 9L, 10L, 12L, 19L, 42L, 130L,
            132L, 133L, 134L, 144L, 145L, 1928L, 2710L),
  name  = c("pm10","pm25","no2","co","o3","so2","no","bc",
            "bc_880nm","nox","pm1","pm4","um025",
            "temperature","relativehumidity","um100","um010"),
  units = c("µg/m³","µg/m³","µg/m³","µg/m³","µg/m³","µg/m³",
            "µg/m³","µg/m³","µg/m³","µg/m³","µg/m³","µg/m³",
            "particles/cm³","°C","%","particles/cm³",
            "particles/cm³"),
  label = c("PM10","PM2.5","NO₂","CO","O₃","SO₂","NO",
            "Black Carbon","BC 880nm","NOx","PM1","PM4",
            "PM >0.25 µm","Temperature","Rel. Humidity",
            "PM >100 µm","PM >0.1 µm"),
  stringsAsFactors = FALSE
)

# Openair colour palettes
OPENAIR_PALETTES <- c(
  "heat","RdYlGn","RdYlBu","Blues","Greens","Reds",
  "YlOrRd","YlGnBu","Purples","viridis","inferno",
  "plasma","magma","default","increment","brewer1"
)

# Temporal averaging choices
AVG_TIMES <- c("min","hour","day","week","month","year")

# Facet / conditioning types
FACET_TYPES <- c("default","month","season","weekday","year","weekend")

# Map tile providers
MAP_TILES <- list(
  "CartoDB Light"    = "CartoDB.Positron",
  "CartoDB Dark"     = "CartoDB.DarkMatter",
  "OpenStreetMap"    = "OpenStreetMap",
  "Satellite (ESRI)" = "Esri.WorldImagery",
  "Topo (ESRI)"      = "Esri.WorldTopoMap"
)

# WHO / AQI break-point presets
WHO_BREAKS <- list(
  pm25 = list(
    breaks = c(0, 15, 25, 50, 75, 150, 500),
    labels = c("Good","Acceptable","Elevated","High","Very High","Hazardous"),
    cols   = c("#00b050","#ffff00","#ff9900","#ff0000","#990099","#7e0023")
  ),
  pm10 = list(
    breaks = c(0, 50, 100, 150, 250, 350, 1000),
    labels = c("Good","Acceptable","Elevated","High","Very High","Hazardous"),
    cols   = c("#00b050","#ffff00","#ff9900","#ff0000","#990099","#7e0023")
  )
)

# Plot catalogue (used by both ui.R and server.R)
PLOT_CATALOGUE <- list(
  summaryPlot    = list(label = "Summary Plot",      group = "Summary & Time",
    needs_wind = FALSE,
    desc = "Data overview: time series + completeness bars for every variable."),
  timePlot       = list(label = "Time Plot",         group = "Summary & Time",
    needs_wind = FALSE,
    desc = "Multi-variable line plot averaged to hourly/daily/monthly resolution."),
  timeVariation  = list(label = "Time Variation",    group = "Summary & Time",
    needs_wind = FALSE,
    desc = "4-panel: diurnal cycle, day-of-week, monthly mean, and combined hour×weekday."),
  calendarPlot   = list(label = "Calendar Plot",     group = "Summary & Time",
    needs_wind = FALSE,
    desc = "Calendar heat-map by pollutant level. Reveals episodes & seasonal patterns."),
  windRose       = list(label = "Wind Rose",         group = "Wind & Rose",
    needs_wind = TRUE,
    desc = "Frequency and speed of wind from each compass direction."),
  pollutionRose  = list(label = "Pollution Rose",    group = "Wind & Rose",
    needs_wind = TRUE,
    desc = "Wind-rose coloured by pollutant concentration – shows likely source sectors."),
  percentileRose = list(label = "Percentile Rose",   group = "Wind & Rose",
    needs_wind = TRUE,
    desc = "5th–95th percentile distribution of concentration by wind direction."),
  polarPlot      = list(label = "Polar Plot",        group = "Polar",
    needs_wind = TRUE,
    desc = "Bivariate polar: concentration vs wind speed & direction."),
  polarFreq      = list(label = "Polar Frequency",   group = "Polar",
    needs_wind = TRUE,
    desc = "Polar frequency weighted by count or concentration across speed intervals."),
  polarAnnulus   = list(label = "Polar Annulus",     group = "Polar",
    needs_wind = TRUE,
    desc = "Concentration vs wind direction × time period (hour, month, season)."),
  smoothTrend    = list(label = "Smooth Trend",      group = "Trend",
    needs_wind = FALSE,
    desc = "Smooth trend with CI bands fitted to time-averaged data."),
  TheilSen       = list(label = "Theil-Sen Trend",   group = "Trend",
    needs_wind = FALSE,
    desc = "Robust non-parametric trend: slope (units/yr), p-value, CI."),
  trendLevel     = list(label = "Trend Level",       group = "Trend",
    needs_wind = FALSE,
    desc = "2-D heat-map across two time dimensions (e.g. hour × month)."),
  scatterPlot    = list(label = "Scatter Plot",      group = "Model Evaluation",
    needs_wind = FALSE,
    desc = "X vs Y scatter with optional grouping and smooth fit line."),
  modStats       = list(label = "Model Statistics",  group = "Model Evaluation",
    needs_wind = FALSE,
    desc = "MB, NMB, RMSE, r, FAC2 – standard performance metrics.")
)

# Build grouped selectInput choices from PLOT_CATALOGUE
plot_choices <- function() {
  grps <- unique(sapply(PLOT_CATALOGUE, `[[`, "group"))
  out  <- list()
  for (g in grps) {
    items <- Filter(function(x) x$group == g, PLOT_CATALOGUE)
    out[[g]] <- setNames(names(items), sapply(items, `[[`, "label"))
  }
  out
}
