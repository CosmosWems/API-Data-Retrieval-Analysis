# ================================================================
#  run_app.R  –  OpenAQ Explorer launcher
#
#  Usage (from the openaq_app/ folder):
#    Rscript run_app.R
#    Rscript run_app.R --port 4040 --host 0.0.0.0
#
#  Or from an R console / RStudio:
#    source("run_app.R")
#    # or simply press the "Run App" button in RStudio
# ================================================================

# ----------------------------------------------------------------
#  1. Parse optional CLI arguments
# ----------------------------------------------------------------
args      <- commandArgs(trailingOnly = TRUE)
.get_arg  <- function(flag, default) {
  idx <- which(args == flag)
  if (length(idx) > 0 && length(args) >= idx + 1)
    args[idx + 1]
  else
    default
}
APP_PORT   <- as.integer(.get_arg("--port", "3838"))
APP_HOST   <- .get_arg("--host", "127.0.0.1")
APP_LAUNCH <- !("--no-browser" %in% args)

cat("=============================================================\n")
cat("  OpenAQ Explorer  –  R Shiny App\n")
cat("=============================================================\n")

# ----------------------------------------------------------------
#  2. Verify we are in the right directory
# ----------------------------------------------------------------
if (!file.exists("global.R") || !file.exists("server.R") ||
    !file.exists("ui.R")) {
  stop(
    "run_app.R must be run from the openaq_app/ directory.\n",
    "  cd openaq_app && Rscript run_app.R\n"
  )
}

# ----------------------------------------------------------------
#  3. CRAN mirror (set once; suppresses the interactive chooser)
# ----------------------------------------------------------------
if (is.null(getOption("repos")) ||
    identical(getOption("repos"), c(CRAN = "@CRAN@"))) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# ----------------------------------------------------------------
#  4. Required packages  (install if absent)
# ----------------------------------------------------------------
required_pkgs <- c(
  # Core Shiny stack
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets",
  # Map
  "leaflet", "leaflet.extras",
  # Tables
  "DT",
  # Air quality analysis
  "openair",
  # Data wrangling
  "dplyr", "tidyr", "lubridate",
  # API / HTTP
  "httr", "jsonlite",
  # Visualisation helpers
  "RColorBrewer", "ggplot2", "scales", "gridExtra",
  # Export
  "remotes", "zip"
)

cat("\nChecking dependencies ...\n")
missing_pkgs <- required_pkgs[
  !sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  cat("  Installing: ", paste(missing_pkgs, collapse = ", "), "\n", sep = "")
  tryCatch(
    install.packages(missing_pkgs, quiet = TRUE),
    error = function(e)
      warning("Could not install: ", paste(missing_pkgs, collapse = ", "),
              "\n  Error: ", e$message)
  )
}

# openaq from GitHub (not on CRAN)
if (!requireNamespace("openaq", quietly = TRUE)) {
  cat("  Installing openaq from GitHub ...\n")
  tryCatch({
    if (!requireNamespace("remotes", quietly = TRUE))
      install.packages("remotes", quiet = TRUE)
    remotes::install_github("openaq/openaq-r", quiet = TRUE)
  }, error = function(e)
    message("  openaq GitHub install failed: ", e$message,
            "\n  Some API features will be unavailable."))
}

cat("  All dependencies OK.\n\n")

# ----------------------------------------------------------------
#  5. Print session / version info
# ----------------------------------------------------------------
cat("  R version : ", paste0(R.version$major, ".", R.version$minor), "\n")
cat("  shiny     : ", as.character(packageVersion("shiny")), "\n")
tryCatch(
  cat("  openair   : ", as.character(packageVersion("openair")), "\n"),
  error = function(e) cat("  openair   : not installed\n"))
cat("\n")

# ----------------------------------------------------------------
#  6. Verify the openair summaryPlot situation and warn early
# ----------------------------------------------------------------
tryCatch({
  library(openair, warn.conflicts = FALSE, quietly = TRUE)
  sp_exists <- tryCatch(
    { get("summaryPlot", envir = asNamespace("openair"), inherits = FALSE);
      TRUE },
    error = function(e) FALSE
  )
  sd_exists <- tryCatch(
    { get("summaryData",  envir = asNamespace("openair"), inherits = FALSE);
      TRUE },
    error = function(e) FALSE
  )
  if (!sp_exists && !sd_exists) {
    cat("  [NOTE] openair::summaryPlot not found in this build ",
        "(openair ", as.character(packageVersion("openair")), ").\n",
        "         The app will use a built-in ggplot2 fallback instead.\n\n",
        sep = "")
  } else {
    fn_name <- if (sp_exists) "summaryPlot" else "summaryData"
    cat("  [OK] openair summary function found: ", fn_name, "\n\n", sep = "")
  }
}, error = function(e)
  cat("  [WARN] Could not load openair:", e$message, "\n\n"))

# ----------------------------------------------------------------
#  7. Launch the app
# ----------------------------------------------------------------
cat(sprintf("  Starting app on  http://%s:%d\n", APP_HOST, APP_PORT))
cat("  Press Ctrl+C (or close this terminal) to stop.\n")
cat("=============================================================\n\n")

shiny::runApp(
  appDir  = ".",
  host    = APP_HOST,
  port    = APP_PORT,
  launch.browser = APP_LAUNCH,
  display.mode   = "normal"
)
