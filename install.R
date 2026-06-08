# ============================================================
#  install.R  –  Run ONCE before launching the app
#  In RStudio : open this file → click Source
#  In R console: source("install.R")
# ============================================================
cat("=======================================================\n")
cat("  OpenAQ Air Quality Explorer  –  Package Installer   \n")
cat("=======================================================\n\n")

cran_pkgs <- c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets",
  "leaflet", "leaflet.extras", "DT",
  "openair", "dplyr", "tidyr", "lubridate",
  "httr", "jsonlite", "RColorBrewer",
  "ggplot2", "scales", "remotes", "zip"
)

for (pkg in cran_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  Installing  %-20s ...\n", pkg))
    tryCatch(
      install.packages(pkg, quiet = TRUE,
                       repos = "https://cloud.r-project.org"),
      error = function(e) cat("  WARNING: could not install", pkg, "\n")
    )
  } else {
    cat(sprintf("  OK   %-20s\n", pkg))
  }
}

cat("\n  Checking openaq (GitHub) ...\n")
if (!requireNamespace("openaq", quietly = TRUE)) {
  tryCatch({
    remotes::install_github("openaq/openaq-r", quiet = TRUE)
    cat("  openaq installed from GitHub.\n")
  }, error = function(e) {
    cat("  WARNING: could not install openaq from GitHub.\n")
    cat("  The app uses direct httr API calls as fallback.\n")
  })
} else {
  cat("  OK   openaq\n")
}

cat("\n=======================================================\n")
cat("  All done!  Launch the app:\n\n")
cat("    shiny::runApp()       # from app directory\n")
cat("    source('run_app.R')   # or use this launcher\n")
cat("=======================================================\n")
