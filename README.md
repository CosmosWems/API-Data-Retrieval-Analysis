# API Data Retrieval & Analysis

> **A Shiny web application for exploring global air quality data**
> via the [OpenAQ v3 API](https://api.openaq.org/v3) with integrated
> atmospheric analysis powered by [openair](https://davidcarslaw.github.io/openair/).

---

## Table of Contents

1. [What This App Does](#what-this-app-does)
2. [File Structure](#file-structure)
3. [Prerequisites](#prerequisites)
   - [R Version](#r-version)
   - [Get a Free OpenAQ API Key](#get-a-free-openaq-api-key)
4. [Quick Start - RStudio (Recommended)](#quick-start--rstudio-recommended)
5. [Quick Start - Plain R Console](#quick-start--plain-r-console)
6. [Quick Start - Terminal / Command Line](#quick-start--terminal--command-line)
7. [Installing Dependencies Manually](#installing-dependencies-manually)
8. [Running on a Server or Shiny Server](#running-on-a-server-or-shiny-server)
9. [Configuration Options](#configuration-options)
10. [Troubleshooting](#troubleshooting)
11. [Package Reference](#package-reference)
12. [Licence & Citation](#licence--citation)

---

## What This App Does

OpenAQ Explorer lets you without writing any code:

- 🗺️ **Discover** monitoring stations worldwide by city name, coordinates, bounding box, or country
- 📥 **Fetch** raw, hourly, or daily sensor measurements directly from the OpenAQ API
- 📊 **Analyse** data with 15 professional plot types (time series, calendar, wind rose, polar plots, trend analysis, scatter)
- 📤 **Export** data as CSV, Excel, RDS, or ZIP; plots as PNG, PDF, or SVG
- 🌓 **Switch** between a dark terminal theme and a light theme

---

## File Structure

After unzipping `API_Data_Retrieval.zip`, you will have:

```
API_Data_Retrieval/
│
├── global.R              # Package loading, constants, shared helpers
├── server.R              # All reactive logic and data processing
├── ui.R                  # All UI layout and controls
├── run_app.R             # Launcher script (dependency check + runApp)
│
├── R/
│   ├── api_helpers.R     # OpenAQ v3 REST API wrappers
│   ├── data_helpers.R    # Data parsing, flattening, and transformation
│   ├── plot_helpers.R    # openair plot dispatcher (15 plot types)
│   └── ui_helpers.R      # Shared UI components (status boxes, headers)
│
└── www/
    ├── styles.css        # Full dark/light theme (CSS custom properties)
    └── theme.js          # Dark ↔ light toggle logic
```

> **Important:** Always launch the app **from inside the `API_Data_Retrieval/` folder**.
> The app uses relative paths (`R/`, `www/`) that only resolve correctly
> when the working directory is `API_Data_Retrieval/`.

---

## Prerequisites

### R Version

| Requirement | Minimum | Recommended |
|-------------|---------|-------------|
| R           | 4.1.0   | 4.3.0 +     |
| RStudio     | 1.4     | 2023.09 +   |

Download R from [cran.r-project.org](https://cran.r-project.org)
Download RStudio from [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/)

### Get a Free OpenAQ API Key

You need a key before the app can fetch any data. It takes about two minutes:

1. Go to <https://explore.openaq.org> and create a free account
2. Log in → click your profile icon (top-right) → **API Keys**
3. Click **Generate New Key** - give it any name
4. **Copy the full key** and keep it somewhere safe (a text file is fine)

> The free tier supports approximately 60 requests per minute, which is more
> than sufficient for interactive exploration.

---

## Quick Start - RStudio (Recommended)

This is the easiest method.

### Step 1 - Open the project

**Option A - Open folder directly:**

```
File → Open Project… (or Open Folder…)
```

Navigate to and select the `API_Data_Retrieval/` folder.

**Option B - Open a specific file:**

In RStudio, go to `File → Open File…` and open any of `global.R`,
`server.R`, or `ui.R` inside `API_Data_Retrieval/`.

---

### Step 2 - Set the working directory

Make sure your working directory is `API_Data_Retrieval/`. Check the path shown
at the top of the RStudio **Console** pane. If it is wrong, set it with:

```r
setwd("/path/to/API_Data_Retrieval")   # adjust path to match your system
```

Or use the menu: **Session → Set Working Directory → To Source File Location**
(after opening one of the `.R` files in the app folder).

---

### Step 3 - Install dependencies (first run only)

Paste this block into the RStudio Console and press **Enter**. This only
needs to be done once - packages are already present on subsequent runs.

```r
# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install all CRAN packages
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets",
  "leaflet", "leaflet.extras", "DT", "openair",
  "dplyr", "tidyr", "lubridate", "httr", "jsonlite",
  "RColorBrewer", "ggplot2", "scales", "gridExtra",
  "remotes", "zip", "openxlsx"
))

# Install openaq from GitHub (not on CRAN)
remotes::install_github("openaq/openaq-r")
```

> **Note:** `openxlsx` is optional - it enables the Excel (.xlsx) export.
> The app works without it and will fall back to CSV.

---

### Step 4 - Launch the app

**Method A - Run App button (simplest):**

With any of `global.R`, `server.R`, or `ui.R` open in the editor,
click the **▶ Run App** button in the top-right corner of the editor pane.

```
┌─────────────────────────────────────────┐
│  global.R  ×          ▶ Run App  ▾      │
└─────────────────────────────────────────┘
```

**Method B - Source the launcher:**

```r
source("run_app.R")
```

**Method C - Direct `shiny::runApp()`:**

```r
shiny::runApp(".")
```

All three methods open the app in your default web browser at
`http://127.0.0.1:3838`.

---

### Step 5 - Use the app

1. **Paste your API key** in the sidebar field and click **Connect**
2. A green **Connected** badge appears when authentication succeeds
3. Go to **Location Explorer** and search for a city
4. Click a station marker on the map
5. Go to **Fetch Data**, choose sensors and date range, click **Fetch Data**
6. Go to **Analysis**, pick a plot type, click **Generate Plot**

---

## Quick Start - Plain R Console

Use this method if you are running R without RStudio (e.g. R GUI on Windows
or macOS, or a plain R session on Linux).

```r
# 1. Set the working directory to the app folder
setwd("/path/to/API_Data_Retrieval")    # ← change this path

# 2. Install packages (first run only)
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets",
  "leaflet", "leaflet.extras", "DT", "openair",
  "dplyr", "tidyr", "lubridate", "httr", "jsonlite",
  "RColorBrewer", "ggplot2", "scales", "gridExtra",
  "remotes", "zip", "openxlsx"
))
remotes::install_github("openaq/openaq-r")

# 3. Launch
shiny::runApp(".")
```

The app opens in your default browser. To stop it, press **Ctrl+C** in the
R console (or close the R session).

---

## Quick Start - Terminal / Command Line

Use this method for headless servers, automated pipelines, or if you prefer
the terminal.

### macOS / Linux

```bash
# Navigate to the app folder
cd /path/to/API_Data_Retrieval

# Run with the launcher script
Rscript run_app.R
```

### Windows (Command Prompt)

```bat
cd C:\path\to\API_Data_Retrieval
Rscript run_app.R
```

### Windows (PowerShell)

```powershell
Set-Location "C:\path\to\API_Data_Retrieval"
Rscript run_app.R
```

The launcher script (`run_app.R`) automatically:

- Checks all required packages and installs any that are missing
- Prints the R and openair versions
- Warns if `openair::summaryPlot` is unavailable (uses a ggplot2 fallback)
- Starts the app at `http://127.0.0.1:3838` and opens your browser

### Optional CLI flags

```bash
# Custom port
Rscript run_app.R --port 4040

# Listen on all network interfaces (useful for LAN access)
Rscript run_app.R --host 0.0.0.0 --port 3838

# Suppress automatic browser launch
Rscript run_app.R --no-browser

# Combined
Rscript run_app.R --host 0.0.0.0 --port 8080 --no-browser
```

> **Security note:** Using `--host 0.0.0.0` makes the app accessible to
> anyone on your network. Only use this on a trusted private network or
> behind a firewall.

---

## Installing Dependencies Manually

If automatic installation fails (e.g. on a locked-down corporate machine),
install packages one at a time:

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))

# --- Core Shiny stack ---
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyjs")
install.packages("shinyWidgets")

# --- Map ---
install.packages("leaflet")
install.packages("leaflet.extras")

# --- Tables ---
install.packages("DT")

# --- Air quality analysis ---
install.packages("openair")

# --- Data wrangling ---
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")

# --- API / HTTP ---
install.packages("httr")
install.packages("jsonlite")

# --- Visualisation ---
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("scales")
install.packages("gridExtra")

# --- Export ---
install.packages("remotes")
install.packages("zip")
install.packages("openxlsx")   # optional - for Excel export

# --- openaq R package (GitHub only) ---
remotes::install_github("openaq/openaq-r")
```

### Verifying installation

```r
pkgs <- c(
  "shiny","shinydashboard","shinyjs","shinyWidgets",
  "leaflet","leaflet.extras","DT","openair",
  "dplyr","tidyr","lubridate","httr","jsonlite",
  "RColorBrewer","ggplot2","scales","gridExtra","remotes","zip"
)
ok      <- sapply(pkgs, requireNamespace, quietly = TRUE)
missing <- pkgs[!ok]

if (length(missing) == 0) {
  cat("All packages installed OK\n")
} else {
  cat("Missing:", paste(missing, collapse = ", "), "\n")
}
```

---

## Running on a Server or Shiny Server

### Shiny Server (open source)

Place the entire `API_Data_Retrieval/` folder inside your Shiny Server apps directory
(typically `/srv/shiny-server/`):

```bash
sudo cp -r API_Data_Retrieval/ /srv/shiny-server/API_Data_Retrieval/
```

The app will be available at `http://your-server-ip:3838/API_Data_Retrieval/`.

Make sure all packages are installed for the `shiny` system user:

```bash
sudo su - shiny -s /bin/bash
R -e "install.packages(c('shiny','shinydashboard','shinyjs','shinyWidgets','leaflet','leaflet.extras','DT','openair','dplyr','tidyr','lubridate','httr','jsonlite','RColorBrewer','ggplot2','scales','gridExtra','remotes','zip'), repos='https://cloud.r-project.org')"
R -e "remotes::install_github('openaq/openaq-r')"
```

### Posit Connect / shinyapps.io

Use the RStudio deployment button or `rsconnect`:

```r
# Install rsconnect if needed
install.packages("rsconnect")

# Deploy (fill in your account details)
rsconnect::deployApp(
  appDir   = "/path/to/API_Data_Retrieval",
  appName  = "openaq-explorer",
  account  = "your-account-name"
)
```

> **Note for cloud deployment:** The API key is entered interactively by each
> user in the browser - it is never stored on the server.

---

## Configuration Options

All constants live in `global.R`. You can edit them without touching any
other file:

| Constant | Default | Description |
|----------|---------|-------------|
| `APP_VERSION` | `"2.0.0"` | Shown in the footer and About tab |
| `OPENAQ_MAX_RAD` | `25000L` | Maximum search radius in metres (API hard limit) |
| `AVG_TIMES` | `c("min","hour","day","week","month","year")` | Temporal aggregation choices |

API base URL (in `R/api_helpers.R`):

```r
OPENAQ_BASE <- "https://api.openaq.org/v3"
```

---

## Troubleshooting

### App does not start

| Symptom | Fix |
|---------|-----|
| `Error: could not find function "runApp"` | Run `install.packages("shiny")` first |
| `Error in source("R/ui_helpers.R")` | Working directory is wrong - run `setwd("/path/to/API_Data_Retrieval")` |
| `There is no package called 'shinydashboard'` | Run the full install block in [Installing Dependencies Manually](#installing-dependencies-manually) |
| Port already in use | Change the port: `shiny::runApp(".", port = 4040)` |

### API / Connection errors

| Error message | Cause | Fix |
|---------------|-------|-----|
| `Invalid API key (401)` | Wrong or expired key | Re-copy from explore.openaq.org |
| `Access forbidden (403)` | Key lacks permissions | Check API key settings on the OpenAQ site |
| `Validation error (422)` | Search radius > 25 km | Reduce radius to 25 km maximum |
| `Rate limit (429)` | Too many requests | Wait 60 seconds, then retry |
| `No locations found` | Spelling / no data in area | Try coordinates search or a wider bounding box |
| `No measurements returned` | Sensor inactive | Widen date range or try daily aggregation |

### Plot errors

| Error | Fix |
|-------|-----|
| `summaryPlot is not an exported object` | Already handled - app uses a ggplot2 fallback automatically |
| `None of the selected pollutant columns exist` | Click **⚙ Prepare Data** before generating a plot |
| `Column ws / wd not in data` | Wind/polar plots need wind speed and direction columns. Fetch a station with meteorological sensors |
| `All pollutant columns are entirely NA` | Widen the date range; the sensor may have no data in the selected period |

### RStudio-specific

| Issue | Fix |
|-------|-----|
| **Run App** button missing | Make sure you have `server.R` or `ui.R` open - not a helper file |
| App opens but looks broken | Clear browser cache; try Chrome or Edge |
| Console shows package conflicts | Restart R (`Session → Restart R`) then re-launch |

---

## Package Reference

| Package | Version tested | Purpose |
|---------|---------------|---------|
| `shiny` | 1.8+ | Web application framework |
| `shinydashboard` | 0.7+ | Dashboard layout |
| `shinyjs` | 2.1+ | JavaScript helpers |
| `shinyWidgets` | 0.8+ | Enhanced UI widgets |
| `leaflet` | 2.2+ | Interactive map |
| `leaflet.extras` | 1.0+ | Draw toolbar on map |
| `DT` | 0.30+ | Interactive data tables |
| `openair` | 2.18+ | Atmospheric data analysis (15 plot types) |
| `dplyr` | 1.1+ | Data manipulation |
| `tidyr` | 1.3+ | Data reshaping |
| `lubridate` | 1.9+ | Date/time parsing |
| `httr` | 1.4+ | HTTP requests to OpenAQ API |
| `jsonlite` | 1.8+ | JSON parsing |
| `RColorBrewer` | 1.1+ | Colour palettes |
| `ggplot2` | 3.4+ | Summary plot fallback |
| `scales` | 1.3+ | Axis formatting |
| `gridExtra` | 2.3+ | Multi-panel summary plot layout |
| `remotes` | 2.4+ | GitHub package installation |
| `zip` | 2.3+ | ZIP archive export |
| `openxlsx` | 4.2+ | Excel export *(optional)* |
| `openaq` | GitHub | OpenAQ R client *(optional)* |

---

## Licence & Citation

**Air quality data** is provided by [OpenAQ](https://openaq.org) under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

**Analysis methods** use the openair package. Please cite:

> Carslaw, D.C. and Ropkins, K. (2012).
> *openair - An R package for air quality data analysis.*
> Environmental Modelling & Software, 27–28, 52–61.

**This application:**

> OpenAQ Explorer (v1.0.0). R Shiny application built with openair,
> leaflet, and the OpenAQ v3 API. 2026.
> **BY: Cosmos Senyo Wemegah (PhD)**. A Research Fellow with EORIC-UENR, Ghana.
> 

---

*Questions or issues? 
> Contact me through cosmossenyo@gmail.com or LinkedIn (https://www.linkedin.com/in/cosmos-wemegah/)
> Check the [Help & Guide tab](# ) inside the app
> or consult the [OpenAQ API documentation](https://docs.openaq.org).*

