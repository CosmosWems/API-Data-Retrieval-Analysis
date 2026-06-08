# ================================================================
#  R/ui_helpers.R  –  Shared UI helper functions
#
#  Loaded by global.R so status_html / sec_hdr / scroll_box are
#  available in BOTH server.R and ui.R without redefinition.
# ================================================================

# Coloured status message box
status_html <- function(msg, type = "neutral")
  shiny::div(class = paste0("status-box status-", type), shiny::HTML(msg))

# Left-bordered section header
sec_hdr <- function(icon_nm, txt)
  shiny::div(class = "sec-hdr",
             shiny::icon(icon_nm),
             shiny::tags$span(txt))

# Scrollable container
scroll_box <- function(..., height = "400px")
  shiny::div(class  = "scroll-area",
             style  = paste0("max-height:", height, ";"),
             ...)
