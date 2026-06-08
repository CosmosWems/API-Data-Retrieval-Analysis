# ================================================================
#  R/plot_helpers.R  –  OpenAir plot dispatcher
# ================================================================

# ----------------------------------------------------------------
#  .oa  –  safe openair function resolver
#  Tries the openair namespace first; falls back to search path.
# ----------------------------------------------------------------
.oa <- function(fn_name) {
  fn <- tryCatch(
    get(fn_name, envir = asNamespace("openair"), inherits = FALSE),
    error = function(e) NULL
  )
  if (!is.null(fn) && is.function(fn)) return(fn)

  fn <- tryCatch(
    get(fn_name, envir = as.environment("package:openair"), inherits = FALSE),
    error = function(e) NULL
  )
  if (!is.null(fn) && is.function(fn)) return(fn)

  fn <- tryCatch(get(fn_name, inherits = TRUE), error = function(e) NULL)
  if (!is.null(fn) && is.function(fn)) return(fn)

  stop(paste0("Cannot find openair function '", fn_name,
              "'. Is openair installed and loaded?"))
}

# ----------------------------------------------------------------
#  .req_cols  –  informative error when required columns missing
# ----------------------------------------------------------------
.req_cols <- function(df, cols) {
  cols    <- cols[nchar(cols) > 0]
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0)
    stop(paste0("Column(s) not in data: ",
                paste(missing, collapse = ", "),
                ".  Available: ",
                paste(names(df), collapse = ", ")))
}

# ----------------------------------------------------------------
#  .clean_poll  –  validate and filter pollutant column vector
# ----------------------------------------------------------------
.clean_poll <- function(df, polls, allow_empty = FALSE) {
  if (is.null(polls)) polls <- character(0)
  polls <- polls[nchar(polls) > 0]
  polls <- intersect(polls, names(df))
  if (length(polls) == 0) {
    if (allow_empty) return(character(0))
    stop("None of the selected pollutant columns exist in the data.  ",
         "Available: ", paste(pollutant_cols(df), collapse = ", "))
  }
  polls
}

# ----------------------------------------------------------------
#  .summary_plot_fallback
#  Pure ggplot2 replacement used when openair's summaryPlot /
#  summaryData is unavailable or errors.
#  Panels:
#    top    – faceted time-series (one strip per variable)
#    bottom – horizontal completeness bars
# ----------------------------------------------------------------
.summary_plot_fallback <- function(df, avg_time = "day") {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for the summary plot fallback.")

  pc <- pollutant_cols(df)

  # --- temporal averaging ----------------------------------------
  .avg <- function(d, period) {
    floor_fn <- switch(period,
      min   = function(x) lubridate::floor_date(x, "minute"),
      hour  = function(x) lubridate::floor_date(x, "hour"),
      day   = function(x) as.POSIXct(as.Date(x), tz = "UTC"),
      week  = function(x) lubridate::floor_date(x, "week"),
      month = function(x) lubridate::floor_date(x, "month"),
      year  = function(x) lubridate::floor_date(x, "year"),
                function(x) as.POSIXct(as.Date(x), tz = "UTC"))
    d$period_ <- floor_fn(d$date)
    dates     <- sort(unique(d$period_))
    out       <- data.frame(date = dates)
    for (v in pc)
      out[[v]] <- as.numeric(
        tapply(d[[v]], d$period_, mean, na.rm = TRUE)[
          match(as.character(dates),
                names(tapply(d[[v]], d$period_, mean, na.rm = TRUE)))])
    out
  }
  df_avg  <- tryCatch(.avg(df, avg_time), error = function(e) df)

  # --- long form for ggplot --------------------------------------
  df_long <- tidyr::pivot_longer(
    df_avg,
    cols      = dplyr::all_of(pc),
    names_to  = "variable",
    values_to = "value")
  df_long$variable <- factor(df_long$variable, levels = pc)

  # --- completeness ----------------------------------------------
  n   <- nrow(df)
  cdf <- data.frame(
    variable = factor(pc, levels = rev(pc)),
    pct      = sapply(pc, function(v) round(sum(!is.na(df[[v]])) / n * 100)),
    stringsAsFactors = FALSE
  )
  cdf$fill_col <- with(cdf,
    ifelse(pct > 80, "#1d8348", ifelse(pct > 50, "#d4ac0d", "#922b21")))

  # --- build panels ----------------------------------------------
  base_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "#2471a3"),
      strip.text       = ggplot2::element_text(colour = "white",
                                               face   = "bold", size = 9),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face   = "bold",
                                               colour = "#154360", size = 12)
    )

  g_ts <- ggplot2::ggplot(
      df_long,
      ggplot2::aes(x = date, y = value, colour = variable)) +
    ggplot2::geom_line(linewidth = 0.45, na.rm = TRUE) +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_datetime(date_labels = "%b %Y") +
    ggplot2::scale_colour_brewer(palette = "Set1", guide = "none") +
    ggplot2::labs(
      title = paste0("Summary  \u2014  ", avg_time, " averages"),
      x = NULL, y = "Value") +
    base_theme

  g_cmp <- ggplot2::ggplot(
      cdf,
      ggplot2::aes(x = variable, y = pct, fill = fill_col)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(pct, "%")),
      hjust = -0.15, size = 3.2, fontface = "bold") +
    ggplot2::coord_flip(ylim = c(0, 115)) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      title = "Data Completeness",
      x = NULL, y = "% valid records") +
    base_theme +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  # --- combine with gridExtra or base grid -----------------------
  np <- length(pc)
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    lmat <- rbind(
      matrix(1L, nrow = max(np * 2, 4), ncol = 1),
      matrix(2L, nrow = max(2, np),     ncol = 1)
    )
    gridExtra::grid.arrange(g_ts, g_cmp, layout_matrix = lmat)
  } else {
    # Simple stacked output if gridExtra unavailable
    print(g_ts)
    print(g_cmp)
  }
  invisible(NULL)
}

# ================================================================
#  run_openair_plot  –  main dispatcher
# ================================================================
run_openair_plot <- function(pt, df, p) {

  # global pre-checks
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
    stop("No data available. Fetch data and click \u2699\ufe0f Prepare first.")
  if (!"date" %in% names(df))
    stop("Data must contain a 'date' (POSIXct) column.")
  if (!inherits(df$date, "POSIXct"))
    df$date <- suppressWarnings(as.POSIXct(df$date, tz = "UTC"))

  switch(pt,

    # ----------------------------------------------------------
    summaryPlot = {
      pc <- pollutant_cols(df)
      if (length(pc) == 0) stop("No pollutant columns found.")
      valid_pc <- pc[sapply(pc, function(x) sum(!is.na(df[[x]])) > 0)]
      if (length(valid_pc) == 0) stop("All pollutant columns are entirely NA.")
      df_sub <- df[, c("date", valid_pc), drop = FALSE]

      # Resolve: summaryPlot was the historic name; some builds
      # export it as summaryData.  Try both, fallback to ggplot2.
      fn <- tryCatch(.oa("summaryPlot"), error = function(e) NULL) %||%
            tryCatch(.oa("summaryData"), error = function(e) NULL)

      if (!is.null(fn)) {
        tryCatch({
          args <- list(mydata = df_sub, avg.time = p$avg_time %||% "day")
          if (!is.null(p$period_ok)) args$period.ok <- isTRUE(p$period_ok)
          do.call(fn, args)
        }, error = function(e) {
          message("[summaryPlot] openair error: ", e$message,
                  "  — using built-in fallback.")
          .summary_plot_fallback(df_sub, p$avg_time %||% "day")
        })
      } else {
        message("[summaryPlot] not found in this openair build ",
                "— using built-in fallback.")
        .summary_plot_fallback(df_sub, p$avg_time %||% "day")
      }
    },

    # ----------------------------------------------------------
    timePlot = {
      polls <- .clean_poll(df, p$pollutant %||% character(0))
      args  <- list(
        mydata     = df,
        pollutant  = polls,
        avg.time   = p$avg_time   %||% "day",
        y.relation = p$y_relation %||% "free",
        type       = p$type       %||% "default"
      )
      if (isTRUE(p$smooth)) args$smooth <- TRUE
      do.call(.oa("timePlot"), args)
    },

    # ----------------------------------------------------------
    timeVariation = {
      polls <- .clean_poll(df, p$pollutant %||% character(0))
      do.call(.oa("timeVariation"), list(
        mydata    = df,
        pollutant = polls,
        normalise = isTRUE(p$normalise),
        type      = p$type %||% "default"
      ))
    },

    # ----------------------------------------------------------
    calendarPlot = {
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      args <- list(
        mydata    = df,
        pollutant = poll,
        year      = p$year      %||% as.integer(format(Sys.Date(), "%Y")),
        statistic = p$statistic %||% "mean",
        cols      = p$cols      %||% "RdYlGn"
      )
      if (!is.null(p$annotate) && nchar(p$annotate) > 0)
        args$annotate <- p$annotate
      if (isTRUE(p$custom_breaks) && !is.null(p$breaks) && length(p$breaks) > 1) {
        args$breaks <- p$breaks
        if (!is.null(p$labels))     args$labels <- p$labels
        if (!is.null(p$break_cols)) args$cols   <- p$break_cols
      }
      do.call(.oa("calendarPlot"), args)
    },

    # ----------------------------------------------------------
    windRose = {
      .req_cols(df, c("ws","wd"))
      do.call(.oa("windRose"), list(
        mydata = df, ws = "ws", wd = "wd",
        type   = p$type   %||% "default",
        ws.int = p$ws_int %||% 2,
        cols   = p$cols   %||% "YlOrRd"
      ))
    },

    # ----------------------------------------------------------
    pollutionRose = {
      .req_cols(df, c("ws","wd"))
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      do.call(.oa("pollutionRose"), list(
        mydata    = df,
        pollutant = poll,
        type      = p$type      %||% "default",
        normalise = isTRUE(p$normalise),
        seg       = p$seg       %||% 0.9,
        cols      = p$cols      %||% "YlOrRd"
      ))
    },

    # ----------------------------------------------------------
    percentileRose = {
      .req_cols(df, c("ws","wd"))
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      do.call(.oa("percentileRose"), list(
        mydata    = df,
        pollutant = poll,
        type      = p$type  %||% "default",
        smooth    = isTRUE(p$smooth)
      ))
    },

    # ----------------------------------------------------------
    polarPlot = {
      .req_cols(df, c("ws","wd"))
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      do.call(.oa("polarPlot"), list(
        mydata     = df,
        pollutant  = poll,
        x          = p$x          %||% "ws",
        wd         = "wd",
        statistic  = p$statistic  %||% "mean",
        type       = p$type       %||% "default",
        cols       = p$cols       %||% "heat",
        resolution = p$resolution %||% 100
      ))
    },

    # ----------------------------------------------------------
    polarFreq = {
      .req_cols(df, c("ws","wd"))
      args <- list(
        mydata    = df,
        type      = p$type      %||% "month",
        ws.int    = p$ws_int    %||% 30,
        statistic = p$statistic %||% "frequency",
        offset    = p$offset    %||% 80,
        trans     = FALSE,
        col       = p$cols      %||% "heat"
      )
      if (!is.null(p$pollutant) && nchar(p$pollutant) > 0 &&
          p$pollutant %in% names(df))
        args$pollutant <- p$pollutant
      do.call(.oa("polarFreq"), args)
    },

    # ----------------------------------------------------------
    polarAnnulus = {
      .req_cols(df, c("ws","wd"))
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      do.call(.oa("polarAnnulus"), list(
        mydata    = df,
        pollutant = poll,
        period    = p$period    %||% "hour",
        statistic = p$statistic %||% "mean",
        cols      = p$cols      %||% "heat"
      ))
    },

    # ----------------------------------------------------------
    smoothTrend = {
      polls <- .clean_poll(df, p$pollutant %||% character(0))
      do.call(.oa("smoothTrend"), list(
        mydata    = df,
        pollutant = polls,
        avg.time  = p$avg_time %||% "month",
        ci        = (p$ci      %||% 95) / 100,
        type      = p$type     %||% "default"
      ))
    },

    # ----------------------------------------------------------
    TheilSen = {
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      do.call(.oa("TheilSen"), list(
        mydata    = df,
        pollutant = poll,
        avg.time  = p$avg_time %||% "month",
        type      = p$type     %||% "default",
        deseason  = isTRUE(p$deseason)
      ))
    },

    # ----------------------------------------------------------
    trendLevel = {
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      do.call(.oa("trendLevel"), list(
        mydata    = df,
        pollutant = poll,
        x         = p$x         %||% "month",
        y         = p$y         %||% "hour",
        statistic = p$statistic %||% "mean",
        cols      = p$cols      %||% "heat"
      ))
    },

    # ----------------------------------------------------------
    scatterPlot = {
      xv <- p$x_var %||% "";  yv <- p$y_var %||% ""
      .req_cols(df, c(xv, yv))
      args <- list(
        mydata = df, x = xv, y = yv,
        method = p$method %||% "default",
        type   = p$type   %||% "default"
      )
      if (!is.null(p$group) && nchar(p$group) > 0)
        args$group <- p$group
      do.call(.oa("scatterPlot"), args)
    },

    # ----------------------------------------------------------
    modStats = {
      .req_cols(df, c(p$mod_col %||% "", p$obs_col %||% ""))
      print(do.call(.oa("modStats"), list(
        mydata = df,
        mod    = p$mod_col,
        obs    = p$obs_col,
        type   = p$type %||% "default"
      )), digits = 4)
    },

    stop(paste0("Unknown plot type: '", pt, "'"))
  )
}
