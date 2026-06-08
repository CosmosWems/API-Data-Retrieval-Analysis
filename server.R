# ================================================================
#  server.R  –  OpenAQ Explorer
# ================================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    connected     = FALSE,
    api_key       = NULL,
    countries     = NULL,
    locations     = NULL,
    sel_location  = NULL,
    sensors_raw   = NULL,
    sensors_flat  = NULL,
    fetched_data  = NULL,
    fetch_meta    = NULL,
    analysis_data = NULL
  )

  # Initialise status UI immediately so it always exists
  output$fetch_status_ui <- renderUI(NULL)

  # ============================================================
  #  CONNECTION
  # ============================================================
  output$conn_status_ui <- renderUI({
    if (rv$connected)
      div(class = "conn-badge conn-ok",
          icon("check-circle"), " Connected")
    else
      div(class = "conn-badge conn-fail",
          icon("times-circle"), " Not connected")
  })

  observeEvent(input$btn_connect, {
    key <- trimws(input$api_key)
    if (nchar(key) < 10) {
      showNotification("\u274c API key too short.", type = "error")
      return()
    }
    output$conn_status_ui <- renderUI(
      div(class = "conn-badge conn-testing",
          icon("spinner"), " Testing \u2026"))
    res <- test_api_key(key)
    if (res$ok) {
      rv$api_key   <- key
      rv$connected <- TRUE
      tryCatch({ rv$countries <- get_countries(key) }, error = function(e) NULL)
      showNotification("\u2705 Connected to OpenAQ!", type = "message", duration = 4)
    } else {
      rv$connected <- FALSE
      showNotification(paste("\u274c", res$msg), type = "error", duration = 8)
    }
  })

  observeEvent(input$btn_disconnect, {
    rv$connected <- FALSE
    rv$api_key   <- NULL
    showNotification("Disconnected.", type = "warning", duration = 3)
  })

  # ============================================================
  #  EXPLORER – UI helpers
  # ============================================================
  output$country_select_ui <- renderUI({
    if (is.null(rv$countries) || nrow(rv$countries) == 0)
      return(tags$p("Connect first to load countries.", style = "color:#999;"))
    ch <- setNames(as.character(rv$countries$id),
                   paste0(rv$countries$name, " (", rv$countries$code, ")"))
    selectizeInput("sel_country", "Country:", choices = ch, selected = NULL,
                   options = list(placeholder = "Type to search\u2026"))
  })

  output$sn_param_ui <- renderUI({
    req(rv$connected)
    ch <- c("All" = "",
            setNames(as.character(OPENAQ_PARAMS$id),
                     paste0(OPENAQ_PARAMS$label,
                            " (ID ", OPENAQ_PARAMS$id, ")")))
    selectInput("sn_param", "Filter by parameter (optional):",
                choices = ch, selected = "")
  })

  output$coord_display_ui <- renderUI({
    lat <- suppressWarnings(as.numeric(input$sc_lat))
    lon <- suppressWarnings(as.numeric(input$sc_lon))
    tagList(
      tags$span(paste0("Lat: ", round(lat, 4))),
      " \u00b7 ",
      tags$span(paste0("Lon: ", round(lon, 4)))
    )
  })

  # ============================================================
  #  MAP
  # ============================================================
  output$main_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 15, lat = 10, zoom = 2) %>%
      addDrawToolbar(
        rectangleOptions    = drawRectangleOptions(repeatMode = FALSE),
        polylineOptions     = FALSE,
        polygonOptions      = FALSE,
        circleOptions       = FALSE,
        markerOptions       = FALSE,
        circleMarkerOptions = FALSE,
        editOptions         = editToolbarOptions(selectedPathOptions = FALSE)
      ) %>%
      addEasyButton(easyButton(
        icon    = "fa-globe", title = "Reset view",
        onClick = JS("function(b,m){m.setView([10,15],2);}")
      ))
  })

  observeEvent(input$map_tile, {
    leafletProxy("main_map") %>%
      clearTiles() %>%
      addProviderTiles(input$map_tile,
                       options = providerTileOptions(noWrap = TRUE))
  })

  observeEvent(input$main_map_click, {
    cl  <- input$main_map_click
    lat <- suppressWarnings(as.numeric(cl$lat))
    lng <- suppressWarnings(as.numeric(cl$lng))
    if (!is.finite(lat) || !is.finite(lng)) return()
    updateNumericInput(session, "sc_lat", value = round(lat, 5))
    updateNumericInput(session, "sc_lon", value = round(lng, 5))
    leafletProxy("main_map") %>%
      clearGroup("click_pt") %>%
      addCircleMarkers(lng, lat, radius = 10,
                       color = "#e74c3c", fillOpacity = .9,
                       group = "click_pt",
                       popup = paste0("<b>Clicked</b><br>",
                                      "Lat: ", round(lat,5), "<br>",
                                      "Lon: ", round(lng,5)))
  })

  observeEvent(input$main_map_draw_new_feature, {
    feat <- input$main_map_draw_new_feature
    tryCatch({
      coords <- feat$geometry$coordinates[[1]]
      lons   <- suppressWarnings(as.numeric(sapply(coords, `[[`, 1)))
      lats   <- suppressWarnings(as.numeric(sapply(coords, `[[`, 2)))
      lons   <- lons[is.finite(lons)]; lats <- lats[is.finite(lats)]
      if (length(lons) == 0 || length(lats) == 0) return()
      updateNumericInput(session, "bb_minlon", value = round(min(lons), 4))
      updateNumericInput(session, "bb_maxlon", value = round(max(lons), 4))
      updateNumericInput(session, "bb_minlat", value = round(min(lats), 4))
      updateNumericInput(session, "bb_maxlat", value = round(max(lats), 4))
      showNotification("\u2705 Bounding box updated from map.",
                       type = "message", duration = 3)
    }, error = function(e) NULL)
  })

  # ============================================================
  #  SEARCH  –  shared result handler
  # ============================================================
  handle_results <- function(raw) {
    if (is.null(raw) || length(raw) == 0) {
      showNotification("\u26a0\ufe0f No locations found.", type = "warning")
      return()
    }
    locs <- tryCatch(
      flatten_locations(raw),
      error = function(e) {
        showNotification(paste("\u274c Parse error:", e$message),
                         type = "error", duration = 8)
        NULL
      })
    if (is.null(locs) || nrow(locs) == 0) {
      showNotification("\u26a0\ufe0f Could not parse location results.",
                       type = "warning")
      return()
    }
    rv$locations <- locs
    showNotification(paste0("\u2705 Found ", nrow(locs), " location(s)."),
                     type = "message", duration = 3)
    update_markers(locs)
  }

  update_markers <- function(locs) {
    if (!all(c("latitude","longitude") %in% names(locs))) return()
    locs$latitude  <- suppressWarnings(as.numeric(locs$latitude))
    locs$longitude <- suppressWarnings(as.numeric(locs$longitude))
    lv <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), ,
               drop = FALSE]
    if (nrow(lv) == 0) {
      showNotification(
        "\u26a0\ufe0f Locations found but none have valid coordinates.",
        type = "warning")
      return()
    }
    popups <- tryCatch(
      vapply(seq_len(nrow(lv)),
             function(i) build_popup(lv[i, , drop = FALSE]), character(1)),
      error = function(e) rep("", nrow(lv)))
    proxy  <- leafletProxy("main_map") %>% clearGroup("stations")
    if (isTRUE(input$map_cluster)) {
      proxy %>% addCircleMarkers(
        data = lv, lng = ~longitude, lat = ~latitude,
        layerId = ~as.character(id), radius = 8, weight = 2,
        color = "#154360", fillColor = "#2471a3", fillOpacity = .85,
        group = "stations", popup = popups,
        clusterOptions = markerClusterOptions())
    } else {
      proxy %>% addCircleMarkers(
        data = lv, lng = ~longitude, lat = ~latitude,
        layerId = ~as.character(id), radius = 8, weight = 2,
        color = "#154360", fillColor = "#2471a3", fillOpacity = .85,
        group = "stations", popup = popups)
    }
    if (nrow(lv) > 1)
      leafletProxy("main_map") %>%
        fitBounds(min(lv$longitude), min(lv$latitude),
                  max(lv$longitude), max(lv$latitude))
    else
      leafletProxy("main_map") %>%
        setView(lv$longitude[1], lv$latitude[1], zoom = 12)
  }

  observeEvent(input$btn_search_name, {
    req(rv$connected, nchar(trimws(input$sn_city)) > 0)
    withProgress(message = "Searching\u2026", value = 0, {
      tryCatch({
        handle_results(
          search_locations_by_name(
            trimws(input$sn_city),
            limit    = as.integer(input$sn_limit),
            param_id = if (nchar(input$sn_param %||% "") > 0)
                         input$sn_param else NULL,
            api_key  = rv$api_key))
      }, error = function(e)
        showNotification(paste("\u274c", e$message),
                         type = "error", duration = 8))
    })
  })

  observeEvent(input$btn_search_coord, {
    req(rv$connected)
    lat    <- suppressWarnings(as.numeric(input$sc_lat))
    lon    <- suppressWarnings(as.numeric(input$sc_lon))
    radius <- suppressWarnings(as.numeric(input$sc_radius))
    if (!is.finite(lat) || !is.finite(lon) || !is.finite(radius)) {
      showNotification("\u274c Invalid coordinates or radius.", type = "error")
      return()
    }
    withProgress(message = "Searching nearby\u2026", value = 0, {
      tryCatch({
        handle_results(
          search_locations_by_coords(lat, lon,
                                     radius_km = radius,
                                     api_key   = rv$api_key))
      }, error = function(e)
        showNotification(paste("\u274c", e$message),
                         type = "error", duration = 8))
    })
  })

  observeEvent(input$btn_search_bbox, {
    req(rv$connected)
    minlon <- suppressWarnings(as.numeric(input$bb_minlon))
    minlat <- suppressWarnings(as.numeric(input$bb_minlat))
    maxlon <- suppressWarnings(as.numeric(input$bb_maxlon))
    maxlat <- suppressWarnings(as.numeric(input$bb_maxlat))
    vals   <- c(minlon, minlat, maxlon, maxlat)
    if (any(!is.finite(vals))) {
      showNotification("\u274c All bounding box values must be valid numbers.",
                       type = "error"); return()
    }
    if (isTRUE(minlon >= maxlon)) {
      showNotification("\u274c Min Lon must be less than Max Lon.",
                       type = "error"); return()
    }
    if (isTRUE(minlat >= maxlat)) {
      showNotification("\u274c Min Lat must be less than Max Lat.",
                       type = "error"); return()
    }
    withProgress(message = "Searching bounding box\u2026", value = 0, {
      tryCatch({
        handle_results(
          search_locations_by_bbox(minlon, minlat, maxlon, maxlat,
                                   api_key = rv$api_key))
      }, error = function(e)
        showNotification(paste("\u274c", e$message),
                         type = "error", duration = 8))
    })
  })

  observeEvent(input$btn_search_country, {
    req(rv$connected,
        !is.null(input$sel_country),
        nchar(as.character(input$sel_country %||% "")) > 0)
    withProgress(message = "Searching country\u2026", value = 0, {
      tryCatch({
        handle_results(
          search_locations_by_country(input$sel_country,
                                      api_key = rv$api_key))
      }, error = function(e)
        showNotification(paste("\u274c", e$message),
                         type = "error", duration = 8))
    })
  })

  # ============================================================
  #  SELECT LOCATION
  # ============================================================
  select_location <- function(loc_id) {
    req(!is.null(rv$locations))
    loc_id <- suppressWarnings(as.integer(loc_id))
    if (!is.finite(loc_id)) return()
    row <- rv$locations[
      suppressWarnings(as.integer(rv$locations$id)) == loc_id, ,
      drop = FALSE]
    if (nrow(row) == 0) return()
    rv$sel_location <- row[1, , drop = FALSE]

    sel_lon <- suppressWarnings(as.numeric(row$longitude[1]))
    sel_lat <- suppressWarnings(as.numeric(row$latitude[1]))
    if (is.finite(sel_lon) && is.finite(sel_lat))
      leafletProxy("main_map") %>%
        clearGroup("sel_pt") %>%
        addCircleMarkers(sel_lon, sel_lat, radius = 15, weight = 3,
                         color = "#27ae60", fillColor = "#27ae60",
                         fillOpacity = .4, group = "sel_pt")

    withProgress(message = "Loading sensors\u2026", value = 0, {
      tryCatch({
        sraw            <- get_location_sensors(loc_id, rv$api_key)
        rv$sensors_raw  <- sraw
        rv$sensors_flat <- if (!is.null(sraw) && is.data.frame(sraw) &&
                                nrow(sraw) > 0)
                             flatten_sensors(sraw)
                           else NULL
        showNotification(
          paste0("\U0001f4cd Selected: ",
                 row$name[1] %||% paste0("ID:", loc_id)),
          type = "message", duration = 4)
        updateTabItems(session, "main_menu", "fetch")
      }, error = function(e)
        showNotification(paste("\u274c Sensors:", e$message),
                         type = "error", duration = 8))
    })
  }

  observeEvent(input$main_map_marker_click, {
    id <- input$main_map_marker_click$id
    if (!is.null(id)) select_location(id)
  })
  observeEvent(input$card_click_id,  select_location(input$card_click_id))
  observeEvent(input$loc_table_rows_selected, {
    req(rv$locations)
    idx <- input$loc_table_rows_selected
    if (length(idx) == 1) select_location(rv$locations$id[idx])
  })

  # ============================================================
  #  LOCATION CARDS & TABLE
  # ============================================================
  output$loc_cards <- renderUI({
    if (is.null(rv$locations) || nrow(rv$locations) == 0)
      return(div(style = "color:#999;padding:18px;text-align:center;",
                 icon("times-circle"), br(), br(),
                 "No results yet. Use search above."))
    locs   <- rv$locations
    n      <- min(nrow(locs), 30)
    sel_id <- if (!is.null(rv$sel_location))
                suppressWarnings(as.integer(rv$sel_location$id))
              else -1L
    tagList(
      div(style = "padding:5px 4px 2px;",
          tags$small(paste0("Showing ", n, " of ", nrow(locs)))),
      lapply(seq_len(n), function(i) {
        l   <- locs[i, , drop = FALSE]
        sel <- identical(suppressWarnings(as.integer(l$id)), sel_id)
        div(class = paste0("loc-card", if (sel) " selected" else ""),
          onclick = paste0("Shiny.setInputValue('card_click_id',",
                           l$id, ",{priority:'event'})"),
          if (!is.null(l$sensorsCount) && !is.na(l$sensorsCount))
            div(class = "loc-card-badge",
                icon("broadcast-tower"), " ", l$sensorsCount),
          div(class = "loc-card-title",
              l$name %||% paste0("ID:", l$id)),
          div(class = "loc-card-sub",
              if ("locality"     %in% names(l) && !is.na(l$locality))
                paste0("\U0001f4cd ", l$locality, "  ") else "",
              if ("country_name" %in% names(l) && !is.na(l$country_name))
                paste0("\U0001f30d ", l$country_name) else ""),
          div(class = "loc-card-meta",
              paste0("ID: ", l$id),
              if ("lastUpdated" %in% names(l) && !is.na(l$lastUpdated))
                paste0("  \u00b7  ",
                       substr(as.character(l$lastUpdated), 1, 10))
              else "")
        )
      })
    )
  })

  output$loc_table <- renderDT({
    req(rv$locations)
    datatable(rv$locations, selection = "single", rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })

  # ============================================================
  #  FETCH DATA – selected location & latest readings
  # ============================================================
  output$sel_loc_ui <- renderUI({
    if (is.null(rv$sel_location))
      return(status_html(
        "No location selected.<br>Go to Location Explorer \u2192 click a marker.",
        "neutral"))
    l   <- rv$sel_location
    lat <- suppressWarnings(as.numeric(l$latitude))
    lon <- suppressWarnings(as.numeric(l$longitude))
    tagList(
      tags$h4(l$name %||% paste0("ID:", l$id),
              style = "color:#154360;font-weight:700;margin-bottom:4px;"),
      if (!is.null(l$locality)     && !is.na(l$locality))
        tags$p(icon("map-marker-alt"), " ", l$locality) else NULL,
      if (!is.null(l$country_name) && !is.na(l$country_name))
        tags$p(icon("globe"), " ", l$country_name) else NULL,
      tags$code(paste0("Location ID: ", l$id)),
      if (is.finite(lat) && is.finite(lon))
        tags$p(style = "font-size:11px;color:#888;margin-top:4px;",
               paste0("\U0001f4cd ", round(lat,5), ", ", round(lon,5)))
      else NULL
    )
  })

  output$latest_ui <- renderUI({
    sf <- rv$sensors_flat
    if (is.null(sf) || !"latest_value" %in% names(sf)) return(NULL)
    rows <- sf[!is.na(sf$latest_value), , drop = FALSE]
    if (nrow(rows) == 0) return(NULL)
    tagList(
      tags$b("Latest readings:"), br(), br(),
      lapply(seq_len(min(nrow(rows), 6)), function(i) {
        r <- rows[i, , drop = FALSE]
        div(style = "display:flex;justify-content:space-between;margin-bottom:4px;",
          span(class = "param-chip-sm", r$param_name %||% "?"),
          tags$b(paste0(round(as.numeric(r$latest_value), 2),
                        " ", r$param_units %||% "")))
      })
    )
  })

  # ============================================================
  #  SENSORS CHECKBOX UI
  # ============================================================
  output$sensors_chk_ui <- renderUI({
    # Require a selected location; sensors_flat may not exist yet
    if (is.null(rv$sel_location))
      return(status_html(
        "Select a location in the Explorer tab first.", "neutral"))

    sf <- rv$sensors_flat

    # If sensors_flat is empty try to build it from sensors_raw
    if (is.null(sf) || nrow(sf) == 0) {
      if (!is.null(rv$sensors_raw) && is.data.frame(rv$sensors_raw) &&
          nrow(rv$sensors_raw) > 0) {
        sf <- tryCatch(flatten_sensors(rv$sensors_raw), error = function(e) NULL)
      }
    }

    if (is.null(sf) || nrow(sf) == 0)
      return(status_html(
        paste0("\u26a0\ufe0f No sensors found for this location.<br>",
               "Try a different station or check the API."), "warn"))

    choices <- sensors_to_choices(sf)
    if (length(choices) == 0)
      return(status_html("Sensor list is empty.", "warn"))

    tagList(
      div(style = "display:flex;gap:6px;margin-bottom:10px;",
        actionButton("chk_all",  "All",  class = "btn-default btn-xs btn-sm"),
        actionButton("chk_none", "None", class = "btn-default btn-xs btn-sm")
      ),
      checkboxGroupInput(
        inputId  = "sel_sensors",
        label    = NULL,
        choices  = choices,
        selected = names(choices)   # pre-select all sensors
      )
    )
  })

  observeEvent(input$chk_all, {
    sf <- rv$sensors_flat
    if (is.null(sf)) return()
    updateCheckboxGroupInput(session, "sel_sensors",
                             selected = as.character(sf$id))
  })
  observeEvent(input$chk_none,
    updateCheckboxGroupInput(session, "sel_sensors",
                             selected = character(0)))

  output$sensor_info_ui <- renderUI({
    sf <- rv$sensors_flat
    if (is.null(sf)) return(NULL)
    n  <- nrow(sf)
    ns <- length(input$sel_sensors %||% character(0))
    status_html(paste0(n, " sensor", if (n != 1) "s" else "",
                       " available.  ", ns, " selected."), "ok")
  })

  # ============================================================
  #  FETCH
  # ============================================================
  observeEvent(input$btn_fetch, {
    req(rv$connected, rv$sel_location, length(input$sel_sensors) > 0)
    output$fetch_status_ui <- renderUI(
      status_html("\U0001f504 Fetching\u2026", "warn"))

    withProgress(message = "Fetching data\u2026", value = 0, {
      sids        <- suppressWarnings(as.integer(input$sel_sensors))
      sids        <- sids[is.finite(sids)]
      if (length(sids) == 0) {
        output$fetch_status_ui <- renderUI(
          status_html("\u274c No valid sensor IDs selected.", "error"))
        return()
      }

      dfrom       <- as.POSIXct(input$date_range[1], tz = "UTC")
      dto         <- as.POSIXct(input$date_range[2], tz = "UTC") + 86399L
      sf          <- rv$sensors_flat
      sensor_list <- list()
      meta_rows   <- list()

      for (i in seq_along(sids)) {
        sid <- sids[i]
        incProgress(1 / length(sids),
                    message = paste0("Sensor ", sid,
                                     "  (", i, "/", length(sids), ")"))

        # Get the human-readable parameter name for this sensor
        pname <- tryCatch({
          if (!is.null(sf) && "id" %in% names(sf)) {
            r <- sf[suppressWarnings(as.integer(sf$id)) == sid, ,
                    drop = FALSE]
            if (nrow(r) > 0) r$param_name[1] %||% paste0("s", sid)
            else paste0("s", sid)
          } else paste0("s", sid)
        }, error = function(e) paste0("s", sid))
        pname <- make.names(pname)

        tryCatch({
          meas <- fetch_measurements(
            sensor_id = sid,
            date_from = dfrom,
            date_to   = dto,
            agg       = input$fetch_agg,
            api_key   = rv$api_key,
            limit     = as.integer(input$fetch_limit)
          )
          # Guard: NULL, list(), or 0-row df all skip
          if (!is.null(meas) && is.data.frame(meas) && nrow(meas) > 0) {
            sensor_list[[pname]] <- meas
            meta_rows[[pname]]   <- list(
              sensor_id = sid, param = pname,
              n         = nrow(meas),
              agg       = input$fetch_agg)
          }
        }, error = function(e)
          showNotification(paste0("Sensor ", sid, ": ", e$message),
                           type = "warning", duration = 6))
      }

      if (length(sensor_list) > 0) {
        wide            <- build_wide_df(sensor_list)
        if (!is.null(wide) && nrow(wide) > 0) {
          rv$fetched_data <- wide
          rv$fetch_meta   <- list(
            location    = rv$sel_location$name %||% "Unknown",
            location_id = rv$sel_location$id,
            date_from   = dfrom, date_to = dto,
            aggregation = input$fetch_agg,
            sensors     = meta_rows,
            fetched_at  = Sys.time())
          nr <- nrow(wide)
          nc <- length(pollutant_cols(wide))
          output$fetch_status_ui <- renderUI(
            status_html(paste0(
              "\u2705 Fetched ", nr, " rows \u00d7 ", nc,
              " parameter(s) [", input$fetch_agg, "]."), "ok"))
        } else {
          output$fetch_status_ui <- renderUI(
            status_html(
              "\u26a0\ufe0f Data was fetched but could not be assembled into a table.",
              "warn"))
        }
      } else {
        output$fetch_status_ui <- renderUI(
          status_html(
            paste0("\u26a0\ufe0f No measurements returned.<br>",
                   "Try a wider date range, different aggregation, ",
                   "or check the sensor is still active."), "warn"))
      }
    })
  })

  # ---- Summary & table -----------------------------------------
  output$fetch_summary_ui <- renderUI({
    if (is.null(rv$fetched_data))
      return(status_html("No data yet.", "neutral"))
    df <- rv$fetched_data
    pc <- pollutant_cols(df)
    m  <- rv$fetch_meta
    tagList(
      fluidRow(
        valueBox(nrow(df),   "Rows",       icon = icon("database"),
                 color = "blue",  width = 6),
        valueBox(length(pc), "Parameters", icon = icon("flask"),
                 color = "green", width = 6)
      ),
      if (!is.null(m)) tagList(
        tags$p(tags$b("Location: "),    m$location),
        tags$p(tags$b("Aggregation: "), m$aggregation),
        tags$p(tags$b("Dates: "),
               as.character(as.Date(m$date_from)), " \u2192 ",
               as.character(as.Date(m$date_to))),
        tags$p(tags$b("Fetched: "),
               format(m$fetched_at, "%Y-%m-%d %H:%M"))
      ),
      hr(),
      tags$b("Parameters:"), br(), br(),
      div(lapply(pc, function(p) span(class = "param-chip-sm", p)))
    )
  })

  output$completeness_ui <- renderUI({
    if (is.null(rv$fetched_data)) return(NULL)
    lapply(completeness_summary(rv$fetched_data), function(x)
      div(style = "margin-bottom:7px;",
        div(style = "display:flex;justify-content:space-between;margin-bottom:2px;",
          tags$small(tags$b(x$param)),
          tags$small(paste0(x$n, " values \u00b7 ", x$pct, "%"))),
        div(class = "progress",
          div(class = "progress-bar",
              style = paste0("width:", x$pct, "%;background:", x$col, ";"),
              paste0(x$pct, "%")))
      )
    )
  })

  output$data_preview_tbl <- renderDT({
    req(rv$fetched_data)
    df       <- rv$fetched_data
    df$date  <- format(df$date, "%Y-%m-%d %H:%M")
    num_cols <- names(df)[sapply(df, is.numeric)]
    datatable(df, rownames = FALSE,
              options = list(pageLength = 12, scrollX = TRUE,
                             order = list(list(0, "desc")))) %>%
      formatRound(num_cols, digits = 3)
  })

  # ---- Downloads -----------------------------------------------
  .dl_df <- function() rv$fetched_data %||% rv$analysis_data

  .write_csv  <- function(f) { req(.dl_df()); write.csv(.dl_df(), f, row.names = FALSE) }
  .write_rds  <- function(f) { req(.dl_df()); saveRDS(.dl_df(), f) }
  .write_xlsx <- function(f) {
    req(.dl_df())
    if (requireNamespace("openxlsx", quietly = TRUE))
      openxlsx::write.xlsx(.dl_df(), f)
    else {
      write.csv(.dl_df(), f, row.names = FALSE)
      showNotification("openxlsx not installed; saved as CSV.", type = "warning")
    }
  }
  .write_zip <- function(f) {
    req(.dl_df())
    td  <- tempdir()
    csv <- file.path(td, "openaq_data.csv")
    rds <- file.path(td, "openaq_data.rds")
    write.csv(.dl_df(), csv, row.names = FALSE)
    saveRDS(.dl_df(), rds)
    zip::zip(f, c(csv, rds), mode = "cherry-pick")
  }

  output$dl_csv  <- downloadHandler(filename = function() paste0("openaq_",Sys.Date(),".csv"),  content = .write_csv)
  output$dl_rds  <- downloadHandler(filename = function() paste0("openaq_",Sys.Date(),".rds"),  content = .write_rds)
  output$dl_xlsx <- downloadHandler(filename = function() paste0("openaq_",Sys.Date(),".xlsx"), content = .write_xlsx)
  output$dl_zip  <- downloadHandler(filename = function() paste0("openaq_",Sys.Date(),".zip"),  content = .write_zip)

  output$exp_csv  <- downloadHandler(filename = function() paste0("openaq_",Sys.Date(),".csv"),  content = .write_csv)
  output$exp_rds  <- downloadHandler(filename = function() paste0("openaq_",Sys.Date(),".rds"),  content = .write_rds)
  output$exp_xlsx <- downloadHandler(filename = function() paste0("openaq_",Sys.Date(),".xlsx"), content = .write_xlsx)
  output$exp_zip  <- downloadHandler(filename = function() paste0("openaq_",Sys.Date(),".zip"),  content = .write_zip)

  output$exp_loc <- downloadHandler(
    filename = function() paste0("locations_", Sys.Date(), ".csv"),
    content  = function(f) { req(rv$locations); write.csv(rv$locations, f, row.names = FALSE) })
  output$exp_sensors <- downloadHandler(
    filename = function() paste0("sensors_", Sys.Date(), ".csv"),
    content  = function(f) {
      df <- rv$sensors_flat %||% rv$sensors_raw
      req(df)
      write.csv(df, f, row.names = FALSE)
    })
  output$exp_meta <- downloadHandler(
    filename = function() paste0("fetch_meta_", Sys.Date(), ".txt"),
    content  = function(f) {
      req(rv$fetch_meta)
      writeLines(capture.output(str(rv$fetch_meta)), f)
    })

  # ============================================================
  #  ANALYSIS
  # ============================================================
  upload_df <- reactive({
    req(input$upload_csv)
    tryCatch(
      read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE),
      error = function(e) { showNotification(e$message, type = "error"); NULL }
    )
  })

  output$map_date_ui <- renderUI({
    req(upload_df())
    selectInput("col_date", "Date/time column:", names(upload_df()))
  })
  output$map_ws_ui <- renderUI({
    req(upload_df())
    selectInput("col_ws", "Wind speed col (ws):",
                c("(none)" = "", names(upload_df())), selected = "")
  })
  output$map_wd_ui <- renderUI({
    req(upload_df())
    selectInput("col_wd", "Wind direction col (wd):",
                c("(none)" = "", names(upload_df())), selected = "")
  })

  output$analysis_src_status_ui <- renderUI({
    if (input$data_src == "fetched") {
      if (is.null(rv$fetched_data))
        status_html("No fetched data. Go to Fetch Data first.", "warn")
      else {
        df <- rv$fetched_data
        status_html(paste0("\u2705 ", nrow(df), " rows \u00d7 ",
                           length(pollutant_cols(df)), " parameter(s) ready."),
                    "ok")
      }
    } else {
      if (is.null(input$upload_csv))
        status_html("Upload a CSV file on the left.", "neutral")
      else
        status_html("\u2705 File loaded. Check mapping, then Prepare.", "ok")
    }
  })

  output$data_info_ui <- renderUI({
    if (is.null(rv$analysis_data)) return(NULL)
    tagList(hr(),
      tags$small(icon("check-circle", style = "color:#1d8348;"),
                 " ", nrow(rv$analysis_data), " rows ready"))
  })

  observeEvent(input$btn_prepare, {
    tryCatch({
      if (input$data_src == "fetched") {
        req(rv$fetched_data)
        rv$analysis_data <- rv$fetched_data
        showNotification("\u2705 OpenAQ data prepared!",
                         type = "message", duration = 4)
      } else {
        req(upload_df(), input$col_date)
        rv$analysis_data <- prepare_for_openair(
          upload_df(),
          date_col = input$col_date,
          ws_col   = if (nchar(input$col_ws %||% "") > 0) input$col_ws else NULL,
          wd_col   = if (nchar(input$col_wd %||% "") > 0) input$col_wd else NULL,
          date_fmt = input$date_fmt %||% "auto")
        showNotification("\u2705 CSV prepared!",
                         type = "message", duration = 4)
      }
    }, error = function(e)
      showNotification(paste("\u274c", e$message),
                       type = "error", duration = 10))
  })

  # ---- Dynamic plot controls -----------------------------------
  output$dyn_controls_ui <- renderUI({
    req(nchar(input$plot_type) > 0)
    df <- rv$analysis_data
    if (is.null(df))
      return(status_html(
        "Click \u2699\ufe0f Prepare Data first.", "neutral"))
    pc       <- pollutant_cols(df)
    wind     <- has_wind(df)
    all_cols <- setdiff(names(df), "date")
    no_wind  <- status_html(
      "<b>\u26a0\ufe0f</b> Wind columns (ws/wd) not detected.", "warn")

    switch(input$plot_type,
      summaryPlot    = tagList(
        selectInput("sp_avg", "Averaging:", AVG_TIMES, "day"),
        checkboxInput("sp_ok", "Period-ok bars", TRUE)),
      timePlot       = tagList(
        checkboxGroupInput("tp_poll", "Variables:", choices = pc,
                           selected = pc[seq_len(min(3, length(pc)))]),
        selectInput("tp_avg",  "Averaging:",  AVG_TIMES, "day"),
        selectInput("tp_yrel", "Y-axis:",
                    c("Free" = "free", "Fixed" = "same"), "free"),
        selectInput("tp_type", "Facet by:", FACET_TYPES, "default"),
        checkboxInput("tp_smooth", "Smooth line", FALSE)),
      timeVariation  = tagList(
        checkboxGroupInput("tv_poll", "Pollutant(s):", choices = pc,
                           selected = pc[seq_len(min(2, length(pc)))]),
        checkboxInput("tv_norm", "Normalise (0-1)", FALSE),
        selectInput("tv_type", "Facet by:", FACET_TYPES, "default")),
      calendarPlot   = tagList(
        selectInput("cp_poll", "Pollutant:", pc, pc[1]),
        numericInput("cp_year", "Year:",
                     as.integer(format(Sys.Date(), "%Y")),
                     2000, as.integer(format(Sys.Date(), "%Y"))),
        selectInput("cp_stat", "Statistic:",
                    c("mean","max","min","median","frequency"), "mean"),
        selectInput("cp_ann",  "Annotation:",
                    c("Date" = "date","Value" = "value",
                      "Wind dir" = "wd","None" = ""), "date"),
        selectInput("cp_col",  "Palette:", OPENAIR_PALETTES, "RdYlGn"),
        checkboxInput("cp_who", "WHO/AQI breaks (PM2.5/PM10)", FALSE),
        conditionalPanel("!input.cp_who",
          checkboxInput("cp_cust", "Custom breaks", FALSE),
          conditionalPanel("input.cp_cust",
            textInput("cp_brk", "Breaks:", "0,12,25,50,100,150,300"),
            textInput("cp_lbl", "Labels:",
                      "Good,Moderate,Elevated,High,Very High,Hazardous")))),
      windRose       = if (!wind) no_wind else tagList(
        selectInput("wr_type", "Facet by:", FACET_TYPES, "default"),
        sliderInput("wr_int",  "Speed interval (m/s):", 0.5, 10, 2, 0.5),
        selectInput("wr_col",  "Palette:", OPENAIR_PALETTES, "YlOrRd")),
      pollutionRose  = if (!wind) no_wind else tagList(
        selectInput("pr_poll", "Pollutant:", pc, pc[1]),
        selectInput("pr_type", "Facet by:", FACET_TYPES, "default"),
        checkboxInput("pr_norm", "Normalise", FALSE),
        sliderInput("pr_seg",  "Segment:", 0.1, 1, .9, .1),
        selectInput("pr_col",  "Palette:", OPENAIR_PALETTES, "YlOrRd")),
      percentileRose = if (!wind) no_wind else tagList(
        selectInput("percr_poll", "Pollutant:", pc, pc[1]),
        selectInput("percr_type", "Facet by:", FACET_TYPES, "default"),
        checkboxInput("percr_sm", "Smooth", FALSE)),
      polarPlot      = if (!wind) no_wind else tagList(
        selectInput("pp_poll", "Pollutant:", pc, pc[1]),
        selectInput("pp_x",    "Radial var:", all_cols, "ws"),
        selectInput("pp_stat", "Statistic:",
                    c("mean","median","max","stdev","weighted.mean"), "mean"),
        selectInput("pp_type", "Facet by:", FACET_TYPES, "default"),
        selectInput("pp_col",  "Palette:", OPENAIR_PALETTES, "heat"),
        sliderInput("pp_res",  "Resolution:", 50, 200, 100, 10)),
      polarFreq      = if (!wind) no_wind else tagList(
        selectInput("pf_poll", "Pollutant (opt.):",
                    c("Frequency only" = "", pc),
                    if (length(pc) > 0) pc[1] else ""),
        selectInput("pf_type", "Facet by:", FACET_TYPES, "month"),
        sliderInput("pf_int",  "Speed interval:", 5, 100, 30, 5),
        selectInput("pf_stat", "Statistic:",
                    c("frequency","mean","median","weighted.mean"), "frequency"),
        sliderInput("pf_off",  "Offset:", 0, 100, 80, 5),
        selectInput("pf_col",  "Palette:", OPENAIR_PALETTES, "heat")),
      polarAnnulus   = if (!wind) no_wind else tagList(
        selectInput("pa_poll", "Pollutant:", pc, pc[1]),
        selectInput("pa_per",  "Period:",
                    c("hour","month","season","weekday","year"), "hour"),
        selectInput("pa_stat", "Statistic:",
                    c("mean","median","max","stdev","weighted.mean"), "mean"),
        selectInput("pa_col",  "Palette:", OPENAIR_PALETTES, "heat")),
      smoothTrend    = tagList(
        checkboxGroupInput("st_poll", "Pollutant(s):", choices = pc,
                           selected = pc[seq_len(min(2, length(pc)))]),
        selectInput("st_avg",  "Averaging:", AVG_TIMES, "month"),
        sliderInput("st_ci",   "CI (%):", 50, 99, 95, 1),
        selectInput("st_type", "Facet by:", FACET_TYPES, "default")),
      TheilSen       = tagList(
        selectInput("ts_poll", "Pollutant:", pc, pc[1]),
        selectInput("ts_avg",  "Averaging:", AVG_TIMES, "month"),
        selectInput("ts_type", "Facet by:", FACET_TYPES, "default"),
        checkboxInput("ts_des", "Remove seasonality", FALSE)),
      trendLevel     = tagList(
        selectInput("tl_poll", "Pollutant:", pc, pc[1]),
        selectInput("tl_x",    "X-axis:",
                    c("month","hour","weekday","season"), "month"),
        selectInput("tl_y",    "Y-axis:",
                    c("hour","month","weekday"), "hour"),
        selectInput("tl_stat", "Statistic:",
                    c("mean","median","max","stdev"), "mean"),
        selectInput("tl_col",  "Palette:", OPENAIR_PALETTES, "heat")),
      scatterPlot    = tagList(
        selectInput("sc_x",      "X variable:", all_cols, all_cols[1]),
        selectInput("sc_y",      "Y variable:", all_cols,
                    if (length(all_cols) > 1) all_cols[2] else all_cols[1]),
        selectInput("sc_method", "Fit:",
                    c("default","lm","loess","spline"), "default"),
        selectInput("sc_type",   "Facet by:", FACET_TYPES, "default"),
        selectInput("sc_grp",    "Group by:",
                    c("(none)" = "", FACET_TYPES[-1]), "")),
      modStats       = status_html(
        "Configure in the Model Evaluation panel below.", "neutral"),
      tags$p("Select a plot type above.", style = "color:#999;")
    )
  })

  .plot_desc_content <- function() {
    info <- PLOT_CATALOGUE[[input$plot_type]]
    if (is.null(info)) return(NULL)
    div(class = "plot-info-bar", style = "margin-top:6px;font-size:12.5px;",
        tags$b(info$label), ": ", info$desc,
        if (isTRUE(info$needs_wind))
          tags$span(style = "color:#b7770d;margin-left:8px;",
                    icon("wind"), " needs ws & wd"))
  }
  output$plot_desc_ui <- renderUI(.plot_desc_content())
  output$plot_info_ui <- renderUI(.plot_desc_content())

  .build_params <- function() {
    pt <- input$plot_type
    switch(pt,
      summaryPlot    = list(avg_time  = input$sp_avg,
                            period_ok = isTRUE(input$sp_ok)),
      timePlot       = list(pollutant  = input$tp_poll %||% character(0),
                            avg_time   = input$tp_avg,
                            y_relation = input$tp_yrel,
                            type       = input$tp_type,
                            smooth     = input$tp_smooth),
      timeVariation  = list(pollutant = input$tv_poll %||% character(0),
                            normalise = input$tv_norm,
                            type      = input$tv_type),
      calendarPlot   = {
        a <- list(pollutant = input$cp_poll, year = input$cp_year,
                  statistic = input$cp_stat, annotate = input$cp_ann,
                  cols      = input$cp_col)
        if (isTRUE(input$cp_who)) {
          gl <- WHO_BREAKS[[tolower(input$cp_poll)]]
          if (!is.null(gl)) {
            a$custom_breaks <- TRUE; a$breaks <- gl$breaks
            a$labels <- gl$labels; a$break_cols <- gl$cols
          }
        } else if (isTRUE(input$cp_cust)) {
          brks <- suppressWarnings(
            as.numeric(trimws(strsplit(input$cp_brk, ",")[[1]])))
          lbls <- trimws(strsplit(input$cp_lbl, ",")[[1]])
          ni   <- length(brks) - 1
          pal  <- tryCatch(
            colorRampPalette(
              RColorBrewer::brewer.pal(max(3, min(9, ni)), "RdYlGn"))(ni),
            error = function(e)
              colorRampPalette(c("green","yellow","red"))(ni))
          a$custom_breaks <- TRUE; a$breaks <- brks
          a$labels <- lbls; a$break_cols <- pal
        }
        a
      },
      windRose       = list(type = input$wr_type, ws_int = input$wr_int,
                            cols = input$wr_col),
      pollutionRose  = list(pollutant = input$pr_poll, type = input$pr_type,
                            normalise = input$pr_norm, seg = input$pr_seg,
                            cols = input$pr_col),
      percentileRose = list(pollutant = input$percr_poll,
                            type = input$percr_type, smooth = input$percr_sm),
      polarPlot      = list(pollutant = input$pp_poll, x = input$pp_x,
                            statistic = input$pp_stat, type = input$pp_type,
                            cols = input$pp_col, resolution = input$pp_res),
      polarFreq      = list(pollutant = input$pf_poll %||% "",
                            type = input$pf_type, ws_int = input$pf_int,
                            statistic = input$pf_stat, offset = input$pf_off,
                            cols = input$pf_col),
      polarAnnulus   = list(pollutant = input$pa_poll, period = input$pa_per,
                            statistic = input$pa_stat, cols = input$pa_col),
      smoothTrend    = list(pollutant = input$st_poll %||% character(0),
                            avg_time  = input$st_avg,
                            ci        = input$st_ci,
                            type      = input$st_type),
      TheilSen       = list(pollutant = input$ts_poll,
                            avg_time  = input$ts_avg,
                            type      = input$ts_type,
                            deseason  = input$ts_des),
      trendLevel     = list(pollutant = input$tl_poll, x = input$tl_x,
                            y         = input$tl_y,
                            statistic = input$tl_stat,
                            cols      = input$tl_col),
      scatterPlot    = list(x_var  = input$sc_x, y_var = input$sc_y,
                            method = input$sc_method, type = input$sc_type,
                            group  = if (nchar(input$sc_grp %||% "") > 0)
                                       input$sc_grp else NULL),
      list()
    )
  }

  make_plot <- function() {
    req(rv$analysis_data, nchar(input$plot_type) > 0)
    tryCatch(
      run_openair_plot(input$plot_type, rv$analysis_data, .build_params()),
      error = function(e) {
        showNotification(paste("Plot error:", e$message),
                         type = "error", duration = 12)
        plot(1, type = "n", ann = FALSE, axes = FALSE,
             xlim = c(0,2), ylim = c(0,2))
        text(1, 1, paste0("Plot error:\n\n", e$message),
             col = "#c0392b", cex = .9, adj = .5)
      }
    )
  }

  plot_rv <- eventReactive(input$btn_plot, make_plot())
  output$openair_plot <- renderPlot(req(plot_rv()), res = 96)

  .save_plot <- function(file, type) {
    req(rv$analysis_data, nchar(input$plot_type) > 0)
    switch(type,
      png = grDevices::png(file, width = 2200, height = 1400, res = 180),
      pdf = grDevices::pdf(file, width = 14, height = 9),
      svg = grDevices::svg(file, width = 14, height = 9))
    tryCatch(
      run_openair_plot(input$plot_type, rv$analysis_data, .build_params()),
      error = function(e) NULL)
    grDevices::dev.off()
  }
  output$dl_png <- downloadHandler(
    filename = function() paste0("openair_",input$plot_type,"_",Sys.Date(),".png"),
    content  = function(f) .save_plot(f, "png"))
  output$dl_pdf <- downloadHandler(
    filename = function() paste0("openair_",input$plot_type,"_",Sys.Date(),".pdf"),
    content  = function(f) .save_plot(f, "pdf"))
  output$dl_svg <- downloadHandler(
    filename = function() paste0("openair_",input$plot_type,"_",Sys.Date(),".svg"),
    content  = function(f) .save_plot(f, "svg"))

  # ---- Model evaluation ----------------------------------------
  output$mod_obs_ui <- renderUI({
    req(rv$analysis_data)
    selectInput("mod_obs", "Observed:", pollutant_cols(rv$analysis_data))
  })
  output$mod_mod_ui <- renderUI({
    req(rv$analysis_data)
    pc <- pollutant_cols(rv$analysis_data)
    selectInput("mod_mod", "Modelled:", pc,
                if (length(pc) > 1) pc[2] else pc[1])
  })
  output$modstats_out <- renderPrint({
    req(input$btn_modstats, rv$analysis_data, input$mod_obs, input$mod_mod)
    isolate(tryCatch(
      openair::modStats(rv$analysis_data,
                        mod  = input$mod_mod,
                        obs  = input$mod_obs,
                        type = input$mod_type %||% "default"),
      error = function(e) cat("Error:", e$message)))
  })

  # ============================================================
  #  EXPORT CARD
  # ============================================================
  output$export_card_ui <- renderUI({
    df  <- rv$fetched_data %||% rv$analysis_data
    m   <- rv$fetch_meta
    if (is.null(df)) return(status_html("No data loaded yet.", "neutral"))
    pc  <- pollutant_cols(df)
    dr1 <- if (!is.null(df$date))
             format(min(df$date, na.rm = TRUE), "%Y-%m-%d") else "?"
    dr2 <- if (!is.null(df$date))
             format(max(df$date, na.rm = TRUE), "%Y-%m-%d") else "?"
    div(style = "background:#f8fafb;border:1px solid #d5dde5;
                 border-radius:10px;padding:18px;",
      fluidRow(
        column(4, tags$p(tags$b("Location:")),
               tags$p(style = "font-size:15px;font-weight:700;color:#154360;",
                      m$location %||% "Uploaded CSV")),
        column(3, tags$p(tags$b("Rows:")),
               tags$p(style = "font-size:22px;font-weight:700;color:#2471a3;",
                      format(nrow(df), big.mark = ","))),
        column(2, tags$p(tags$b("Params:")),
               tags$p(style = "font-size:22px;font-weight:700;color:#27ae60;",
                      length(pc))),
        column(3, tags$p(tags$b("Dates:")),
               tags$p(paste0(dr1, " \u2192 ", dr2)))
      ),
      hr(),
      tags$b("Columns:"), br(), br(),
      div(lapply(pc, function(p) span(class = "param-chip", p)))
    )
  })
}
