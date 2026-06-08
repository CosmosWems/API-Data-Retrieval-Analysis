# ================================================================
#  R/data_helpers.R  –  Data parsing & transformation helpers
# ================================================================

`%||%` <- function(a, b)
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# ----------------------------------------------------------------
#  Safe numeric coercion
# ----------------------------------------------------------------
.to_num <- function(x) suppressWarnings(as.numeric(x))

# ----------------------------------------------------------------
#  .extract_coord_n – numeric, always length n
# ----------------------------------------------------------------
.extract_coord_n <- function(col, field, n) {
  result <- tryCatch({
    if (is.null(col) || length(col) == 0)
      return(rep(NA_real_, n))

    # data.frame column (flatten=TRUE)
    if (is.data.frame(col)) {
      if (!field %in% names(col)) return(rep(NA_real_, n))
      v <- .to_num(col[[field]])
      if (length(v) == n) return(v)
      return(rep(NA_real_, n))
    }

    # list column (one element per row)
    if (is.list(col)) {
      if (length(col) != n) return(rep(NA_real_, n))
      return(vapply(col, function(x) {
        if (is.null(x)) return(NA_real_)
        if (is.list(x) || is.data.frame(x)) {
          val <- tryCatch(x[[field]], error = function(e) NULL)
          if (is.null(val) || length(val) == 0) return(NA_real_)
          return(suppressWarnings(as.numeric(val[[1]])))
        }
        NA_real_
      }, numeric(1)))
    }

    # plain atomic vector
    if (is.atomic(col) && length(col) == n)
      return(.to_num(col))

    rep(NA_real_, n)
  }, error = function(e) rep(NA_real_, n))

  # final length guarantee
  if (length(result) != n) rep(NA_real_, n) else result
}

# ----------------------------------------------------------------
#  .extract_char_n – character, always length n
# ----------------------------------------------------------------
.extract_char_n <- function(col, field, n) {
  result <- tryCatch({
    if (is.null(col) || length(col) == 0)
      return(rep(NA_character_, n))

    if (is.data.frame(col)) {
      if (!field %in% names(col)) return(rep(NA_character_, n))
      v <- as.character(col[[field]])
      if (length(v) == n) return(v)
      return(rep(NA_character_, n))
    }

    if (is.list(col)) {
      if (length(col) != n) return(rep(NA_character_, n))
      return(vapply(col, function(x) {
        if (is.null(x)) return(NA_character_)
        val <- tryCatch(x[[field]], error = function(e) NULL)
        if (is.null(val) || length(val) == 0) return(NA_character_)
        as.character(val[[1]])
      }, character(1)))
    }

    if (is.atomic(col) && length(col) == n)
      return(as.character(col))

    rep(NA_character_, n)
  }, error = function(e) rep(NA_character_, n))

  if (length(result) != n) rep(NA_character_, n) else result
}

# ----------------------------------------------------------------
#  .safe_atomic – coerce any column to a length-n atomic vector
# ----------------------------------------------------------------
.safe_atomic <- function(col, n) {
  if (is.null(col))  return(rep(NA_character_, n))
  if (is.data.frame(col)) {
    # take the first sub-column
    if (ncol(col) == 0) return(rep(NA_character_, n))
    col <- col[[1]]
  }
  if (is.list(col)) {
    col <- vapply(col, function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      as.character(x[[1]])
    }, character(1))
  }
  v <- as.character(col)
  if (length(v) == n) v else rep(NA_character_, n)
}

# ----------------------------------------------------------------
#  flatten_locations – clean data.frame from OpenAQ v3 /locations
# ----------------------------------------------------------------
flatten_locations <- function(raw) {
  if (is.null(raw) || length(raw) == 0) return(NULL)

  if (!is.data.frame(raw))
    raw <- tryCatch(as.data.frame(raw, stringsAsFactors = FALSE),
                    error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  n <- nrow(raw)

  # ---- coordinates (flat: coordinates.latitude) ----------------
  if ("coordinates.latitude" %in% names(raw)) {
    raw$latitude  <- suppressWarnings(as.numeric(raw[["coordinates.latitude"]]))
    raw$longitude <- suppressWarnings(as.numeric(raw[["coordinates.longitude"]]))
    raw[["coordinates.latitude"]]  <- NULL
    raw[["coordinates.longitude"]] <- NULL
  } else if ("coordinates" %in% names(raw)) {
    raw$latitude  <- .extract_coord_n(raw$coordinates, "latitude",  n)
    raw$longitude <- .extract_coord_n(raw$coordinates, "longitude", n)
    raw$coordinates <- NULL
  } else {
    lat_col <- intersect(c("latitude","lat","Latitude","Lat"), names(raw))[1]
    lon_col <- intersect(c("longitude","lon","Longitude","Lon","lng","Lng"),
                         names(raw))[1]
    raw$latitude  <- if (!is.na(lat_col[1]))
                       suppressWarnings(as.numeric(raw[[lat_col]]))
                     else rep(NA_real_, n)
    raw$longitude <- if (!is.na(lon_col[1]))
                       suppressWarnings(as.numeric(raw[[lon_col]]))
                     else rep(NA_real_, n)
  }
  raw$latitude  <- suppressWarnings(as.numeric(raw$latitude))
  raw$longitude <- suppressWarnings(as.numeric(raw$longitude))

  # ---- country (flat: country.name) ----------------------------
  if ("country.name" %in% names(raw)) {
    raw$country_name <- as.character(raw[["country.name"]])
    raw$country_code <- tryCatch(as.character(raw[["country.code"]]),
                                 error = function(e) rep(NA_character_, n))
    for (nm in grep("^country\\.", names(raw), value = TRUE))
      raw[[nm]] <- NULL
  } else if ("country" %in% names(raw)) {
    raw$country_name <- .extract_char_n(raw$country, "name", n)
    raw$country_code <- .extract_char_n(raw$country, "code", n)
    raw$country <- NULL
  }

  # ---- owner / provider (flat: owner.name) ---------------------
  if ("owner.name" %in% names(raw)) {
    raw$owner_name <- as.character(raw[["owner.name"]])
    for (nm in grep("^owner\\.", names(raw), value = TRUE)) raw[[nm]] <- NULL
  } else if ("owner" %in% names(raw)) {
    raw$owner_name <- .extract_char_n(raw$owner, "name", n)
    raw$owner <- NULL
  }

  if ("provider.name" %in% names(raw)) {
    raw$provider_name <- as.character(raw[["provider.name"]])
    for (nm in grep("^provider\\.", names(raw), value = TRUE)) raw[[nm]] <- NULL
  } else if ("provider" %in% names(raw)) {
    raw$provider_name <- .extract_char_n(raw$provider, "name", n)
    raw$provider <- NULL
  }

  # ---- drop known nested / array columns -----------------------
  drop_prefixes <- c("instruments","licenses","parameters","sensors",
                     "bbox","bounds","distance","datetimeFirst","datetimeLast")
  for (col in names(raw)) {
    if (any(sapply(drop_prefixes, function(p) startsWith(col, p))))
      raw[[col]] <- NULL
  }

  # ---- coerce any remaining list / data.frame columns ----------
  for (col in names(raw)) {
    if (is.list(raw[[col]]) || is.data.frame(raw[[col]]))
      raw[[col]] <- .safe_atomic(raw[[col]], n)
  }

  # ---- select useful columns -----------------------------------
  keep <- intersect(
    c("id","name","locality","country_name","country_code",
      "latitude","longitude","isMobile","isMonitor",
      "sensorsCount","lastUpdated","owner_name","provider_name"),
    names(raw))
  out <- raw[, keep, drop = FALSE]

  if ("id" %in% names(out))
    out$id <- suppressWarnings(as.integer(out$id))

  out
}

# ----------------------------------------------------------------
#  flatten_sensors – clean data.frame from /locations/{id}/sensors
# ----------------------------------------------------------------
flatten_sensors <- function(raw) {
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) return(NULL)
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  n   <- nrow(raw)

  # ---- parameter (flat: parameter.name / nested: parameter) ----
  if ("parameter.name" %in% names(raw)) {
    raw$param_name  <- as.character(raw[["parameter.name"]])
    raw$param_units <- tryCatch(as.character(raw[["parameter.units"]]),
                                error = function(e) rep(NA_character_, n))
    dn <- tryCatch(as.character(raw[["parameter.displayName"]]),
                   error = function(e) rep(NA_character_, n))
    raw$param_label <- ifelse(!is.na(dn) & nchar(dn) > 0,
                              dn, raw$param_name)
    for (col in grep("^parameter\\.", names(raw), value = TRUE))
      raw[[col]] <- NULL
  } else if ("parameter" %in% names(raw)) {
    pc <- raw$parameter
    raw$param_name  <- .extract_char_n(pc, "name",        n)
    raw$param_units <- .extract_char_n(pc, "units",       n)
    dn <- .extract_char_n(pc, "displayName", n)
    raw$param_label <- ifelse(!is.na(dn) & nchar(dn) > 0,
                              dn, raw$param_name)
    raw$parameter <- NULL
  }

  # ---- latest (flat: latest.value / latest.datetime.utc) -------
  if ("latest.value" %in% names(raw)) {
    raw$latest_value <- suppressWarnings(as.numeric(raw[["latest.value"]]))

    # datetime – several possible flat sub-column names
    dt_col <- NULL
    for (nm in c("latest.datetime.utc","latest.datetime",
                 "latest.datetimeLast.utc")) {
      if (nm %in% names(raw)) { dt_col <- raw[[nm]]; break }
    }
    if (is.null(dt_col) && "latest.datetime" %in% names(raw))
      dt_col <- raw[["latest.datetime"]]

    raw$latest_time <- if (!is.null(dt_col))
                         .safe_atomic(dt_col, n)
                       else rep(NA_character_, n)

    for (nm in grep("^latest\\.", names(raw), value = TRUE))
      raw[[nm]] <- NULL
  } else if ("latest" %in% names(raw)) {
    lt <- raw$latest
    raw$latest_value <- .extract_coord_n(lt, "value",    n)
    raw$latest_time  <- .extract_char_n(lt,  "datetime", n)
    raw$latest <- NULL
  }

  # ---- drop common nested columns ------------------------------
  for (col in c("summary","coverage","flags","calibrations"))
    if (col %in% names(raw)) raw[[col]] <- NULL

  # ---- coerce any remaining list / data.frame columns ----------
  for (col in names(raw)) {
    if (is.list(raw[[col]]) || is.data.frame(raw[[col]]))
      raw[[col]] <- .safe_atomic(raw[[col]], n)
  }

  if ("id" %in% names(raw))
    raw$id <- suppressWarnings(as.integer(raw$id))

  raw
}

# ----------------------------------------------------------------
#  sensor_label  –  human-readable checkbox label
# ----------------------------------------------------------------
sensor_label <- function(row) {
  nm  <- tryCatch(as.character(row$param_name[[1]]),  error = function(e) "?")
  un  <- tryCatch(as.character(row$param_units[[1]]), error = function(e) "")
  sid <- tryCatch(as.character(row$id[[1]]),          error = function(e) "?")
  lv  <- tryCatch(round(as.numeric(row$latest_value[[1]]), 2),
                  error = function(e) NA_real_)
  if (!is.null(lv) && length(lv) == 1 && !is.na(lv))
    paste0(nm, " [", un, "]  (ID:", sid, " · latest:", lv, ")")
  else
    paste0(nm, " [", un, "]  (ID:", sid, ")")
}

# ----------------------------------------------------------------
#  sensors_to_choices – named character vector for checkboxGroupInput
# ----------------------------------------------------------------
sensors_to_choices <- function(sensors_flat) {
  if (is.null(sensors_flat) || !is.data.frame(sensors_flat) ||
      nrow(sensors_flat) == 0)
    return(character(0))

  sf <- sensors_flat

  if (!"id" %in% names(sf)) sf$id <- seq_len(nrow(sf))
  if (!"param_name" %in% names(sf)) sf$param_name <- paste0("sensor_", sf$id)

  setNames(
    as.character(sf$id),
    vapply(seq_len(nrow(sf)),
           function(i) sensor_label(sf[i, , drop = FALSE]),
           character(1))
  )
}

# ----------------------------------------------------------------
#  extract_datetime  –  pull UTC timestamp from ANY response shape
#
#  After flatten=TRUE the columns are:
#    hourly/daily: period.datetimeFrom.utc
#    raw:          datetime.utc
#  Fallbacks handle older / un-flattened shapes.
# ----------------------------------------------------------------
extract_datetime <- function(df) {
  # ---- flat names (most common after flatten=TRUE) -------------
  flat_candidates <- c(
    "period.datetimeFrom.utc",
    "period.datetime.utc",
    "period.datetimeFrom.local",
    "datetime.utc",
    "datetime.local",
    "datetime"
  )
  for (nm in flat_candidates)
    if (nm %in% names(df)) return(as.character(df[[nm]]))

  # ---- nested period data.frame (older jsonlite without flatten) -
  if ("period" %in% names(df) && is.data.frame(df$period)) {
    per <- df$period
    dtf <- if ("datetimeFrom" %in% names(per)) per$datetimeFrom
           else if ("datetime" %in% names(per)) per$datetime
           else NULL
    if (!is.null(dtf)) {
      if (is.data.frame(dtf))
        return(as.character(if ("utc" %in% names(dtf)) dtf$utc else dtf[[1]]))
      return(as.character(dtf))
    }
  }

  # ---- nested datetime data.frame ------------------------------
  if ("datetime" %in% names(df) && is.data.frame(df$datetime)) {
    dt <- df$datetime
    return(as.character(if ("utc" %in% names(dt)) dt$utc else dt[[1]]))
  }

  # ---- bare date column (already character/POSIXct) ------------
  if ("date" %in% names(df)) return(as.character(df$date))

  NULL
}

# ----------------------------------------------------------------
#  tidy_sensor  –  one sensor result → 2-column data.frame
#                  (date [POSIXct], <param_name> [numeric])
# ----------------------------------------------------------------
tidy_sensor <- function(meas_df, param_name) {
  if (is.null(meas_df) || !is.data.frame(meas_df) || nrow(meas_df) == 0)
    return(NULL)

  dt_str <- extract_datetime(meas_df)
  if (is.null(dt_str) || length(dt_str) == 0 || all(is.na(dt_str)))
    return(NULL)

  # Robust POSIXct parse – handle Z-suffix and space-separated formats
  dt <- suppressWarnings(
    lubridate::parse_date_time(
      dt_str,
      orders = c("YmdHMSz","YmdHMS","Ymd HMSz","Ymd HMS",
                 "Ymd HM","Ymd H","Ymd"),
      tz = "UTC",
      quiet = TRUE
    )
  )
  # fallback
  if (all(is.na(dt)))
    dt <- suppressWarnings(as.POSIXct(dt_str, tz = "UTC"))

  # round to the nearest hour to align sensors
  dt <- lubridate::round_date(dt, unit = "hour")

  val <- suppressWarnings(as.numeric(meas_df$value))

  n   <- min(length(dt), length(val))
  if (n == 0) return(NULL)

  out <- data.frame(
    date  = dt[seq_len(n)],
    value = val[seq_len(n)],
    stringsAsFactors = FALSE
  )
  names(out)[2] <- make.names(param_name)

  out[!is.na(out$date), , drop = FALSE]
}

# ----------------------------------------------------------------
#  build_wide_df  –  join tidy sensor frames into openair-ready df

# ----------------------------------------------------------------
build_wide_df <- function(sensor_list) {
  # Build tidy parts
  parts <- vector("list", length(sensor_list))
  names(parts) <- names(sensor_list)

  for (nm in names(sensor_list)) {
    parts[[nm]] <- tryCatch(
      tidy_sensor(sensor_list[[nm]], nm),
      error = function(e) NULL
    )
  }
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) return(NULL)

  # Deduplicate column names across parts
  seen <- character(0)
  for (i in seq_along(parts)) {
    cn <- setdiff(names(parts[[i]]), "date")
    for (c0 in cn) {
      if (c0 %in% seen) {
        k   <- 2L
        new <- paste0(c0, "_", k)
        while (new %in% seen) { k <- k + 1L; new <- paste0(c0, "_", k) }
        names(parts[[i]])[names(parts[[i]]) == c0] <- new
        c0 <- new
      }
      seen <- c(seen, c0)
    }
  }

  result <- parts[[1]]
  for (i in seq_along(parts)[-1])
    result <- dplyr::full_join(result, parts[[i]], by = "date")

  result |>
    dplyr::arrange(date) |>
    dplyr::distinct(date, .keep_all = TRUE)
}

# ----------------------------------------------------------------
#  prepare_for_openair  –  coerce an arbitrary CSV to openair form
# ----------------------------------------------------------------
prepare_for_openair <- function(df, date_col = "date",
                                ws_col = NULL, wd_col = NULL,
                                date_fmt = "auto") {
  if (date_col != "date" && date_col %in% names(df))
    names(df)[names(df) == date_col] <- "date"

  if (!inherits(df$date, "POSIXct")) {
    df$date <- if (date_fmt == "auto") {
      tryCatch(
        as.POSIXct(df$date, tz = "UTC"),
        error = function(e)
          lubridate::parse_date_time(df$date,
            orders = c("ymd HMS","mdy HMS","dmy HMS",
                       "ymd HM","mdy HM","dmy HM","ymd")))
    } else {
      lubridate::parse_date_time(df$date, orders = date_fmt)
    }
  }

  rename_col <- function(df, old, new) {
    if (!is.null(old) && nchar(old) > 0 && old %in% names(df) && old != new)
      names(df)[names(df) == old] <- new
    df
  }
  df <- rename_col(df, ws_col, "ws")
  df <- rename_col(df, wd_col, "wd")

  for (col in setdiff(names(df), "date"))
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))

  df[!is.na(df$date), , drop = FALSE]
}

# ----------------------------------------------------------------
#  completeness_summary – for the progress-bar UI
# ----------------------------------------------------------------
completeness_summary <- function(df) {
  pc <- pollutant_cols(df)
  n  <- nrow(df)
  lapply(pc, function(p) {
    v   <- sum(!is.na(df[[p]]))
    pct <- if (n > 0) round(v / n * 100) else 0L
    list(param = p, n = v, pct = pct,
         col = if (pct > 80) "#1d8348"
               else if (pct > 50) "#d4ac0d"
               else "#922b21")
  })
}

# ----------------------------------------------------------------
#  Utility helpers
# ----------------------------------------------------------------
has_wind <- function(df)
  all(c("ws","wd") %in% names(df)) &&
  sum(!is.na(df$ws) & !is.na(df$wd)) > 10

pollutant_cols <- function(df) setdiff(names(df), c("date","ws","wd"))

build_popup <- function(row) {
  nm  <- tryCatch(as.character(row$name[[1]]),
                  error = function(e) paste0("ID:", row$id))
  loc <- if (!is.null(row$locality) && !is.na(row$locality) &&
             nchar(as.character(row$locality)) > 0)
           paste0("\U0001f4cd ", row$locality, "<br>") else ""
  cty <- if (!is.null(row$country_name) && !is.na(row$country_name) &&
             nchar(as.character(row$country_name)) > 0)
           paste0("\U0001f30d ", row$country_name, "<br>") else ""
  sns <- if (!is.null(row$sensorsCount) && !is.na(row$sensorsCount))
           paste0("  &bull;  Sensors: ", row$sensorsCount) else ""
  upd <- if (!is.null(row$lastUpdated) && !is.na(row$lastUpdated))
           paste0("<br><small>Updated: ",
                  substr(as.character(row$lastUpdated), 1, 10), "</small>")
         else ""
  paste0("<b>", nm, "</b><br>", loc, cty,
         "ID: <code>", row$id, "</code>", sns, upd)
}
