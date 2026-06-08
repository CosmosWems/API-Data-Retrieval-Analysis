# ================================================================
#  R/api_helpers.R  –  OpenAQ v3 REST API wrappers
# ================================================================

OPENAQ_BASE    <- "https://api.openaq.org/v3"
OPENAQ_MAX_RAD <- 25000L   # metres – API hard limit (25 km)

# ----------------------------------------------------------------
#  Core authenticated GET – returns parsed list or stops with msg
# ----------------------------------------------------------------
openaq_get <- function(endpoint, params = list(), api_key) {

  resp <- httr::GET(
    paste0(OPENAQ_BASE, endpoint),
    httr::add_headers("X-API-Key" = api_key,
                      "Accept"    = "application/json"),
    query = if (length(params) > 0) params else NULL
  )

  sc   <- httr::status_code(resp)
  body <- httr::content(resp, "text", encoding = "UTF-8")

  # ---- HTTP error guards ----------------------------------------
  if (sc == 401) stop("Invalid API key. Verify it at explore.openaq.org.")
  if (sc == 403) stop("Access forbidden (403). Key may lack permissions.")
  if (sc == 404) stop("Endpoint not found (404).")
  if (sc == 422) stop(paste0("Validation error (422): ", body))
  if (sc == 429) stop("Rate limit hit (429). Wait ~60 s and retry.")
  if (sc >= 500) stop(paste0("OpenAQ server error (", sc, "). Try later."))
  if (sc != 200) stop(paste0("API error (", sc, "): ", body))

  # ---- Single, clean JSON parse ---------------------------------
  # flatten = TRUE converts nested objects to dot-separated column
  # names (e.g. coordinates.latitude) preventing the
  # "replacement has N rows" data.frame error.
  tryCatch({
    parsed <- jsonlite::fromJSON(
      txt              = body,
      simplifyVector   = TRUE,
      simplifyDataFrame = TRUE,
      flatten          = TRUE
    )
    parsed
  }, error = function(e) {
    message("[openaq_get] JSON parse failed: ", e$message)
    list(results = data.frame(), meta = list(error = e$message))
  })
}

# ----------------------------------------------------------------
#  Test API key – returns list(ok, msg)
# ----------------------------------------------------------------
test_api_key <- function(api_key) {
  tryCatch({
    openaq_get("/countries", list(limit = 1), api_key)
    list(ok = TRUE, msg = "Connected.")
  }, error = function(e) {
    list(ok = FALSE, msg = as.character(e$message))
  })
}

# ----------------------------------------------------------------
#  Countries
# ----------------------------------------------------------------
get_countries <- function(api_key, limit = 300) {
  res <- openaq_get("/countries", list(limit = limit), api_key)
  if (is.null(res$results) || length(res$results) == 0)
    return(data.frame(id = integer(0), name = character(0),
                      code = character(0), stringsAsFactors = FALSE))
  as.data.frame(res$results, stringsAsFactors = FALSE)
}

# ----------------------------------------------------------------
#  Location search helpers – all return res$results (raw df/list)
#  or NULL when nothing is found.
# ----------------------------------------------------------------

search_locations_by_name <- function(name, limit = 50,
                                     param_id = NULL, api_key) {
  p <- list(search = trimws(name), limit = as.integer(limit))
  if (!is.null(param_id) && nchar(as.character(param_id)) > 0)
    p$parameters_id <- as.integer(param_id)
  res <- openaq_get("/locations", p, api_key)
  if (is.null(res$results) || length(res$results) == 0) return(NULL)
  res$results
}

search_locations_by_coords <- function(lat, lon, radius_km = 25,
                                       limit = 100, api_key) {
  lat    <- as.numeric(lat)
  lon    <- as.numeric(lon)
  if (!is.finite(lat) || !is.finite(lon))
    stop("Latitude and longitude must be valid numbers.")
  radius <- min(as.integer(round(as.numeric(radius_km) * 1000)),
                OPENAQ_MAX_RAD)
  res <- openaq_get("/locations",
    list(coordinates = paste0(lat, ",", lon),
         radius      = radius,
         limit       = as.integer(limit)),
    api_key)
  if (is.null(res$results) || length(res$results) == 0) return(NULL)
  res$results
}

search_locations_by_bbox <- function(min_lon, min_lat, max_lon, max_lat,
                                     limit = 200, api_key) {
  vals <- as.numeric(c(min_lon, min_lat, max_lon, max_lat))
  if (any(!is.finite(vals)))
    stop("All bounding box values must be valid numbers.")
  if (vals[1] >= vals[3]) stop("Min longitude must be less than max.")
  if (vals[2] >= vals[4]) stop("Min latitude must be less than max.")
  res <- openaq_get("/locations",
    list(bbox  = paste(vals, collapse = ","),
         limit = as.integer(limit)),
    api_key)
  if (is.null(res$results) || length(res$results) == 0) return(NULL)
  res$results
}

search_locations_by_country <- function(country_id, limit = 200, api_key) {
  res <- openaq_get("/locations",
    list(countries_id = as.integer(country_id),
         limit        = as.integer(limit)),
    api_key)
  if (is.null(res$results) || length(res$results) == 0) return(NULL)
  res$results
}

# ----------------------------------------------------------------
#  Sensors for a location  (params = list() passed explicitly)
# ----------------------------------------------------------------
get_location_sensors <- function(location_id, api_key) {
  res <- openaq_get(
    endpoint = paste0("/locations/", as.integer(location_id), "/sensors"),
    params   = list(),
    api_key  = api_key
  )
  if (is.null(res$results) || length(res$results) == 0)
    return(data.frame())
  as.data.frame(res$results, stringsAsFactors = FALSE)
}

# ----------------------------------------------------------------
#  Measurements  (raw / hourly / daily)
#
#  Returns a data.frame (possibly 0 rows) or NULL.
#  Handles the case where the API returns an empty JSON array "[]"
#  which jsonlite converts to list() – not data.frame().
# ----------------------------------------------------------------
.fmt_dt <- function(x)
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

fetch_measurements <- function(sensor_id, date_from, date_to,
                               agg = "hourly", api_key, limit = 1000) {
  ep <- switch(agg,
    raw    = paste0("/sensors/", sensor_id, "/measurements"),
    hourly = paste0("/sensors/", sensor_id, "/hours"),
    daily  = paste0("/sensors/", sensor_id, "/days"),
    stop("agg must be 'raw', 'hourly', or 'daily'"))

  res <- openaq_get(ep,
    list(datetime_from = .fmt_dt(date_from),
         datetime_to   = .fmt_dt(date_to),
         limit         = as.integer(limit)),
    api_key)

  r <- res$results

  # Empty array "[]" → list(); NULL; 0-row df → all return NULL
  if (is.null(r))                                  return(NULL)
  if (is.list(r) && !is.data.frame(r) && length(r) == 0) return(NULL)
  if (is.data.frame(r) && nrow(r) == 0)           return(NULL)

  # Coerce to data.frame (already flat from flatten=TRUE)
  tryCatch(
    as.data.frame(r, stringsAsFactors = FALSE),
    error = function(e) {
      message("[fetch_measurements] coerce error: ", e$message)
      NULL
    }
  )
}
