# --- TEST POINT ---
# lat = 34.7
# lon = -119.7
# pt = c(lon, lat)
# pt = data.frame(lon, lat)
# pt <- st_as_af
# send post request to API to create AOI poly
api_aoi <- function(pt) {
  pt <- jsonlite::toJSON(pt)
  post <- httr::content(httr::POST(
    url = "https://api.burndex.ai/aoi",
    query = list(pt = pt)
  ))
  geojsonsf::geojson_sf(post[[1]])
}

# send post request to API for historical fire perimeters within AOI
fire_perims_api <- function(pt) {
  pt <- pt %>%
    sf::st_coordinates() %>%
  tibble::as_tibble() %>%
    setNames(c("lng", "lat"))
  post <- httr::POST(
    url = "https://api.burndex.ai/fires",
    query = list(
      lat = pt$lat,
      lon = pt$lng
    )
  )
  httr::stop_for_status(post)
  content <- httr::content(post, encoding = "application/json")
  geojsonsf::geojson_sf(content[[1]])
}
# send post request to API for historical fire perimeters within AOI
fire_time_api <- function(pt) {
  pt <- jsonlite::toJSON(pt)
  post <- httr::content(httr::POST(
    url = "https://api.burndex.ai/fires_timeseries",
    query = list(pt= pt)
  ))
  jsonlite::fromJSON(post[[1]])
}


bx_handle <- function() {
  httr::handle("https://api.burndex.ai/")
}

bx_agent <- function() {
  httr::user_agent("app.burndex.ai")
}

bx_GET <- function(endpoint, ...) {
  request <- httr::GET(
    handle = bx_handle(),
    path   = endpoint,
    agent  = bx_agent(),
    ...
  )

  httr::stop_for_status(request)

  httr::content(request)
}

bx_POST <- function(endpoint, ...) {
  request <- httr::POST(
    handle = bx_handle(),
    path   = endpoint,
    agent  = bx_agent(),
    ...
  )

  httr::stop_for_status(request)

  httr::content(request)
}
# get_prediction <- function(pt, date) {
#   bb <- pt %>%
#     AOI::aoi_buffer(10, km = TRUE) %>%
#     sf::st_bbox()
#   r <- bx_POST(
#     "predict-aoi",
#     query = list(
#       xmin = bb$xmin,
#       xmax = bb$xmax,
#       ymin = bb$ymin,
#       ymax = bb$ymax,
#       date = date
#     )
#   )
#   jsonlite::fromJSON(r[[1]]) %>%
#     tidy_to_raster(x = lon, y = lat, z = .pred)
# }

#' @title Predict Burning Index via API
#' @param pt Point `sf` object
#' @param date Date
#' @return vector of prediction(s)
bx_predict <- function(pt, date) {
  bb <- pt %>%
    AOI::aoi_buffer(0.005, km = TRUE) %>%
    sf::st_bbox()
  cont <- bx_POST(
    "predict-aoi",
    query = list(
      xmin = bb$xmin,
      xmax = bb$xmax,
      ymin = bb$ymin,
      ymax = bb$ymax,
      date = date
    )
  )
  tibble::as_tibble(
    jsonlite::fromJSON(cont[[1]])
  )
}

#' @title Get Prediction Raster
#' @param pt `sf` object
#' @param date Date
bx_raster <- function(pt, date) {
  preds <- bx_predict(pt, date)
  tidy_to_raster(preds, lon, lat, .pred)
}


