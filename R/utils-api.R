api_aoi <- function(pt) {
  pt <- jsonlite::toJSON(pt)
  post <- httr::content(httr::POST(
    url = "http://localhost:8000/aoi",
    query = list(pt = pt)
  ))
  geojsonsf::geojson_sf(post[[1]])
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