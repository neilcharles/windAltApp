pressure_altitudes <- function(){
  tibble::tibble(
    pressure_alt = c("1000hPa", "975hPa", "950hPa", "925hPa", "900hPa", "850hPa")
  )
}

open_meteo_data <- function(lat, lon, fact, forecast_service = "dwd-icon", timezone = "Europe%2FLondon"){

  weather <- tibble::tibble(metric = as.character(fact)) |>
      dplyr::mutate(weather =
                      purrr::map(
                        .x = metric,
                        .f = ~jsonlite::read_json(
                          glue::glue("https://api.open-meteo.com/v1/{forecast_service}?latitude={lat}&longitude={lon}&hourly={.x}&timezone={timezone}&elevation=nan")  #&past_days=5")
        )
      )) |>
    dplyr::mutate(
      weather_tbl =
        purrr::map(
          .x = weather,
          .f = ~
              tibble::tibble(
                time = unlist(.x$hourly$time),
                value = unlist(.x$hourly[2]),
                elevation = unlist(.x$elevation)
                )
          )
        ) |>
    dplyr::select(-weather) |>
    tidyr::unnest(cols = weather_tbl)

  weather
}


get_weather_at_altitude <- function(lat = NULL, lon = NULL, fact, forecast_service = "dwd-icon"){

  pressure_altitudes() |>
    dplyr::mutate(fact = fact) |>
    dplyr::mutate(request = glue::glue("{fact}_{pressure_alt}")) |>
    dplyr::mutate(weather = purrr::map(
      .x = request,
      .f = ~ open_meteo_data(lat, lon, .x, forecast_service)
    )) |>
    dplyr::select(-request) |>
    tidyr::unnest(weather)

}

get_weather_at_10m <- function(lat = NULL, lon = NULL, fact, forecast_service = "dwd-icon"){

    open_meteo_data(lat, lon, fact, forecast_service) |>
      mutate(geopotential_height = elevation + 10,
             metric = stringr::str_replace(metric, "_10m", ""),
             pressure_alt = NA)

}
