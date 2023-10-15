pressure_altitudes <- function(){
  tibble::tibble(
    pressure_alt = c("1000hPa", "975hPa", "950hPa", "925hPa", "900hPa", "850hPa"),
         alt_m = c(110, 320, 500, 800, 1000, 1500)
  )
}

open_meteo_data <- function(lat, lon, fact, forecast_service = "gfs", timezone = "Europe%2FLondon"){

  weather <- tibble::tibble(metric = as.character(fact)) |>
      dplyr::mutate(weather =
                      purrr::map(
                        .x = metric,
                        .f = ~jsonlite::read_json(
                          glue::glue("https://api.open-meteo.com/v1/{forecast_service}?latitude={lat}&longitude={lon}&hourly={.x}&timezone={timezone}")  #&past_days=5")
        )
      )) |>
    dplyr::mutate(
      weather_tbl =
        purrr::map(
          .x = weather,
          .f = ~
              tibble::tibble(
                time = unlist(.x$hourly$time),
                value = unlist(.x$hourly[2])
                )
          )
        ) |>
    dplyr::select(-weather) |>
    tidyr::unnest(cols = weather_tbl)

  weather
}


get_weather_at_altitude <- function(lat = NULL, lon = NULL, fact){

  pressure_altitudes() |>
    dplyr::mutate(fact = fact) |>
    dplyr::mutate(request = glue::glue("{fact}_{pressure_alt}")) |>
    dplyr::mutate(weather = purrr::map(
      .x = request,
      .f = ~ open_meteo_data(lat, lon, .x)
    )) |>
    dplyr::select(-request) |>
    tidyr::unnest(weather)

}
