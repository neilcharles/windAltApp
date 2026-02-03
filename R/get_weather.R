pressure_altitudes <- function(){
  tibble::tibble(
    pressure_alt = c("1000hPa", "975hPa", "950hPa", "925hPa", "900hPa", "850hPa", "700hPa", "600hPa")
  )
}

open_meteo_data <- function(lat, lon, fact, forecast_service = "icon", timezone = "Europe%2FLondon"){

  weather <- tibble::tibble(metric = as.character(fact)) |>
      dplyr::mutate(weather =
                      purrr::map(
                        .x = metric,
                        .f = ~jsonlite::read_json(
                          # glue::glue("https://api.open-meteo.com/v1/{forecast_service}?latitude={lat}&longitude={lon}&hourly={.x}&timezone={timezone}&elevation=nan")  #&past_days=5")  # OLD pre _seamless
                          glue::glue("https://api.open-meteo.com/v1/forecast?latitude={lat}&longitude={lon}&hourly={.x}&timezone={timezone}&models={forecast_service}_seamless&elevation=nan")
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

open_meteo_data_new <- function(lat, lon, request, forecast_service = "icon", timezone = "Europe%2FLondon"){

  result <- jsonlite::read_json(
    glue::glue(
      "https://api.open-meteo.com/v1/forecast?latitude={lat}&longitude={lon}&hourly={request}&timezone={timezone}&models={forecast_service}_seamless&elevation=nan"
    )
  )

  weather <- result$hourly |>
    purrr::map_df(~unlist(.x)) |>
    tidyr::pivot_longer(-time, names_to = "metric") |>
    dplyr::mutate(elevation = result$elevation)

  weather
}

get_weather_at_altitude_new <- function(lat = NULL, lon = NULL, fact = c("windspeed", "winddirection", "geopotential_height", "temperature", "cloud_cover"), forecast_service = "icon"){

  request <- pressure_altitudes() |>
    dplyr::cross_join(tibble::as_tibble(fact)) |>
    dplyr::mutate(request_param = glue::glue("{value}_{pressure_alt}")) |>
    dplyr::pull(request_param) |>
    paste0(collapse = ",")

  open_meteo_data_new(lat, lon, request, forecast_service) |>
    dplyr::mutate(pressure_alt = str_extract(metric, "\\d+hPa*")) |>
    dplyr::mutate(fact = stringr::str_replace(metric, glue::glue("_{pressure_alt}"), ""))

}


get_weather_at_altitude <- function(lat = NULL, lon = NULL, fact, forecast_service = "icon"){

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

get_weather_basic <- function(lat = NULL, lon = NULL, fact, forecast_service = "icon"){

    open_meteo_data(lat, lon, fact, forecast_service) |>
      mutate(geopotential_height = elevation + 10,
             metric = stringr::str_replace(metric, "_\\d+m", ""),
             pressure_alt = NA)

}

get_weather_basic_new <- function(lat = NULL, lon = NULL, fact = c("windspeed_10m", "winddirection_10m", "wind_gusts_10m", "temperature", "weather_code"), forecast_service = "icon"){

  request <- paste0(fact, collapse = ",")

  open_meteo_data_new(lat, lon, request, forecast_service) |>
    mutate(geopotential_height = elevation + 10,
           metric = stringr::str_replace(metric, "_\\d+m", ""),
           pressure_alt = NA)

}


get_weather_code <- function(){

}
