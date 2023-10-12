get_wind_alt <- function(lat = NULL, lon = NULL){

  get_data <- function(lat, lon, fact){
    raw <- jsonlite::read_json(
      # glue::glue("https://api.open-meteo.com/v1/ecmwf?latitude={lat}&longitude={lon}&hourly={fact}")
      glue::glue("https://api.open-meteo.com/v1/gfs?latitude={lat}&longitude={lon}&hourly={fact}&timezone=Europe%2FLondon")  #&past_days=5")
    )

    raw_tbl <- tibble::tibble(
      time = unlist(raw$hourly$time),
      wind_value = unlist(raw$hourly[fact])
    ) |>
      dplyr::mutate(fact = fact) |>
      tidyr::pivot_longer(cols = c(-time, -fact))

    raw_tbl

  }

  weather <- get_data(lat, lon, "windspeed_1000hPa") |>
    dplyr::union_all(
      get_data(lat, lon, "windspeed_975hPa")
    ) |>
    dplyr::union_all(
      get_data(lat, lon, "windspeed_950hPa")
    ) |>
    dplyr::union_all(
      get_data(lat, lon, "windspeed_925hPa")
    ) |>
    dplyr::union_all(
      get_data(lat, lon, "windspeed_900hPa")
    ) |>
    dplyr::union_all(
      get_data(lat, lon, "windspeed_850hPa")
    ) |>
    dplyr::union_all(
      get_data(lat, lon, "winddirection_1000hPa")
    ) |>
    dplyr::union_all(
      get_data(lat, lon, "winddirection_975hPa")
    ) |>
    dplyr::union_all(
      get_data(lat, lon, "winddirection_950hPa")
    ) |>
    dplyr::union_all(
      get_data(lat, lon, "winddirection_925hPa")
    ) |>
    dplyr::union_all(
      get_data(lat, lon, "winddirection_900hPa")
    ) |>
    dplyr::union_all(
      get_data(lat, lon, "winddirection_850hPa")
    )

  weather

}
