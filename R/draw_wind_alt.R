draw_wind_alt <- function(weather, location, wind_speed_red_kph = 25, altitude_units, speed_units, attribution){

  #Drop rows that have altitudes below ground level due to low pressure
  weather <- weather |>
    filter(!geopotential_height < 1)

  wind_speed_red <- units_to_selected(wind_speed_red_kph, "kph", speed_units)

  interp_wind <- tibble::as_tibble(approx(weather$geopotential_height, weather$windspeed, n=100))

  # Draw a guideline for each alt (as a list for compactness)
  draw_guidelines <- function(y, max_x, with_annotations = TRUE){
    y = round(y / 10) * 10

    if(with_annotations){
      list(
        ggplot2::geom_hline(yintercept = y, colour = "grey", linetype = "dashed", size = 0.2),
        ggplot2::annotate("text", x = -5, y = y, label = y, size = 6, colour = "grey", vjust = -0.5)
      )
    } else {
      list(
        ggplot2::geom_hline(yintercept = y, colour = "grey", linetype = "dashed", size = 0.2)
      )
    }
  }

  max_x <- max(weather$windspeed)

  # For main chart, with text
  guidelines <- weather |>
    dplyr::group_by(geopotential_height) |>
    dplyr::summarise() |>
    pull(geopotential_height) |>
    purrr::map2(.y = max_x, .f = ~draw_guidelines(.x, .y))

  # For cloud chart, just lines
  guidelines_simple <- weather |>
    dplyr::group_by(geopotential_height) |>
    dplyr::summarise() |>
    pull(geopotential_height) |>
    purrr::map2(.y = max_x, .f = ~draw_guidelines(.x, .y, with_annotations = FALSE))

  guidelines_temp <<- guidelines
  weather_temp <<- weather
  wind_speed_red_temp <<- wind_speed_red
  interp_wind_temp <<- interp_wind


  # Base background
  chart_base <- weather |>
    ggplot2::ggplot(ggplot2::aes(x = windspeed, y = geopotential_height)) +
    #Takeoff Line
    ggplot2::geom_hline(yintercept = location$elevation, colour = "black", size = 1, linetype = "dashed") +

    ggplot2::scale_y_continuous(limits = c(0, max(weather_temp$geopotential_height*1.1))) +

    ggplot2::ggtitle(location$takeoff_name, unique(weather$time)) +

    ggplot2::labs(x = glue::glue("windspeed ({speed_units})"),
                  y = glue::glue("altitude amsl ({altitude_units})"))

  # Left side wind chart
  wind_chart <- chart_base +
    guidelines +
    ggplot2::scale_colour_gradientn(
      colours = c('lightblue', 'lightblue', 'green', 'red'),
      values = c(0, 0.25, 0.7, 1),
      na.value = 'red',
      limits = c(0, wind_speed_red)
    ) +
    ggplot2::scale_linewidth(
      range = c(1,15),
      limits = c(0,150)
    ) +
    ggplot2::geom_path(data = interp_wind, ggplot2::aes(x = y, y = x, linewidth = y, colour = y), alpha = 1, lineend="round") +
    ggplot2::geom_text(label = "\u2193", size = 15, ggplot2::aes(angle = -winddirection), hjust = 0, colour = "#696969", alpha = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = round(windspeed)), hjust=1.5, vjust=0, size = 6, colour = "black") +
    ggplot2::annotate("text", x = -5, y = location$elevation, label = glue::glue("{location$takeoff_name} Takeoff"), hjust = 0, vjust=-0.5, size = 5, colour = "black") +
    ggplot2::theme(axis.line=ggplot2::element_blank(),
                   axis.ticks=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_text(size = 12),
                   axis.title.x=ggplot2::element_text(size = 12),
                   plot.caption=ggplot2::element_text(size = 10),
                   plot.title=ggplot2::element_text(size = 16),
                   plot.subtitle=ggplot2::element_text(size = 12),
                   axis.text.y=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_text(size = 12),
                   legend.position="none",
                   panel.background=ggplot2::element_blank(),
                   panel.border=ggplot2::element_blank(),
                   panel.grid.major=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank(),
                   plot.background=ggplot2::element_rect(fill = 'white', colour = 'white'))

  # RIght side cloud chart
  clouds_chart <- chart_base +
    guidelines_simple +
    #Clouds
    ggimage::geom_image(image = "cloud-solid2.png", aes(x = -3, alpha = cloud_cover), size = 0.08, hjust = 0, colour = "#696969") + #clouds
    ggplot2::scale_alpha_continuous(range = c(0,1), limits = c(0,100)) +
    # ggplot2::geom_text(label = "\u2601", aes(x = -3), size = 15, hjust = 0.5, colour = "#696969", alpha = 0.6, vjust = -0.1) + #clouds
    ggplot2::geom_text(aes(label = glue::glue("{cloud_cover}%"), x = -3), size = 4, hjust = 0.3, colour = "#FFFFFF", alpha = 1, vjust = -1) +
    ggplot2::geom_text(aes(label = glue::glue("{round(temperature, 0)}\u00B0c"), x = -3, colour = temperature), size = 4, hjust = 0.3, alpha = 1, vjust = 1) +
    scale_colour_gradientn(
      colours = c("blue", "yellow", "orange"),
      limits = c(-3, 20),
      oob = scales::squish
    ) +    labs(
      caption = attribution
    ) +
    ggplot2::theme(axis.line=ggplot2::element_blank(),
                   axis.ticks=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_text(size = 12, colour = "white"),
                   axis.title.x=ggplot2::element_text(size = 12, colour = "white"),
                   plot.caption=ggplot2::element_text(size = 10),
                   plot.title=ggplot2::element_text(size = 16, colour = "white"),
                   plot.subtitle=ggplot2::element_text(size = 12, colour = "white"),
                   axis.text.y=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_text(size = 12, colour = "white"),
                   legend.position="none",
                   panel.background=ggplot2::element_blank(),
                   panel.border=ggplot2::element_blank(),
                   panel.grid.major=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank(),
                   plot.background=ggplot2::element_rect(fill = 'white', colour = 'white'))

  patchwork::wrap_plots(wind_chart, clouds_chart, widths = c(3, 1))
}
