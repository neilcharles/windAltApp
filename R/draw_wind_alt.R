draw_wind_alt <- function(weather, location, wind_speed_red_kph = 25, altitude_units, speed_units){

  #Drop rows that have altitudes below ground level due to low pressure
  weather <- weather |>
    filter(!geopotential_height < 1)

  wind_speed_red <- units_to_selected(wind_speed_red_kph, "kph", speed_units)

  interp_wind <- tibble::as_tibble(approx(weather$geopotential_height, weather$windspeed, n=100))

  # Draw a guideline for each alt (as a list for compactness)
  draw_guidelines <- function(y, max_x){
    y = round(y / 10) * 10

    list(
      ggplot2::geom_segment(data = weather, ggplot2::aes(x = -5, xend = max_x*1.1, y = y, yend = y), colour = "grey", linetype = "dashed", size = 0.1),
      ggplot2::geom_text(ggplot2::aes(x = -5, y = y, label = y), hjust=0, vjust=0, size = 6, colour = "grey")
    )
  }

  max_x <- max(weather$windspeed)

  guidelines <- weather |>
    dplyr::group_by(geopotential_height) |>
    dplyr::summarise() |>
    pull(geopotential_height) |>
    purrr::map2(.y = max_x, .f = ~draw_guidelines(.x, .y))


  # Draw the chart
  chart <- weather |>
    ggplot2::ggplot(ggplot2::aes(x = windspeed, y = geopotential_height)) +
    guidelines +

    ggplot2::scale_colour_gradient2(
      low='green',
      mid = 'green',
      high = 'red',
      midpoint = 8,
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

    #Takeoff Line
    ggplot2::geom_segment(data = weather, ggplot2::aes(x = -5, xend = max(windspeed)*1.1, y = location$elevation, yend = location$elevation), colour = "black", size = 1, linetype = "dashed") +
    ggplot2::geom_text(ggplot2::aes(x = -5, y = location$elevation, label = glue::glue("{location$takeoff_name} Takeoff")), hjust=0, vjust=-0.5, size = 5, colour = "black") +

    ggplot2::ggtitle(location$takeoff_name, unique(weather$time)) +

    ggplot2::labs(x = glue::glue("windspeed ({speed_units})"),
                  y = glue::glue("altitude amsl ({altitude_units})"),
                  caption = "Data: DWD-ICON") +

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

  chart

}
