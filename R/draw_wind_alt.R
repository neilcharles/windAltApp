draw_wind_alt <- function(weather, location, wind_speed_red_kph = 25, altitude_units, speed_units){

  wind_speed_red <- units_to_selected(wind_speed_red_kph, "kph", speed_units)

  interp_wind <- tibble::as_tibble(approx(weather$altitude, weather$windspeed, n=100))

  altitude_lines <- units_to_selected(c(110, 320, 500, 800, 1000, 1500), "metres", altitude_units)

  altitude_lines = round(altitude_lines / 10) * 10

  chart <- weather |>
    ggplot2::ggplot(ggplot2::aes(x = windspeed, y = altitude)) +

    #Alt Lines
    ggplot2::geom_segment(data = weather, ggplot2::aes(x = -5, xend = max(windspeed)*1.1, y = altitude_lines[1], yend = altitude_lines[1]), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = -5, y = altitude_lines[1], label = altitude_lines[1]), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::geom_segment(data = weather, ggplot2::aes(x = -5, xend = max(windspeed)*1.1, y = altitude_lines[2], yend = altitude_lines[2]), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = -5, y = altitude_lines[2], label = altitude_lines[2]), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::geom_segment(data = weather, ggplot2::aes(x = -5, xend = max(windspeed)*1.1, y = altitude_lines[3], yend = altitude_lines[3]), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = -5, y = altitude_lines[3], label = altitude_lines[3]), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::geom_segment(data = weather, ggplot2::aes(x = -5, xend = max(windspeed)*1.1, y = altitude_lines[4], yend = altitude_lines[4]), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = -5, y = altitude_lines[4], label = altitude_lines[4]), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::geom_segment(data = weather, ggplot2::aes(x = -5, xend = max(windspeed)*1.1, y = altitude_lines[5], yend = altitude_lines[5]), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = -5, y = altitude_lines[5], label = altitude_lines[5]), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::geom_segment(data = weather, ggplot2::aes(x = -5, xend = max(windspeed)*1.1, y = altitude_lines[6], yend = altitude_lines[6]), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = -5, y = altitude_lines[6], label = altitude_lines[6]), hjust=0, vjust=0, size = 5, colour = "grey") +

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
                  y = glue::glue("altitude ({altitude_units})"),
                  caption = "Data: NOAA GFS") +

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
                   plot.background=ggplot2::element_blank())

  chart

}
