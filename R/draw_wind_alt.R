draw_wind_alt <- function(weather, location){

  interp_wind <- tibble::as_tibble(approx(weather$alt_feet, weather$windspeed, n=100))

  chart <- weather |>
    ggplot2::ggplot(ggplot2::aes(x = windspeed, y = alt_feet)) +

    #Alt Lines
    ggplot2::geom_segment(data = weather, ggplot2::aes(x = 0, xend = max(windspeed)*1.1, y = 360, yend = 360), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = 360, label = "360'"), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::geom_segment(data = weather, ggplot2::aes(x = 0, xend = max(windspeed)*1.1, y = 1050, yend = 1050), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = 1050, label = "1050'"), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::geom_segment(data = weather, ggplot2::aes(x = 0, xend = max(windspeed)*1.1, y = 1640, yend = 1640), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = 1640, label = "1640'"), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::geom_segment(data = weather, ggplot2::aes(x = 0, xend = max(windspeed)*1.1, y = 2620, yend = 2620), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = 2620, label = "2620'"), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::geom_segment(data = weather, ggplot2::aes(x = 0, xend = max(windspeed)*1.1, y = 3280, yend = 3280), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = 3280, label = "3280'"), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::geom_segment(data = weather, ggplot2::aes(x = 0, xend = max(windspeed)*1.1, y = 4920, yend = 4920), colour = "grey", linetype = "dashed", size = 0.1) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = 4920, label = "4920'"), hjust=0, vjust=0, size = 5, colour = "grey") +

    ggplot2::scale_colour_gradient2(
      low='green',
      mid = 'green',
      high = 'red',
      midpoint = 8,
      na.value = 'red',
      limits = c(0,23)
    ) +
    ggplot2::scale_linewidth(
      range = c(1,15),
      limits = c(0,150)
    ) +
    ggplot2::geom_path(data = interp_wind, ggplot2::aes(x = y, y = x, linewidth = y, colour = y), alpha = 1, lineend="round") +

    ggplot2::geom_text(label = "\u2193", size = 10, ggplot2::aes(angle = -winddirection), hjust = 0, colour = "black") +
    ggplot2::geom_text(ggplot2::aes(label = windspeed), hjust=1.5, vjust=0, size = 5, colour = "#696969") +

    #Takeoff Line
    ggplot2::geom_segment(data = weather, ggplot2::aes(x = 0, xend = max(windspeed)*1.1, y = location$elevation, yend = location$elevation), colour = "black", size = 1, linetype = "dashed") +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = location$elevation, label = glue::glue("{location$takeoff_name} Takeoff")), hjust=0, vjust=-0.5, size = 5, colour = "black") +

    ggplot2::ggtitle(location$takeoff_name, unique(weather$time)) +

    ggplot2::labs(x = "windspeed (kph)",
                  caption = "Data: NOAA GFS") +

    ggplot2::theme(axis.line=ggplot2::element_blank(),
                   axis.ticks=ggplot2::element_blank(),
                   # axis.text.x=ggplot2::element_blank(),
                   # axis.title.x=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),
                   legend.position="none",
                   panel.background=ggplot2::element_blank(),
                   panel.border=ggplot2::element_blank(),
                   panel.grid.major=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank(),
                   plot.background=ggplot2::element_blank())

  chart

}
