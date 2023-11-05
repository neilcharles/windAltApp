windValueBoxUI <- function(id){
  bslib::value_box(
    title = textOutput(NS(id, "forecast_model")),
    value = uiOutput(NS(id, "wind_value")),
    showcase = plotOutput(NS(id, "wind_plot")),
    showcase_layout = showcase_left_center(width = 0.7),
    full_screen = TRUE,
    theme_color = "secondary",
    textOutput(NS(id, "forecast_windspeed_units")),
    textOutput(NS(id, "forecast_altitude"))
  )
}

windValueBoxServer <- function(id, weather, selected_hour, wind_speed_red_kph = 25, altitude_units, speed_units, forecast_model){
  shiny::moduleServer(id, function(input, output, session){

    weather_formatted <- shiny::reactive({

      plot_data <- weather()

      pal <- scales::gradient_n_pal(colours = c("darkgreen","darkgreen","red"),
                                    values= c(0, wind_speed_red_kph()/3, wind_speed_red_kph()))

      plot_data$hex <- pal(plot_data$windspeed)

      plot_data <- plot_data |>
        tidyr::replace_na(list(hex = "red"))

      plot_data$border <- ifelse(plot_data$hour==selected_hour(), 3, 1)
      plot_data$border_colour <- ifelse(plot_data$hour==selected_hour(), "white", "black")

      plot_data

    })

    weather_selected_hour <- shiny::reactive({
      weather_formatted() |>
        dplyr::filter(hour==selected_hour())
    })

    output$forecast_model <- renderText({
      forecast_model
    })

    output$forecast_altitude <- renderText({
      glue::glue("{round(weather_selected_hour()$geopotential_height/10)*10} {altitude_units()}")
    })

    output$forecast_windspeed_units <- renderText({
      glue::glue("windspeed ({speed_units()})")
    })

    output$wind_value <- renderUI(
      shiny::p(round(weather_selected_hour()$windspeed), style = paste0("color: ", weather_selected_hour()$hex))
    )

    output$wind_plot <- renderPlot({

      plot <-
        ggplot2::ggplot(data = weather_formatted(), aes(x = hour, y = 1)) +
        ggplot2::scale_colour_gradient2(
          low='darkgreen',
          mid = 'darkgreen',
          high = 'red',
          midpoint = wind_speed_red_kph()/3,
          na.value = 'red',
          limits = c(0, wind_speed_red_kph())
        ) +
        ggplot2::geom_text(label = "\u27A7", size = 20, ggplot2::aes(angle = -winddirection - 90, colour = windspeed, hjust = 0.5, vjust = 0.5), alpha = 0.6) +
        ggplot2::geom_text(aes(label = round(windspeed)), size = 5, colour = "black", hjust = 0.5, vjust = -1.5, alpha = 1) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 2)) +
        ggplot2::scale_x_continuous(breaks = c(8, 12, 16, 20), labels = c('0800', '1200', '1600', '2000')) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position="none",
                       axis.line.x = ggplot2::element_line(colour = "#626E81"),
                       axis.ticks.x = ggplot2::element_line(colour = "#626E81", linewidth = 1),
                       axis.text.x = ggplot2::element_text(colour = "#626E81", size = 12),
                       axis.ticks.length.x = unit(2, "pt"))

      plot
    })
  })
}
