library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(ragg)
library(bsicons)
library(shinycssloaders)
library(gt)


sites_alt <-
  readr::read_csv('https://raw.githubusercontent.com/neilcharles/uk_pg_sites/main/sites.csv') |>
  dplyr::filter(is.na(exclude))

cache_location <- "inst/data/cache/weather"

thematic_shiny(font = "auto")

wind_alt_app <- function(...) {
  ui <- function(request) {
    page_fluid(
      tags$head(tags$style(
        HTML('p {font-family: "Nunito Sans"};')
      ),
      includeHTML("html/googleanalytics.html")
      ),

      theme = bs_theme(
        version = 5,
        fg = "rgb(99, 99, 105)",
        #primary = "#636369",
        #primary = "#83CDfB",
        primary = "#0568A6",
        secondary = "#D7D7D9",
        success = "#52BD6F",
        info = "#0568A6",
        warning = "#F2B705",
        danger = "#D92344",
        base_font = font_google("Nunito Sans"),
        heading_font = font_google("Nunito Sans"),
        font_scale = 0.8,
        `enable-rounded` = FALSE,
        preset = "litera",
        bg = "#fff"
      ),

      # Allow dropdown to overflow card

      title = "Wind At Altitude",

          navset_pill(
          nav_panel(title = "Hourly Chart",
                    card(
                      card_header(
                        layout_columns(
                          widths = c(4,4,4),

                          dateInput(
                            'uiDatePicker',
                            'Date',
                            min = lubridate::today(),
                            max = lubridate::today() + lubridate::days(6),
                            format = "yyyy-mm-dd DD"
                          ),
                        div(),
                        sliderInput(
                          'uiTimePicker',
                          'Time',
                          min = 0,
                          max = 23,
                          value = 12,
                          step = 1,
                          animate = FALSE
                        ))
                      ),
                      h6(
                        "This app is new and currently in testing. Data comes from OpenMeteo and currently approximates the location of takeoff, it is essential that you also check an established weather forecast."
                      ),

                      withSpinner(plotOutput('wind_chart', width = "100%", height = 550))

                    )),
          nav_panel(title = "Week Overview",
                      withSpinner(gt_output("wind_table"))
                    ),
          nav_spacer(),
          nav_menu(
            title = "Settings",
            align = "right",
            nav_panel(h6(icon("map-marker"), "Select Site"),

                      card(
                        card_header("Choose a site"),

                      selectInput('uiSitePicker',
                                  NULL,
                                  c('', unique(
                                    sites_alt$takeoff_name
                                  )),
                                  multiple = FALSE),
                      a("Email Me.", href = "mailto:neil.d.charles@gmail.com"),
                      hr(),
                      leaflet::leafletOutput('mini_map', width = "100%")

)



                      ),
            nav_item(
              card(
              radioButtons(
                "uiAltitudeUnits",
                "Altitudes",
                c("feet", "metres"),
                "feet"
              ),
                radioButtons("uiSpeedUnits", "Speeds", c("kph", "mph"), "kph"),
              sliderInput("uiColourRed", "Red Colour Limit (kph)", min = 15, max = 40, value = 25)
            ))

          )
        )
      )
  }

  server <- function(input, output, session) {
    output$site_picker <- renderUI({

    })

    location <- reactive({
      sites_alt |>
        dplyr::filter(takeoff_name == input$uiSitePicker) |>
        dplyr::mutate(elevation = units_to_selected(elevation, "metres", input$uiAltitudeUnits))
    })

    weather <- reactive({
      validate(need(
        !is.null(input$uiSitePicker) & nchar(input$uiSitePicker) > 0,
        "Use the top-right settings button to select a site."
      ),
      )

      # Check if recent cached forecast exists
      cache_age <-
        file.info(glue::glue("{cache_location}/{input$uiSitePicker}.rds"))$mtime

      if (!is.na(cache_age)) {
        if (cache_age > lubridate::now() - lubridate::hours(1)) {
          return(readr::read_rds(
            glue::glue("{cache_location}/{input$uiSitePicker}.rds")
          ))
        }
      }

      # Get weather
      withProgress(message = 'Getting Data...', value = 0.5, {
        lat <- location()$takeoff_lat
        lon <- location()$takeoff_lon

        # Wind
        wind <- get_weather_at_altitude(lat, lon, "windspeed") |>
          dplyr::union_all(get_weather_at_altitude(lat, lon, "winddirection")) |>
          dplyr::select(-metric) |>
          tidyr::pivot_wider(names_from = fact, values_from = value) |>
          dplyr::mutate(
            date = lubridate::date(time),
            hour = as.integer(stringr::str_extract(time, "(?<=\\T)([0-9][0-9])")),
            takeoff_name = location()$takeoff_name
          )

        weather <- list(wind = wind)

        weather |>
          readr::write_rds(glue::glue("{cache_location}/{input$uiSitePicker}.rds"))

        weather
      })
    })


    output$wind_chart <- renderPlot({
      req(weather(),
          input$uiSitePicker)

      validate(need(
        unique(weather()$wind$takeoff_name) == unique(location()$takeoff_name),
        "Click 'Get Site Forecast' to refresh."
      ))

      weather_wind <- weather()$wind |>
        dplyr::mutate(
          windspeed = units_to_selected(windspeed, "kph", input$uiSpeedUnits),
          altitude = units_to_selected(alt_m, "metres", input$uiAltitudeUnits)
        )

      #Convert time picker hour to the format used by open meteo
      hour <- ifelse(
        nchar(as.character(input$uiTimePicker)) == 1,
        glue::glue("0{as.character(input$uiTimePicker)}"),
        as.character(input$uiTimePicker)
      )

      date_time <- glue::glue("{input$uiDatePicker}T{hour}:00")

      weather_wind |>
        dplyr::filter(time == date_time) |>
        draw_wind_alt(
          location = location(),
          altitude_units = input$uiAltitudeUnits,
          speed_units = input$uiSpeedUnits,
          wind_speed_red_kph = input$uiColourRed
        )
    })

    output$wind_table <- render_gt({

      weather()$wind |>
        filter(hour > 5 & hour < 21) |>
        mutate(date = format(date, "%Y-%m-%d %A")) |>
        left_join(pressure_altitudes()) |>
        select(date, hour, windspeed, alt_m) |>
        mutate(
          hour = glue::glue("{hour}:00"),
          altitude = round(units_to_selected(alt_m, "metres", input$uiAltitudeUnits)/10)*10,
          windspeed = round(units_to_selected(windspeed, "kph", input$uiSpeedUnits))) |>
          select(-alt_m) |>
        pivot_wider(names_from = altitude, values_from = windspeed) |>
        group_by(date) |>
        gt(id = "windtable", rowname_col = "hour") |>
        data_color(columns = everything(),
                   palette = c("green", "red"),
                   domain = c(0, units_to_selected(input$uiColourRed, "kph", input$uiSpeedUnits)),
                   na_color = "red") |>
      tab_spanner(glue::glue("Windspeed ({input$uiSpeedUnits}) at Altitude ({input$uiAltitudeUnits})"), columns = everything()) |>
        opt_css(
          css = "
    .cell-output-display {
      overflow-x: unset !important;
    }
    div#windtable {
      overflow-x: unset !important;
      overflow-y: unset !important;
    }
    #windtable .gt_col_heading {
      position: sticky !important;
      top: 0 !important;
       z-index: 1;
    }"
        )
    })

    output$mini_map <- leaflet::renderLeaflet({
      req(input$uiSitePicker)

      location() |>
        leaflet::leaflet(options = leaflet::leafletOptions(attributionControl =
                                                             FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) |>
        leaflet::addMarkers(lng = ~ takeoff_lon, lat = ~ takeoff_lat)

    })

    #Stop on close
    # session$onSessionEnded(function() {
    #   stopApp()
    # })

    #Bookmark URL --------------------------------------------------------------
    observe({
      reactiveValuesToList(input)
      session$doBookmark()
    })

    onBookmarked(updateQueryString)

    setBookmarkExclude(
      c(
        'uiDatePicker',
        'uiTimePicker',
        'mini_map_zoom',
        'mini_map_center',
        'mini_map_bounds',
        'uiGetWeather',
        'uiSitePicker_open'
      )
    )

    onRestored(function(state) {
      updateSelectInput(session,
                        "uiSitePicker",
                        selected = state$input$uiSitePicker)
    })
    #---------------------------------------------------------------------------

  }

  shinyApp(ui = ui,
           server = server,
           enableBookmarking = "url")
}
