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
      tags$head(
        tags$style(HTML('p {font-family: "Nunito Sans"}
                        .bslib-value-box .value-box-title {font-size: 1.1rem; !important}')),
        includeHTML("html/googleanalytics.html")
      ),

      theme = bs_theme(
        version = 5,
        fg = "rgb(99, 99, 105)",
        primary = "#0568A6",
        secondary = "#D7D7D9",
        success = "#52BD6F",
        info = "#0568A6",
        warning = "#F2B705",
        danger = "#D92344",
        base_font = font_google("Nunito Sans"),
        heading_font = font_google("Nunito Sans"),
        font_scale = 0.8,
        `enable-rounded` = TRUE,
        preset = "cosmo",
        bg = "#fff"
      ),

      # Allow dropdown to overflow card

      title = "Wind At Altitude",

      layout_sidebar(
        sidebar = sidebar(
          selectInput('uiSitePicker',
                      "Select Site",
                      c('', unique(
                        sites_alt$takeoff_name
                      )),
                      multiple = FALSE),
          hr(),
          radioButtons(
            "uiAltitudeUnits",
            "Altitudes",
            c("feet", "metres"),
            "feet"
          ),
          radioButtons("uiSpeedUnits", "Speeds", c("kph", "mph"), "kph"),
          sliderInput(
            "uiColourRed",
            "Red Colour Limit (kph)",
            min = 15,
            max = 40,
            value = 25
          )
        ),

        h6(
          "This app is new and still in active development. It may not be free from errors and it is essential that you also check an established weather forecast before flying."
        ),


              card(
                layout_columns(
                widths = c(8, 4),
                uiOutput('location_valuebox'),
                div(
                  radioButtons(
                    'uiForecastModel',
                    'Forecast Model',
                    choices = c('DWD ICON' = "DWD_ICON", 'NOAA GFS' = "NOAA_GFS"),
                    selected = 'DWD_ICON'
                  ),

                dateInput(
              'uiDatePicker',
              'Date',
              min = lubridate::today(),
              max = lubridate::today() + lubridate::days(6),
              format = "yyyy-mm-dd DD"
            ),
            sliderInput(
              'uiTimePicker',
              'Time',
              min = 8,
              max = 20,
              value = 12,
              step = 1,
              animate = FALSE
            ))
          )
      ),

      navset_pill(
        id = "nav",
        nav_panel(title = "Hourly Detail",
                  card(
                    card_header("placeholder"),
                    withSpinner(plotOutput(
                      'wind_chart', width = "100%", height = 550
                    ))

                  )),
        nav_panel(title = "Ground Level",
                  card(
                  card_header('Ground level wind estimated by DWD-ICON & GFS forecast models based on average terrain height'),
                  layout_column_wrap(
                    width = "400px",

                  card(
                    card_header(
                      h3('Wind at 10m AGL'),
                      p("Base windspeed at ground level")
                    ),
                    windValueBoxUI("wind10m")
                  ),
        card(
          card_header(
            h3('Gusts at 10m AGL'),
            p("Wind gusts at ground level"),
            windValueBoxUI("gusts10m")

          )
        )))),
        nav_panel(title = "Site Altitude",
                  card(
                    card_header('Wind at altitudes close to takeoff height estimated by DWD-ICON & GFS forecast models'),
                    layout_column_wrap(
                      width = "400px",

                      card(
                        fill = FALSE,
                        card_header(
                          h3('Wind on the hill'),
                          p('Nearest available forecast at or below takeoff altitude')
                        ),
                        windValueBoxUI("windLow"),
                      ),
                      card(
                        card_header(
                          h3('Wind above the hill'),
                          p('Nearest available forecast above takeoff altitude')
                        ),
                        windValueBoxUI("windMid")
                      ),
                      card(
                        card_header(
                          h3('Wind at height'),
                          p('Forecast at two modelled pressure levels above takeoff altitude')
                        ),
                        windValueBoxUI("windHigh")
                      )
                    )
                  )),
        # nav_panel(
        #   title = "Weekly Overview",
        #   br(),
        #   h6(
        #     "Windspeeds from approximately 300' to 5000'. Altitudes change as air pressure changes so use this table for a rough overview of the week and the hourly chart to get speeds at precise altitudes.",
        #     style = "text-align:center"
        #   ),
        #   withSpinner(gt_output("wind_table"))
        # )
      )
    ))
  }

  server <- function(input, output, session) {
    location <- reactive({
      sites_alt |>
        dplyr::filter(takeoff_name == input$uiSitePicker) |>
        dplyr::mutate(elevation = units_to_selected(elevation, "metres", input$uiAltitudeUnits))
    })

    output$location_valuebox <- renderUI({
      value_box(
        title = glue::glue(
          '{round(location()$elevation)} {input$uiAltitudeUnits}'
        ),
        value = location()$takeoff_name,
        showcase = leaflet::leafletOutput('mini_map'),
        showcase_layout = showcase_left_center(max_height = "250px"),
        p(
          glue::glue('{location()$takeoff_lat},{location()$takeoff_lon}')
        )
      )
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

      format_openmet <- function(data) {
        data |>
          dplyr::mutate(
            date = lubridate::date(time),
            hour = as.integer(stringr::str_extract(time, "(?<=\\T)([0-9][0-9])")),
            takeoff_name = location()$takeoff_name
          )
      }

      withProgress(value = 0.0, {
        lat <- location()$takeoff_lat
        lon <- location()$takeoff_lon

        setProgress(message = 'Getting DWD-ICON ground level', value = 0.1)

        # Wind ground - DWD
        wind_ground_dwd <-
          get_weather_at_10m(lat, lon, "windspeed_10m", "dwd-icon") |>
          dplyr::union_all(get_weather_at_10m(lat, lon, "winddirection_10m", "dwd-icon")) |>
          dplyr::union_all(get_weather_at_10m(lat, lon, "wind_gusts_10m", "dwd-icon")) |>
          tidyr::pivot_wider(names_from = metric, values_from = value) |>
          format_openmet()

        setProgress(message = 'Getting NOAA GFS ground level', value = 0.2)

        # Wind ground - GFS
        wind_ground_gfs <-
          get_weather_at_10m(lat, lon, "windspeed_10m", "gfs") |>
          dplyr::union_all(get_weather_at_10m(lat, lon, "winddirection_10m", "gfs")) |>
          dplyr::union_all(get_weather_at_10m(lat, lon, "wind_gusts_10m", "gfs")) |>
          tidyr::pivot_wider(names_from = metric, values_from = value) |>
          format_openmet()

        setProgress(message = 'Getting DWD-ICON at altitude', value = 0.3)

        # Wind at alt - DWD
        wind_dwd <-
          get_weather_at_altitude(lat, lon, "windspeed", "dwd-icon") |>
          dplyr::union_all(get_weather_at_altitude(lat, lon, "winddirection", "dwd-icon")) |>
          dplyr::union_all(get_weather_at_altitude(lat, lon, "geopotential_height", "dwd-icon")) |>
          dplyr::select(-metric) |>
          tidyr::pivot_wider(names_from = fact, values_from = value) |>
          format_openmet()

        setProgress(message = 'Getting NOAA GFS at altitude', value = 0.6)

        # Wind at alt - GFS
        wind_gfs <-
          get_weather_at_altitude(lat, lon, "windspeed", "gfs") |>
          dplyr::union_all(get_weather_at_altitude(lat, lon, "winddirection", "gfs")) |>
          dplyr::union_all(get_weather_at_altitude(lat, lon, "geopotential_height", "gfs")) |>
          dplyr::select(-metric) |>
          tidyr::pivot_wider(names_from = fact, values_from = value) |>
          format_openmet()

        weather <- list(wind_ground_DWD_ICON = wind_ground_dwd,
                        wind_ground_NOAA_GFS = wind_ground_gfs,
                        wind_DWD_ICON = wind_dwd,
                        wind_NOAA_GFS = wind_gfs)

        weather |>
          readr::write_rds(glue::glue("{cache_location}/{input$uiSitePicker}.rds"))

        weather
      })
    })

    weather_selected_date <- reactive({
      req(weather(),
          input$uiSitePicker)

      weather_wind <- weather() |>
        purrr::map(
          ~ .x |>
            dplyr::mutate(
              dplyr::across(
                dplyr::contains(c("speed", "gust")),
                ~units_to_selected(.x, "kph", input$uiSpeedUnits)
              ),
              dplyr::across(
                dplyr::contains(c("height", "elevation")),
                ~units_to_selected(.x, "metres", input$uiAltitudeUnits)
              )
            ) |>
            dplyr::filter(hour >= 8,
                          hour <= 20)
        )

      weather_wind |>
        purrr::map(~ .x |>
                     dplyr::filter(date == input$uiDatePicker))
    })

    weather_selected_hour <- reactive({
      weather_selected_date() |>
        purrr::map(~ .x |>
                     dplyr::filter(hour == input$uiTimePicker))
    })

    weather_site_altitudes <- reactive({
      #Get the pressure altitudes around takeoff for selected hour
      hour_takeoff_alt <- weather_selected_hour() |>
        purrr::map(
          ~ .x |>
            left_join(get_site_altitudes(location(), .x), by = c("pressure_alt")) |>
            select(pressure_alt, altitude_name) |>
            tidyr::drop_na()
        )

      #Apply best fit pressure altitudes to the whole day
      date_takeoff_alt <- weather_selected_date() |>
        purrr::map2(.y = hour_takeoff_alt,
                    ~ .x |>
                      left_join(.y, by = "pressure_alt"))

      date_takeoff_alt

    })


    output$wind_chart <- renderPlot({

      req(weather_selected_hour())

      weather_selected_hour()[[glue::glue("wind_{input$uiForecastModel}")]] |>
        draw_wind_alt(
          location = location(),
          altitude_units = input$uiAltitudeUnits,
          speed_units = input$uiSpeedUnits,
          wind_speed_red_kph = input$uiColourRed,
          attribution = input$uiForecastModel
        )
    })

    output$wind_table <- render_gt({
      weather()[["wind_dwd"]] |>
        filter(hour > 5 & hour < 21) |>
        mutate(date = format(date, "%Y-%m-%d %A")) |>
        select(date, hour, windspeed, pressure_alt) |>
        mutate(
          hour = glue::glue("{hour}:00"),
          windspeed = round(
            units_to_selected(windspeed, "kph", input$uiSpeedUnits)
          )
        ) |>
        pivot_wider(names_from = pressure_alt, values_from = windspeed) |>
        group_by(date) |>
        gt(id = "windtable", rowname_col = "hour") |>
        data_color(
          columns = everything(),
          palette = c("green", "red"),
          domain = c(
            0,
            units_to_selected(input$uiColourRed, "kph", input$uiSpeedUnits)
          ),
          na_color = "red"
        ) |>
        tab_spanner(glue::glue("Windspeed ({input$uiSpeedUnits}) at Altitude"),
                    columns = everything()) |>
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

    # Low level wind value boxes
    windValueBoxServer(
      "windLow",
      reactive(
        filter(
          weather_site_altitudes()[[glue::glue("wind_{input$uiForecastModel}")]],
          altitude_name == "below takeoff"
        )
      ),
      reactive(input$uiTimePicker),
      reactive(units_to_selected(input$uiColourRed, "kph", input$uiSpeedUnits)),
      reactive(input$uiAltitudeUnits),
      reactive(input$uiSpeedUnits),
      input$uiForecastModel,
      width = session$clientData[['output_windLow-wind_plot_width']]
    )

    # Mid level wind value boxes
    windValueBoxServer(
      "windMid",
      reactive(
        filter(
          weather_site_altitudes()[[glue::glue("wind_{input$uiForecastModel}")]],
          altitude_name == "above takeoff"
        )
      ),
      reactive(input$uiTimePicker),
      reactive(units_to_selected(input$uiColourRed, "kph", input$uiSpeedUnits)),
      reactive(input$uiAltitudeUnits),
      reactive(input$uiSpeedUnits),
      input$uiForecastModel,
      width = session$clientData[['output_windMid-wind_plot_width']]
    )

    # High level wind value boxes
    windValueBoxServer(
      "windHigh",
      reactive(
        filter(
          weather_site_altitudes()[[glue::glue("wind_{input$uiForecastModel}")]],
          altitude_name == "at height"
        )
      ),
      reactive(input$uiTimePicker),
      reactive(units_to_selected(input$uiColourRed, "kph", input$uiSpeedUnits)),
      reactive(input$uiAltitudeUnits),
      reactive(input$uiSpeedUnits),
      input$uiForecastModel,
      width = session$clientData[['output_windHigh-wind_plot_width']]
    )

    # 10m wind value boxes
    windValueBoxServer(
      "wind10m",
      reactive(
        weather_site_altitudes()[[glue::glue("wind_ground_{input$uiForecastModel}")]],
      ),
      reactive(input$uiTimePicker),
      reactive(units_to_selected(input$uiColourRed, "kph", input$uiSpeedUnits)),
      reactive(input$uiAltitudeUnits),
      reactive(input$uiSpeedUnits),
      input$uiForecastModel,
      width = session$clientData[['output_wind10m-wind_plot_width']],
      override_altitude = TRUE
    )

    # 10m gust value boxes
    windValueBoxServer(
      "gusts10m",
      reactive(
        weather_site_altitudes()[[glue::glue("wind_ground_{input$uiForecastModel}")]] |>
          dplyr::mutate(windspeed = wind_gusts)
      ),
      reactive(input$uiTimePicker),
      reactive(units_to_selected(input$uiColourRed, "kph", input$uiSpeedUnits)),
      reactive(input$uiAltitudeUnits),
      reactive(input$uiSpeedUnits),
      input$uiForecastModel,
      width = session$clientData[['output_gusts10m-wind_plot_width']],
      override_altitude = TRUE
    )


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
