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

        layout_columns(
          col_widths = c(2,10),
          selectInput('uiSitePicker',
                      "Select Site",
                      c('', unique(
                        sites_alt$takeoff_name
                      )),
                      multiple = FALSE)
        ),


              card(
                layout_columns(
                col_widths = c(8, 4),
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
        nav_panel(
          title = "Weekly Overview",
          br(),
          layout_columns(
            col_widths = c(4,4,4),
          withSpinner(gt_output("summary_table1")),
          withSpinner(gt_output("summary_table2")),
          withSpinner(gt_output("summary_table3")))

        ),
        nav_spacer(),
        nav_menu(
          title = "Settings",
          align = "right",
          nav_item(
          card(
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
          )))
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
        ),
        br(),
        p("This app is new and still in active development. It may not be free from errors and it is essential that you also check an established weather forecast before flying.")
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

    weather_selected_units <- reactive({

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

      weather_wind

    })

    weather_selected_date <- reactive({
      req(weather(),
          input$uiSitePicker)

      weather_selected_units() |>
        purrr::map(~ .x |>
                     dplyr::filter(date == input$uiDatePicker))
    })

    weather_selected_hour <- reactive({
      weather_selected_date() |>
        purrr::map(~ .x |>
                     dplyr::filter(hour == input$uiTimePicker))
    })

    weather_site_altitudes <- reactive({

      weather_forecast_alt <- weather_selected_units()[[glue::glue("wind_{input$uiForecastModel}")]]

      #Get altitudes for the pressure at midday for each day
      hour_takeoff_alt <- weather_forecast_alt |>
        dplyr::filter(hour==12) |>
        tidyr::nest(data = -date) |>
        dplyr::mutate(site_altitudes = purrr::map(.x = data, .f = ~get_site_altitudes(location(), .x))) |>
        dplyr::select(-data) |>
        tidyr::unnest(site_altitudes)

      #Apply the midday altitudes to the entire forecast to allow drawing as a table
      weather_overview <- weather_forecast_alt |>
        dplyr::left_join(hour_takeoff_alt, by = c("pressure_alt", "date")) |>
        dplyr::filter(!is.na(altitude_name))

      weather_overview

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

    output$summary_table1<- render_gt({
      draw_summary_table(weather_site_altitudes() |>
                           dplyr::filter(date == min(weather_site_altitudes()$date)))
    })

    output$summary_table2<- render_gt({
      draw_summary_table(weather_site_altitudes() |>
                           dplyr::filter(date == min(weather_site_altitudes()$date)+1))
    })

    output$summary_table3<- render_gt({
      draw_summary_table(weather_site_altitudes() |>
                           dplyr::filter(date == min(weather_site_altitudes()$date)+2))
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
