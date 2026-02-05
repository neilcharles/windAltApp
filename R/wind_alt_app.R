library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(ragg)
library(bsicons)
library(waiter)
library(gt)
library(shinycssloaders)

addResourcePath('www', 'www/')

sites_alt <-
  readr::read_csv('https://raw.githubusercontent.com/neilcharles/uk_pg_sites/main/sites.csv') |>
  dplyr::filter(is.na(exclude))

cache_location <- "inst/data/cache/weather"

thematic_shiny(font = "auto")

wind_alt_app <- function(...) {

  ui <- function(request) {
    page_fixed(
      useWaiter(),
      waiter_preloader(
        html = waiter_loading_screen_html,
        color = "#FFFFFF",
        fadeout = 2000
      ),
      tags$head(
        tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "www/favicon.png"),
        tags$style(
      HTML(
        '
                .container, .container-fluid {
          max-width: 750px !important;
          margin: 0 auto;
        }

        p {
            font-family: "Nunito Sans";
        }

         .bslib-value-box .value-box-title {
            font-size: 1.1rem; !important;
         }

        table.dataTable tr.selected td, table.dataTable tr.selected {
        box-shadow: inset 0 0 0 9999px #2b4364 !important;
        }
     .top-bar {
        background-color: #ffffff;
        border-bottom: 1px solid #dee2e6;
        padding: 15px 0;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }

      .logo-container {
        text-align: center;
      }

      .logo-container img {
        max-width: 100%;
        height: 75px !important;
        height: auto;
      }

      .card{overflow: visible !important;}
      .card-body{overflow: visible !important;}
        '
      )
    ), includeHTML("html/googleanalytics.html")),
      theme = bs_theme(
        version = 5,
        fg = "rgb(99, 99, 105)",
        primary = "#2b4364",
        secondary = "#D7D7D9",
        success = "#52BD6F",
        info = "#2b4364",
        warning = "#F2B705",
        danger = "#D92344",
        base_font = font_google("Nunito Sans"),
        heading_font = font_google("Nunito Sans"),
        font_scale = 0.8,
        `enable-rounded` = TRUE,
        preset = "minty",
        bg = "#fff"
      ),

      # Allow dropdown to overflow card

      title = "flyable.uk",

    br(),

    card(
      layout_columns(
        col_widths = 12,
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h3("Select a Flying Site", style = "margin: 0;"),
          tags$img(
            src = "www/flyable-logo.png",
            height = "35px",
            alt = "Flyable Logo"
          )
        ),
        selectInput('uiSitePicker', NULL, c('', unique(
          sites_alt$takeoff_name
        )), multiple = FALSE)
      )
    ),
      conditionalPanel(
        condition = "input.uiSitePicker != ''",

        accordion(
          open = FALSE,
          accordion_panel(
            title = "Check Site Details",
            uiOutput('location_valuebox')
          )
        ),
        br(),
          shinyWidgets::radioGroupButtons(
            'uiForecastModel',
            NULL,
            choices = c(
              '
              <div style="font-weight:bold;">DWD ICON</div>
              <div style="font-size:0.75em;">Modern German model</div>
              ' =
              "DWD_ICON",
              '
              <div style="font-weight:bold;">MET Office</div>
              <div style="font-size:0.75em;">Modern British model</div>
              ' =
              "MET_OFFICE",
              '
              <div style="font-weight:bold;">NOAA GFS</div>
              <div style="font-size:0.75em;">Older US model (like XCWeather)</div>
              ' =
              "NOAA_GFS"
            ),
            selected = 'DWD_ICON',
            justified = TRUE,  # Spreads buttons to full width
            width = "100%"
          ),


        br(),

          navset_card_pill(
            id = "nav",
            nav_panel(
              title = "Summary",
              p("Click on an hour to jump to altitude detail"),
              div(
                style = "display: flex; justify-content: center;",
                withSpinner(
                  DT::dataTableOutput("weather_summary_table", width = 400),
                  type = 5, color = "#D7D7D9", size = 1
                )
              )
            ),
            nav_panel(title = "Hourly Detail",
                      layout_columns(
                        col_widths = c(4,1,7),
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
                          min = 8,
                          max = 20,
                          value = 12,
                          step = 1,
                          animate = FALSE
                        )
                      ),
                      input_switch("uiShowHighAlt", "Show High Altitude", FALSE),
                      hr(),

                      withSpinner(
                        plotOutput(
                          'wind_chart', width = "100%", height = 550
                        ), type = 5, color = "#D7D7D9", size = 1
                      )

                      ),
            nav_panel(
              title = "About",
              br(),
              h3("Who built this app?"),
              p(
                "Neil Charles, an average weekend warrior PG pilot who likes building techy stuff. I mostly fly in the Pennines, or more accurately, I mostly look after my kids, muck about building apps and occasionally fly in the Pennines."),
                p("You can contact me ",
                shiny::a(href = "https://linktr.ee/neilcharles", "here.")
              ),
              h3("How does the app work?"),
              p(
                "I built the Wind at Altitude app to make it easy to see wind speed and direction at the altitudes where paragliders mostly fly. The app uses data from Open Meteo and caches it for one hour. If the app's cache is older than an hour then it requests a new forecast from Open Meteo (which may be unchanged)."
              ),
              p(
                "The Wind at Altitude app is designed primarily to avoid pilots being surprised by unexpectedly strong winds and wind direction changes above take-off altitude because those can be dangerous and can be difficult see in a straightforward way using other weather forecasting sites."
              ),
              h3("Can you add more data?"),
              p(
                "Maybe but not if it would clutter the charts and risk confusing people. If you want incredibly rich data for XC planning, RASP is wonderful but Wind at Altitude is designed to sit between simple ground-level tourist forecasts and RASP to help you decide whether it will be flyable."
              ),
              h3("Can I switch to mph?"),
              p(
                "Yes. Look for the settings button on the right hand side and you can change a few settings there, including measurement units and calibrating the colour scale on the chart."
              ),
              h3("Can I make the app remember my settings?"),
              p(
                "When you change settings they're stored in the page URL. If you set it up the way you like it and then create a bookmark, next time you visit the bookmark, the site will be set to your choice of units and your favourite hill etc."
              )
            ),
            nav_spacer(),
            nav_menu(title = "Settings", align = "right", nav_item(
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
                )
              )
            ))
          )
      ),
      conditionalPanel(
        condition = "input.uiSitePicker == ''",

      hr(),
      h3("New!"),
      HTML("<ul>
        <li>We've moved to flyable.uk</li>
        <li>Much faster loading</li>
        <li>Met Office Forecasts</li>
        <li>Week ahead ground level summary</li>
        <li>Cloud cover and temperature at altitude</li>
        <li>Option in 'settings' to see higher altitudes</li>
        </ul>"),
      hr()
      ),
      p(
        "Weather data is provided for informational purposes only and no guarantees are made regarding accuracy, timeliness, or fitness for flight. Use this website at your own risk in combination with other sources and always perform your own on-site assessment before launching."
      ),
      hr()

    )
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
        showcase_layout = showcase_bottom(max_height = "250px"),
        p(
          glue::glue('{location()$takeoff_lat},{location()$takeoff_lon}')
        )
      )
    })

    weather <- reactive({
      validate(need(
        !is.null(input$uiSitePicker) & nchar(input$uiSitePicker) > 0,
        "Use the dropdown box to select a site."
      ),)

      # Check if recent cached forecast exists
      cache_age <-
        file.info(
          glue::glue(
            "{cache_location}/{janitor::make_clean_names(input$uiSitePicker)}.rds"
          )
        )$mtime

      if (!is.na(cache_age)) {
        if (cache_age > lubridate::now() - lubridate::hours(1)) {
          return(readr::read_rds(
            glue::glue(
              "{cache_location}/{janitor::make_clean_names(input$uiSitePicker)}.rds"
            )
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

        setProgress(message = 'Getting DWD-ICON ground level', value = 0.05)

        # Wind ground - DWD
        wind_ground_dwd <-
          get_weather_basic_new(lat, lon, forecast_service = "icon") |>
          tidyr::pivot_wider(names_from = metric, values_from = value) |>
          format_openmet()

        setProgress(message = 'Getting NOAA GFS ground level', value = 0.1)

        # Wind ground - GFS
        wind_ground_gfs <-
          get_weather_basic_new(lat, lon, forecast_service = "gfs") |>
          tidyr::pivot_wider(names_from = metric, values_from = value) |>
          format_openmet()

        setProgress(message = 'Getting Met Office ground level', value = 0.15)

        # Wind ground - Met Office
        wind_ground_mo <-
          get_weather_basic_new(lat, lon, forecast_service = "ukmo") |>
          tidyr::pivot_wider(names_from = metric, values_from = value) |>
          format_openmet()

        setProgress(message = 'Getting DWD-ICON at altitude', value = 0.2)

        # Wind at alt - DWD
        wind_dwd <-
          get_weather_at_altitude_new(lat, lon, forecast_service = "icon") |>
          dplyr::select(-metric) |>
          tidyr::pivot_wider(names_from = fact, values_from = value) |>
          format_openmet()

        setProgress(message = 'Getting NOAA GFS at altitude', value = 0.4)

        # Wind at alt - GFS
        wind_gfs <-
          get_weather_at_altitude_new(lat, lon, forecast_service = "gfs") |>
          dplyr::select(-metric) |>
          tidyr::pivot_wider(names_from = fact, values_from = value) |>
          format_openmet()

        setProgress(message = 'Getting Met Office at altitude', value = 0.6)

        # Wind at alt - Met Office
        wind_mo <-
          get_weather_at_altitude_new(lat, lon, forecast_service = "ukmo") |>
          dplyr::select(-metric) |>
          tidyr::pivot_wider(names_from = fact, values_from = value) |>
          format_openmet()

        setProgress(message = 'Getting Met Office at altitude', value = 0.6)

        # Weather Code - Met Office
        weather_code_mo <-
          get_weather_basic(lat, lon, "weather_code", "ukmo") |>
          dplyr::union_all(get_weather_basic(lat, lon, "temperature_2m", "ukmo")) |>
          tidyr::pivot_wider(names_from = metric, values_from = value) |>
          format_openmet()

        weather <- list(
          wind_ground_DWD_ICON = wind_ground_dwd,
          wind_ground_NOAA_GFS = wind_ground_gfs,
          wind_ground_MET_OFFICE = wind_ground_mo,
          wind_DWD_ICON = wind_dwd,
          wind_NOAA_GFS = wind_gfs,
          wind_MET_OFFICE = wind_mo
        )

        weather |>
          readr::write_rds(
            glue::glue(
              "{cache_location}/{janitor::make_clean_names(input$uiSitePicker)}.rds"
            )
          )

        weather
      })
    })

    altitude_limit <- reactive({
      if (!input$uiShowHighAlt) {
        return(units_to_selected(1500, "metres", input$uiAltitudeUnits))
      } else {
        return(units_to_selected(100000, "metres", input$uiAltitudeUnits))
      }
    })

    weather_selected_units <- reactive({
      weather_wind <- weather() |>
        purrr::map(
          ~ .x |>
            dplyr::mutate(
              dplyr::across(
                dplyr::contains(c("speed", "gust")),
                ~ units_to_selected(.x, "kph", input$uiSpeedUnits)
              ),
              dplyr::across(
                dplyr::contains(c("height", "elevation")),
                ~ units_to_selected(.x, "metres", input$uiAltitudeUnits)
              )
            ) |>
            dplyr::filter(hour >= 8, hour <= 20) |>
            dplyr::filter(geopotential_height <= altitude_limit())
        )

      weather_wind

    })

    weather_selected_date <- reactive({
      req(weather(), input$uiSitePicker)

      weather_selected_units() |>
        purrr::map( ~ .x |>
                      dplyr::filter(date == input$uiDatePicker))
    })

    weather_selected_hour <- reactive({
      weather_selected_date() |>
        purrr::map( ~ .x |>
                      dplyr::filter(hour == input$uiTimePicker))
    })

    weather_site_altitudes <- reactive({
      weather_forecast_alt <- weather_selected_units()[[glue::glue("wind_{input$uiForecastModel}")]]

      #Get altitudes for the pressure at midday for each day
      hour_takeoff_alt <- weather_forecast_alt |>
        dplyr::filter(hour == 12) |>
        tidyr::nest(data = -date) |>
        dplyr::mutate(site_altitudes = purrr::map(
          .x = data,
          .f = ~ get_site_altitudes(location(), .x)
        )) |>
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

      cht <- weather_selected_hour()[[glue::glue("wind_{input$uiForecastModel}")]] |>
        draw_wind_alt(
          location = location(),
          altitude_units = input$uiAltitudeUnits,
          speed_units = input$uiSpeedUnits,
          wind_speed_red_kph = input$uiColourRed,
          attribution = input$uiForecastModel
        )

      cht
    })

    output$mini_map <- leaflet::renderLeaflet({
      req(input$uiSitePicker)

      location() |>
        leaflet::leaflet(options = leaflet::leafletOptions(attributionControl =
                                                             FALSE)) |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) |>
        leaflet::addMarkers(lng = ~ takeoff_lon, lat = ~ takeoff_lat)

    })

    # Summary table ------------------------------------------------------------
    output$weather_summary_table <- DT::renderDT({
      req(weather_selected_units())

      tbl <- weather_summary_table(weather_selected_units()[[glue::glue("wind_ground_{input$uiForecastModel}")]],
                            speed_units = input$uiSpeedUnits,
                            wind_speed_red_kph = input$uiColourRed
                            )

      tbl
    })

    # Callback to switch to altitude chart -------------------------------------
    observeEvent(input$weather_summary_table_rows_selected, {

      req(input$weather_summary_table_rows_selected)

      # Get the selected row index
      selected_row <- input$weather_summary_table_rows_selected

      # Get the corresponding data from your weather data
      selected_data <- weather_selected_units()[[glue::glue("wind_ground_{input$uiForecastModel}")]][selected_row, ]

      # Update the date and time pickers
      updateDateInput(session, "uiDatePicker", value = selected_data$date)
      updateSliderInput(session, "uiTimePicker", value = selected_data$hour)

      # Switch to the Hourly Detail tab
      nav_select(id = "nav", selected = "Hourly Detail", session = session)
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
      updateSelectInput(session, "uiSitePicker", selected = state$input$uiSitePicker)
    })
    #---------------------------------------------------------------------------

  }

  shinyApp(ui = ui,
           server = server,
           enableBookmarking = "url")
}
