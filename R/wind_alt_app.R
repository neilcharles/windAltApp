library(shiny)
library(bslib)
library(thematic)
library(ragg)

sites_alt <-
  readr::read_csv('https://raw.githubusercontent.com/neilcharles/uk_pg_sites/main/sites.csv') |>
  dplyr::filter(is.na(exclude))

cache_location <- "inst/data/cache"

thematic_shiny(font = "auto")

wind_alt_app <- function(...) {
  ui <- function(request) {
    page_fluid(
      tags$head(tags$style(
        HTML('p {font-family: "Nunito Sans"};')
      )),

      theme = bs_theme(
        version = 5,
        fg = "rgb(99, 99, 105)",
        primary = "#0568A6",
        secondary = "#D7D7D9",
        success = "#52BD6F",
        info = "#83CDFB",
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

      titlePanel(title = ""),

      layout_columns(
        col_widths = c(8, 4),
        uiOutput("wind_chart_box"),

        card(
          card_header("Select Site"),
          fill = FALSE,
          layout_columns(
            col_widths = c(6, 6),
            uiOutput('site_picker'),
            actionButton('uiGetWeather', 'Get Site Forecast')
          ),
          p(
            "This app is new and currently in testing. Please do not use it as your only source of information."
          ),
          a("Email Me.", href = "mailto:neil.d.charles@gmail.com"),
          hr(),
          leaflet::leafletOutput('mini_map', width = "100%")
        )
      )
    )
  }

  server <- function(input, output, session) {
    output$date_picker <- renderUI({
      dateInput(
        'uiDatePicker',
        'Date',
        min = min(weather()$date),
        max = max(weather()$date)
      )
    })

    output$site_picker <- renderUI({
      shinyWidgets::pickerInput(
        'uiSitePicker',
        NULL,
        unique(sites_alt$takeoff_name),
        multiple = FALSE,
        options = list(container = "body", `live-search` = TRUE)
      )
    })

    output$time_picker <- renderUI({
      sliderInput(
        'uiTimePicker',
        'Time',
        min = min(weather()$hour),
        max = max(weather()$hour),
        value = 12,
        step = 1
      )
    })

    location <- reactive({
      sites_alt |>
        dplyr::filter(takeoff_name == input$uiSitePicker) |>
        dplyr::mutate(elevation = elevation * 3.28)
    })

    weather <- eventReactive(input$uiGetWeather, {
      withProgress(message = 'Getting Data...', value = 0.5, {

        #Check if recent cached forecast exists
        cache_age <-
          file.info(glue::glue(
            "{cache_location}/gfs_wind_alt/{input$uiSitePicker}.rds"
          ))$mtime

        if (!is.na(cache_age)) {
          if (cache_age > lubridate::now() - lubridate::hours(1)) {
            return(readr::read_rds(
              glue::glue(
                "{cache_location}/gfs_wind_alt/{input$uiSitePicker}.rds"
              )
            ))
          }
        }

        get_wind_alt(location()$takeoff_lat, location()$takeoff_lon) |>
          dplyr::select(-name) |>
          tidyr::separate(fact, c('variable', 'pressure_alt'), sep = "_") |>
          tidyr::pivot_wider(names_from = variable, values_from = value) |>
          dplyr::mutate(
            alt = dplyr::case_when(
              pressure_alt == "1000hPa" ~ 110,
              pressure_alt == "975hPa" ~ 320,
              pressure_alt == "950hPa" ~ 500,
              pressure_alt == "925hPa" ~ 800,
              pressure_alt == "900hPa" ~ 1000,
              pressure_alt == "850hPa" ~ 1500
            ),
            windspeed = round(windspeed, 0)
          ) |>
          dplyr::mutate(alt_feet = 3.28 * alt) |>
          dplyr::mutate(
            date = lubridate::date(time),
            hour = as.integer(stringr::str_extract(time, "(?<=\\T)([0-9][0-9])")),
            takeoff_name = location()$takeoff_name
          ) |>
          readr::write_rds(glue::glue("{cache_location}/gfs_wind_alt/{input$uiSitePicker}.rds"))
      })

    })

    output$wind_chart <- renderPlot({
      validate(need(
        unique(weather()$takeoff_name) == unique(location()$takeoff_name),
        "Click 'Get Site Forecast' to refresh."
      ))

      req(input$uiTimePicker)

      hour <- ifelse(
        nchar(as.character(input$uiTimePicker)) == 1,
        glue::glue("0{as.character(input$uiTimePicker)}"),
        as.character(input$uiTimePicker)
      )

      date_time <- glue::glue("{input$uiDatePicker}T{hour}:00")

      weather() |>
        dplyr::filter(time == date_time) |>
        draw_wind_alt(location = location())
    })

    output$wind_chart_box <- renderUI({
      req(weather())

      tagList(
        card(
          card_header("Select Date and Time"),
          uiOutput('date_picker'),
          uiOutput('time_picker')
        ),

        card(
          card_header("Wind Forecast"),
          full_screen = TRUE,
          plotOutput('wind_chart', width = "100%", height = 550)
        )
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
