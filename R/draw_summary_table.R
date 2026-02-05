weather_summary_table <- function(weather, speed_units){
  weather |>
    mutate(
      wind_display = glue::glue(
        '<div style="display:flex; align-items:center; justify-content:flex-end;">',
        '<span style="display:inline-block; transform:rotate({winddirection + 90}deg); font-size:30px; margin-right:8px;">➧</span>',
        '<div style="text-align:left;">',
        '<div style="font-weight:bold;">{round(windspeed)}{speed_units}</div>',
        '<div style="font-size:0.75em;">max {round(wind_gusts)}{speed_units}</div>',
        '</div>',
        '</div>'
      ),
      gust_indicator = "",  # Empty column for color indicator
      temperature = glue::glue("{round(temperature, 0)}\u00B0c"),
      date_formatted = format(date, "%Y-%m-%d %A")
    ) |>
    left_join(select(wmo_codes(), code, day_image, day_description), by = c("weather_code" = "code")) |>
    mutate(day_image = paste0('<img src="', day_image, '" height="50" title="', day_description, '"></img>')) |>
    mutate(date_formatted = format(date, "%Y-%m-%d %A")) |>
    arrange(date, hour) |>
    select(date_formatted, hour, gust_indicator, wind_display, wind_gusts, day_image, temperature) |>
    DT::datatable(
      extensions = 'RowGroup',
      options = list(
        paging = FALSE,
        searching = FALSE,
        dom = 't',
        rowGroup = list(dataSrc = 0),
        columnDefs = list(
          list(visible = FALSE, targets = 0),
          list(visible = FALSE, targets = 4),
          list(width = '10px', targets = 2)
        ),
        headerCallback = DT::JS(
          "function(thead, data, start, end, display){",
          "  $(thead).remove();",
          "}"
        )
      ),
      class = 'row-border compact',
      rownames = FALSE,
      escape = FALSE,
      selection = 'single'
    ) |>
    DT::formatStyle(
      columns = 1:ncol(weather),
      verticalAlign = 'middle',
      padding = '2px 5px',
      lineHeight = '1',
      fontSize = '16px'
    ) |>
    DT::formatStyle(
      'hour',
      textAlign = 'left'
    ) |>
    DT::formatStyle(
      'gust_indicator',
      'wind_gusts',
      backgroundColor = DT::styleInterval(
        cuts = c(15, 25, 35, 45),
        values = c('#d4edda', '#fff3cd', '#ffeaa7', '#fab1a0', '#ff7675')
      ),
      backgroundClip = 'padding-box',
      border = '4px solid white'
    )
  }
