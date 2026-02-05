# Function to interpolate colors based on wind speed
get_wind_colour <- function(speed, max_speed = wind_speed_red) {
  # Normalize speed to 0-1 scale
  normalized <- pmin(speed / max_speed, 1)

  # Define color stops matching your scale
  # 0 to 0.25: lightblue
  # 0.25 to 0.7: lightblue to green
  # 0.7 to 1: green to red

  if (normalized <= 0.25) {
    return('#ADD8E6')  # lightblue
  } else if (normalized <= 0.7) {
    # Interpolate between lightblue and green
    ratio <- (normalized - 0.25) / (0.7 - 0.25)
    return(colorRampPalette(c('#ADD8E6', '#00FF00'))(100)[round(ratio * 99) + 1])
  } else {
    # Interpolate between green and red
    ratio <- (normalized - 0.7) / (1 - 0.7)
    return(colorRampPalette(c('#00FF00', '#FF0000'))(100)[round(ratio * 99) + 1])
  }
}

weather_summary_table <- function(weather, speed_units, wind_speed_red_kph){

  wind_speed_red <- units_to_selected(wind_speed_red_kph, "kph", speed_units)

  weather <- weather |>
    rowwise() |>
    mutate(wind_colour = get_wind_colour(round(wind_gusts) , wind_speed_red)) |>
    ungroup()

  weather |>
    mutate(
      hour = ifelse(nchar(hour)==2, glue::glue("{hour}:00"), glue::glue("0{hour}:00")),
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
    select(date_formatted, hour, gust_indicator, wind_display, wind_gusts, wind_colour, day_image, temperature) |>
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
          list(visible = FALSE, targets = 5),
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
      'wind_colour',
      backgroundColor = DT::styleEqual(
        levels = unique(weather$wind_colour),
        values = unique(weather$wind_colour)
      ),
      backgroundClip = 'padding-box',
      border = '4px solid white'
    )
  }
