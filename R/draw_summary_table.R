rotate_gt_column <-
  function(gt_table, rotation, target_column_index) {
    for (i in 1:length(rotation)) {
      gt_table <- gt_table |>
        tab_style(list(glue::glue(
          "transform: rotate({rotation[i]}deg);"
        )),
        locations = cells_body(columns = target_column_index, rows = i))
    }

    gt_table
  }


make_column_label <- function(weather_overview, altitude_selected){
  altitude_label <- weather_overview |>
    dplyr::group_by(altitude_name, altitude) |>
    dplyr::summarise() |>
    dplyr::mutate(altitude = round(altitude)) |>
    dplyr::mutate(altitude_label = glue::glue("{altitude_name} ({altitude})")) |>
    dplyr::filter(altitude_name==altitude_selected) |>
    pull(altitude_label)

  altitude_label
}

draw_summary_table <- function(weather_overview){

  pal <- scales::gradient_n_pal(colours = c("#11BD39", "#11BD39","#FF1830"),
                                values= c(0, 8, 25))

  table_data <- weather_overview |>
    dplyr::filter(!is.na(altitude_name)) |>
    dplyr::mutate(across(contains("windspeed"), round)) |>
    dplyr::select(date, hour, windspeed, winddirection, altitude_name) |>
    tidyr::pivot_wider(names_from = altitude_name,
                       values_from = c(windspeed, winddirection)) |>
    janitor::clean_names() |>
    mutate(icon_below_takeoff = "\u27A7",
           icon_above_takeoff = "\u27A7",
           icon_at_height = "\u27A7",
           hex_below_takeoff = pal(windspeed_below_takeoff),
           hex_above_takeoff = pal(windspeed_above_takeoff),
           hex_at_height = pal(windspeed_at_height)) |>
    tidyr::replace_na(list(hex_below_takeoff = "#FF1830",
                           hex_above_takeoff = "#FF1830",
                           hex_at_height = "#FF1830"))

  table_data |>
    dplyr::select(date, hour,
                  windspeed_below_takeoff, icon_below_takeoff, hex_below_takeoff,
                  windspeed_above_takeoff, icon_above_takeoff, hex_above_takeoff,
                  windspeed_at_height, icon_at_height, hex_at_height) |>
    dplyr::group_by(date) |>
    mutate(date = format(date, "%Y-%m-%d %A")) |>
    arrange(date, hour) |>
    mutate(hour = glue::glue("{hour}:00")) |>
    gt(id="two") |>
    tab_style(cell_text(color = "white"), cells_body(columns = dplyr::contains("windspeed"))) |>
    tab_style(cell_text(size = "xx-large"), locations = cells_body(columns = c(4,7,10))) |>
    #Below TO
    tab_style(cell_fill(color = from_column("hex_below_takeoff")),
              locations = cells_body(columns = c("windspeed_below_takeoff"))) |>
    # tab_style(cell_text(color = from_column("hex_below_takeoff")),
    #           locations = cells_body(columns = c("icon_below_takeoff"))) |>
    rotate_gt_column(table_data$winddirection_below_takeoff+90, 4) |>
    #Above TO
    tab_style(cell_fill(color = from_column("hex_above_takeoff")),
              locations = cells_body(columns = c("windspeed_above_takeoff"))) |>
    # tab_style(cell_text(color = from_column("hex_above_takeoff")),
    #           locations = cells_body(columns = c("icon_above_takeoff"))) |>
    rotate_gt_column(table_data$winddirection_above_takeoff+90, 7) |>
    #At Height
    tab_style(cell_fill(color = from_column("hex_at_height")),
              locations = cells_body(columns = c("windspeed_at_height"))) |>
    # tab_style(cell_text(color = from_column("hex_at_height")),
    #           locations = cells_body(columns = c("icon_at_height"))) |>
    rotate_gt_column(table_data$winddirection_at_height+90, 10) |>
    #Column titles
    gt::tab_spanner(make_column_label(weather_overview, "below takeoff"), c(3,4)) |>
    gt::tab_spanner(make_column_label(weather_overview, "above takeoff"), c(6,7)) |>
    gt::tab_spanner(make_column_label(weather_overview, "at height"), c(9,10)) |>
    #Format widths etc.
    gt::cols_hide(dplyr::contains("hex")) |>
    gt::cols_width(dplyr::contains("windspeed")~gt::px(85)) |>
    gt::cols_width(dplyr::contains("icon")~gt::px(30)) |>
    cols_label(everything() ~"") |>
    tab_style(
      style = cell_borders(
        sides = c("left"),
        weight = px(2)),
      locations = cells_body(
        columns = c(3,6,9)
      )
    ) |>
    tab_style(
      style = cell_borders(
        sides = c("left"),
        weight = px(2)),
      locations = cells_body(
        columns = c(3,6,9)
      )
    ) |>
    cols_align(
      align = "center",
      columns = dplyr::everything()
    ) |>
    opt_css(
      css = "
    .cell-output-display {
      overflow-x: unset !important;
    }
    div#two {
      overflow-x: unset !important;
      overflow-y: unset !important;
    }
    #two .gt_col_heading {
      position: sticky !important;
      top: 0 !important;
    }
    "
    )

}
