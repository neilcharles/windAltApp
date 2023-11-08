library(gt)
library(gtExtras)

pal <- scales::gradient_n_pal(colours = c("darkgreen","darkgreen","red"),
                                     values= c(0, 8, 25))

write_rds(dwd, "temp.rds")

table_data <- dwd |>
  dplyr::filter(!is.na(altitude_name)) |>
  dplyr::select(date, hour, windspeed, winddirection, altitude_name) |>
  tidyr::pivot_wider(names_from = altitude_name,
                     values_from = c(windspeed, winddirection)) |>
  janitor::clean_names() |>
  mutate(icon = "\u27A7",
         hex_below_takeoff = pal(windspeed_below_takeoff)) |>
  tidyr::replace_na(list(hex_below_takeoff = "#FF0000"))


# table_data <- tibble::tibble(icon = c("\u27A7", "\u27A7", "\u27A7"), rotation = c(0, 90, 180))

table_data |>
  dplyr::select(date, hour, windspeed_below_takeoff, icon, hex_below_takeoff) |>
  gt() |>
  tab_style(cell_text(size = "xx-large"),
            locations = cells_body(columns = c(4))) |>
  tab_style(cell_text(color = from_column("hex_below_takeoff")),
            locations = cells_body(columns = c(3,4))) |>
  rotate_gt_column(table_data$winddirection_below_takeoff+90, 4) |>
  gt::cols_hide("hex_below_takeoff") |>
  gt::tab_spanner("On the Hill", c(3,4)) |>
  gt::tab_spanner("Hour", c(2)) |>
  cols_label(everything() ~"")
  # tab_options(column_labels.hidden = TRUE)



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
