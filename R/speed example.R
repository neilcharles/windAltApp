library(gt)
library(glue)

#Make some example data
rowcount <- 500

example_data <- data.frame(
  labels = 1:rowcount,
  rotation = round(runif(rowcount, 0, 360), 0),
  colour = colorRampPalette(c("red","green"))(rowcount)
)

#Function to rotate a gt column by a vector of angles
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

#Draw the table
example_data |>
  gt() |>
    tab_style(cell_fill(color = from_column("colour")),
              locations = cells_body(columns = c("labels"))) |>
    rotate_gt_column(example_data$rotation, 1)
