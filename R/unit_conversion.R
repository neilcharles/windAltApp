units_to_selected <- function(data, data_units, dashboard_units) {

  if(data_units==dashboard_units) return(data)

  if (data_units == "kph") {
    if (dashboard_units == "mph")
      return(data / 1.61)
  }

  if (data_units == "mph") {
    if (dashboard_units == "kph")
      return(data * 1.61)
  }

  if (data_units == "feet") {
    if (dashboard_units == "metres")
      return(data / 3.28)
  }

  if (data_units == "metres") {
    if (dashboard_units == "feet")
      return(data * 3.28)
  }
}
