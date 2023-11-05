get_site_altitudes <- function(location, weather){

  available_altitudes <- unique(weather$geopotential_height)
  takeoff_altitude <- location$elevation

  available_differences <- available_altitudes - takeoff_altitude

  #Find the nearest available forecast altitudes to takeoff alt
  below_takeoff <- which(available_differences==max(available_differences[available_differences < 0]))
  above_takeoff <- which(available_differences==min(available_differences[available_differences > 0]))

  return(tibble::tibble(
    altitude_name = c('below takeoff', 'above takeoff', 'at height'),
    pressure_alt = c(weather$pressure_alt[below_takeoff],
                     weather$pressure_alt[above_takeoff],
                     weather$pressure_alt[above_takeoff+1]),
    altitude = c(available_altitudes[below_takeoff],
                 available_altitudes[above_takeoff],
                 available_altitudes[above_takeoff+1]),
    altitude_difference = c(available_differences[below_takeoff],
                            available_differences[above_takeoff],
                            available_differences[above_takeoff+1])
  ))

}
