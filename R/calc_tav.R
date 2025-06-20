utils::globalVariables(c("TMAX", "TMIN", "TAIR", "TAVG"))

#' Calculate the temperature average from long term weather data
#'
#' @param wth a data frame containing long term weather data including
#'   columns of date of measurement (DATE), daily maximum temperature (TMAX),
#'   and daily minimum temperature (TMIN)
#'
#' @export
#'
calc_tav <- function(wth){

  tav <- wth |>
    within({
      # Calculate daily average temperature
      TAIR = (TMAX + TMIN)/2}) |>
    with({
      mean(TAIR, na.rm = TRUE)
      })

  return(tav)
}
