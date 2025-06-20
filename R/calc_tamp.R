utils::globalVariables(c("DATE", "TMAX", "TMIN", "MM", "TAIR", "TAVG",
                         "."))
#' Calculate the temperature amplitude from long term weather data
#'
#' @param wth a data frame containing long term weather data including
#'   columns of date of measurement (DATE), daily maximum temperature (TMAX),
#'   and daily minimum temperature (TMIN)
#'
#' @export
#'
calc_tamp <- function(wth){

  tamp <- wth |>
    within({
      # Create month column
      MM = format(DATE, "%m")
      # Calculate daily average temperature
      TAIR = (TMAX + TMIN)/2}) |>
    # Aggregate by month
    aggregate(TAIR ~ MM,
              FUN = \(.x) mean(.x, na.rm = TRUE)) |>
    # Pull out the monthly average temperature values
    with(TAIR) |>
    # Calculate half the difference between the highest and lowest monthly
    #   temperature:
    range() |>
    diff()

  return(tamp/2)
}
