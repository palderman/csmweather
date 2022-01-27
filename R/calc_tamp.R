utils::globalVariables(c("DATE", "TMAX", "TMIN", "MM", "TAIR", "TAVG",
                         "."))
#' Calculate the temperature amplitude from long term weather data
#'
#' @param wth a data frame containing long term weather data including
#'   columns of date of measurement (DATE), daily maximum temperature (TMAX),
#'   and daily minimum temperature (TMIN)
#'
#' @importFrom dplyr "%>%" mutate group_by summarize pull
#' @importFrom lubridate month
#'
#' @export
#'
calc_tamp <- function(wth){

  tamp <- wth %>%
    mutate(
      # Create month column
      MM = month(DATE),
      # Calculate daily average temperature
      TAIR = (TMAX + TMIN)/2) %>%
    # Group by month
    group_by(MM) %>%
    # Calculate monthly average temperature
    summarize(TAVG = mean(TAIR, na.rm = TRUE)) %>%
    # Pull out the monthly average temperature values
    pull(TAVG) %>%
    # Calculate half the difference between the highest and lowest monthly
    #   temperature:
    range() %>%
    diff() %>%
    {./2}

  return(tamp)
}
