utils::globalVariables(c("TMAX", "TMIN", "TAIR", "TAVG"))

#' Calculate the temperature average from long term weather data
#'
#' @param wth a data frame containing long term weather data including
#'   columns of date of measurement (DATE), daily maximum temperature (TMAX),
#'   and daily minimum temperature (TMIN)
#'
#' @importFrom dplyr "%>%" mutate summarize pull
#' @importFrom lubridate month
#'
#' @export
#'
calc_tav <- function(wth){

  tav <- wth %>%
    mutate(
      # Calculate daily average temperature
      TAIR = (TMAX + TMIN)/2) %>%
    summarize(TAVG = mean(TAIR, na.rm = TRUE)) %>%
    # Pull out the monthly average temperature values
    pull(TAVG)

  return(tav)
}
