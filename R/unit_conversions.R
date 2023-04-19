#' Convert wind speed from meters per second to kilometers per day
#'
#' @param m_s a vector of wind speed in meters per second
#'
#' @return a vector of wind speed in kilometers per day
#'
#' @export
#'
m_s_to_km_d <- function(m_s){
  return(m_s*86.4)
}

#' Convert degrees Fahrenheit to degrees Celsius
#'
#' @param t_f a vector of temperature in degrees Fahrenheit
#'
#' @return a vector of temperature in degrees Celsius
#'
#' @export
#'
f_to_c <- function(t_f){
  return((t_f-32)*5/9)
}
