#' Adjust wind speed to 2 meter height
#'
#' Adjust wind speed measured at an arbitrary height to wind speed at 2 meters
#'  based on equation 47 from the FAO Irrigation and Drainage Paper 56 (Allen et al., 1998)
#'
#' @param wind the wind speed measured at a given height
#'
#' @param height the height of wind speed measurement in meters
#'
#' @export
#'
wth_2m_wind <- function(wind, height){
  wind*4.87/log(67.8*height - 5.42)
}
