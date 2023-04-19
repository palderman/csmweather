#' Linearly interpolate data over dates
#'
#' @export
#'
#' @param time_series vector of data to be interpolated
#'
lin_interp <- function(time_series){
  na_elems <- is.na(time_series)
  gap_filled <- approx((1:length(time_series))[!na_elems],
                       time_series[!na_elems],
                       xout = 1:length(time_series))

  return(gap_filled[["y"]])
}
