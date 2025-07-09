#' Interpolate over missing data
#'
#' @export
#'
#' @param x vector of data to be interpolated
#'
#' @param method a character string indicating the method to be used for interpolation
#'
wth_interpolate <- function(x, method = c("linear", "spline")){

  method <- match.arg(method)

  na_elems <- is.na(x)

  if(method == "linear"){
    gap_filled <- approx((1:length(x))[!na_elems],
                         x[!na_elems],
                         xout = 1:length(x))
  }else if(method == "spline"){
    gap_filled <- spline(x = (1:length(x))[!na_elems],
                         y = x[!na_elems],
                         xout = 1:length(x),
                         method = "fmm")
  }else{
    sprintf("method \"%s\" is not supported", method) |>
    stop()
  }

  return(gap_filled[["y"]])
}
