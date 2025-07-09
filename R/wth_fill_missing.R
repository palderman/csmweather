#' Fill missing data gaps in weather data
#'
#' Fill in gaps in observed weather data using a range of techniques including
#'  from a secondary data source, linear interpolation or assumed numeric constant
#'
#' @param wth_data a data frame containing the primary weather data with missing
#'   data gaps to be filled
#'
#' @param fill_data an optional data frame containing observations to use for
#'   filling missing data gaps in wth_data
#'
#' @param method an optional named list of methods to be used for filling missing
#'   observations where the name of the list element indicates which column and
#'   the value of the list element indicates the method to be used for that column.
#'   Values may include an expression, a numeric constant (e.g. 0), or a
#'   character string indicating the method to be used (i.e. "linear", "spline" or "data").
#'   For any column not listed, the default is to use the matching column from
#'   fill_data ("data"), if provided, assume zero for rainfall (0) and then to
#'   linearly interpolate remaining variables ("linear").
#'
#' @param rain_column a character string containing the name of the rain column
#'  within wth_data
#'
#' @param date_column a character string containing the name of the date column
#'  within wth_data
#'
#' @export
#'
wth_fill_missing <- function(wth_data, fill_data = NULL, method = NULL,
                             rain_column = "RAIN", date_column = "DATE"){

  for(nm in setdiff(names(wth_data), names(method))){
    if(nm %in% names(fill_data)){
      method[[nm]] <- "data"
    }else if(nm == rain_column){
      method[[nm]] <- 0
    }else{
      method[[nm]] <- "linear"
    }
  }

  data_out <- wth_data[order(wth_data[[date_column]]), ]

  for(nm in setdiff(names(wth_data), date_column)){
    if(is.numeric(method[[nm]])){
      data_out[[nm]][is.na(data_out[[nm]])] <- method[[nm]]
    }else if(method[[nm]] %in% c("linear", "spline")){
      data_out[[nm]] <- wth_interpolate(data_out[[nm]], method = method[[nm]])
    }else if(method[[nm]] == "data"){
      stopifnot(nm %in% names(fill_data))
      na_rows <- which(is.na(data_out[[nm]]))
      data_out[[nm]][na_rows] <-
        merge(data_out[na_rows, date_column, drop = FALSE],
              fill_data[, c(date_column, nm)],
              all.x = TRUE)[[nm]]
    }
  }

  data_out
}
