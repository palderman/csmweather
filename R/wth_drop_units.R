#' Drop units from a weather data frame
#'
#' @param wth_df a data frame with columns of weather variables as units vectors
#'
#' @export
#'
wth_drop_units <- function(wth_df){
  for(i in seq_along(wth_df)){
    if("units" %in% class(wth_df[[i]])){
      wth_df[[i]] <- units::drop_units(wth_df[[i]])
    }
  }
  for(i in seq_along(attr(wth_df, "GENERAL"))){
    if("units" %in% class(attr(wth_df, "GENERAL")[[i]])){
      attr(wth_df, "GENERAL")[[i]] <-
        units::drop_units(attr(wth_df, "GENERAL")[[i]])
    }
  }
  wth_df
}
