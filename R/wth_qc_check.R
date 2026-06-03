#' Perform quality controls checks on weather data
#'
#' Apply and summarize quality control checks on weather data
#'
#' @param wth_data a data frame containing the weather data to be assessed with
#'   quality control checks
#'
#' @param exclude_columns a character string containing the name of the columns
#'  within wth_data to exclude from quality control checks
#'
#' @param qc_checks an optional named list of functions that implement
#'  additional quality control checks
#'
#' @export
#'
wth_qc_check <- function(wth_data, exclude_columns = "DATE", qc_checks = NULL){

  checks <- list(`NA count` = \(.x) sum(is.na(.x)),
                 `IQR outliers` = iqr_outlier_count,
                 `Gap length (med)` = \(.x) gap_length(.x, median),
                 `Gap length (max)` = \(.x) gap_length(.x, max)) |>
    c(qc_checks)

  check_df <- data.frame(check = names(checks))

  for(.c in setdiff(names(wth_data), exclude_columns)){
    check_df[[.c]] <-
      lapply(checks, \(.x) .x(wth_data[[.c]])) |>
      unlist()
  }

  check_df
}

iqr_outlier_count <- function(.x, na.rm = TRUE){
  qt <- quantile(.x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(qt)
  sum(.x < qt[1] - 1.5*iqr | .x > qt[2] + 1.5*iqr,
      na.rm = na.rm)
}

gap_length <- function(.x, FUN = median){
  if(!any(is.na(.x))) return(NA_integer_)
  .x |>
    is.na() |>
    rle() |>
    with({lengths[values]}) |>
    FUN()
}
