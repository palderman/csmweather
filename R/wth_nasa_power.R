utils::globalVariables(c("DATE", "YYYYMMDD", "SRAD", "ALLSKY_SFC_SW_DWN",
                         "TMAX", "T2M_MAX", "TMIN", "T2M_MIN", "RAIN",
                         "PRECTOTCORR", "WIND", "WS2M", "RHUM", "RH2M"))

#' Get NASA POWER agroclimatology data for crop modeling
#'
#' @export
#'
#' @param lat latitude of query point in decimal degrees
#'
#' @param long longitude of query point in decimal degrees
#'
#' @param start first date for data query as POSIXt, Date, or
#'   character formatted as four-digit year, two-digit month,
#'   and two-digit day of month (e.g. "2010-12-31")
#'
#' @param end last date for data query as POSIXt, Date, or
#'   character formatted as four-digit year, two-digit month,
#'   and two-digit day of month (e.g. 2010-12-31)
#'
#' @param pars a character vector of parameters to download. A full
#'   parameter dictionary can be found at:
#'   [https://power.larc.nasa.gov/#resources](https://power.larc.nasa.gov/#resources)
#'
#' @param INSI a four-digit character code to uniquely identify the
#'   query location
#'
wth_nasa_power <- function(lat, long, start, end,
                           pars = c("PRECTOTCORR","T2M_MAX",
                                    "T2M_MIN","ALLSKY_SFC_SW_DWN",
                                    "WS2M", "RH2M", "T2MDEW"),
                           INSI = "NASA"){

  if(!is.character(start)){
    start <- format(start, "%Y%m%d")
  }else{
    start <- gsub("-", "", start)
  }

  if(!is.character(end)){
    end <- format(end, "%Y%m%d")
  }else{
    end <- gsub("-", "", end)
  }

  wth_cols <- c("SRAD" = "ALLSKY_SFC_SW_DWN",
                "TMAX" = "T2M_MAX",
                "TMIN" = "T2M_MIN",
                "RAIN" = "PRECTOTCORR",
                "WIND" = "WS2M",
                "RHUM" = "RH2M",
                "DEWP" = "T2MDEW")

  wth_cols <-
    wth_cols[wth_cols %in% pars]

  query_result <-
    nasa_power_query(parameters = pars,
                     latitude = lat,
                     longitude = long,
                     start = start,
                     end = end,
                     format = "CSV") |>
    submit_curl_request()

  query_content <- query_result$content |>
    rawToChar() |>
    strsplit("\r*\n") |>
    unlist()

  header_lines <-
    query_content |>
    grep("HEADER", x = _)

  header <- query_content[header_lines[1]:header_lines[2]]

  power_data <-
    read.csv(text = query_content,
             skip = header_lines[2],
             na.strings = c("-999", "-999.00", "-999.0", "-99", "-99.00",
                            "-99.0")) |>
    within({
      DATE = as.Date(sprintf("%4i%3.3i", YEAR, DOY), format = "%Y%j")
      if("ALLSKY_SFC_SW_DWN" %in% pars) SRAD = units::set_units(ALLSKY_SFC_SW_DWN, "MJ/m2/d")
      if("T2M_MAX" %in% pars) TMAX = units::set_units(T2M_MAX, "Celsius")
      if("T2M_MIN" %in% pars) TMIN = units::set_units(T2M_MIN, "Celsius")
      if("PRECTOTCORR" %in% pars) RAIN = units::set_units(PRECTOTCORR, "mm")
      if("WS2M" %in% pars) WIND = units::set_units(units::set_units(WS2M, "m/s"), "km/d")
      if("RH2M" %in% pars) RHUM = units::set_units(RH2M, "percent")
      if("T2MDEW" %in% pars) DEWP = units::set_units(T2MDEW, "Celsius")
      }) |>
    subset(select = c("DATE", names(wth_cols)))

  POWER_elev <-
    header |>
    grep("^elevation", x = _, value = TRUE) |>
    extract_string("(?<== )[0-9.]+") |>
    as.numeric()

  if(all(c("T2M_MAX", "T2M_MIN") %in% pars)){
    tav <- calc_tav(power_data)
    amp <- calc_tamp(power_data)
  }else{
    tav <- amp <- NA_real_
  }

  general <- data.frame(
    INSI = INSI,
    LAT = lat,
    LONG = long,
    TAV = tav,
    AMP = amp,
    ELEV = units::set_units(POWER_elev, "m"),
    REFHT = units::set_units(2, "m"),
    WNDHT = units::set_units(2, "m"),
    stringsAsFactors = FALSE
  )

  pars_doc <-
    pars |>
    paste0(collapse = "|") |>
    paste0("^(", x = _, ")") |>
    grep(header, value = TRUE) |>
    gsub("(ALLSKY_SFC_SW_DWN) +", "SRAD from \\1 ", x = _) |>
    gsub("(T2M_MAX) +", "TMAX from \\1 ", x = _) |>
    gsub("(T2M_MIN) +", "TMIN from \\1 ", x = _) |>
    gsub("(PRECTOTCORR) +", "RAIN from \\1 ", x = _) |>
    gsub("(WS2M) +", "WIND from \\1 ", x = _) |>
    gsub("(RH2M) +", "RHUM from \\1 ", x = _) |>
    gsub("(T2MDEW) +", "DEWP from \\1 ", x = _) |>
    gsub(" +$", "", x = _) |>
    paste0("  ", x = _)

  attr(power_data, "POWER.Header") <-
    pars |>
    paste0("^", x = _) |>
    c("HEADER", "^parameter") |>
    paste0(collapse = "|") |>
    paste0("(", x = _, ")") |>
    grep(header, invert = TRUE, value = TRUE) |>
    gsub("(missing.*:) +-999 *$", "\\1 NA", x = _)

  attr(power_data, "GENERAL") <- general

  attr(power_data, "comments") <- c(
    "Weather data extracted from NASA POWER agroclimatology dataset",
    "using the csmweather R package:",
    pars_doc,
    "See https://power.larc.nasa.gov for full documentation."
  )

  return(power_data)
}
