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
#' @param ... additional parameters passed to [nasapower::get_power()]
#'
#' @importFrom nasapower get_power
#' @importFrom dplyr "%>%" select rename mutate
#' @importFrom tibble tibble
#' @importFrom stringr str_extract str_split str_replace_all str_c
#' @importFrom lubridate force_tz
#'
get_nasa_power <- function(lat, long, start, end,
                           pars = c("PRECTOTCORR","T2M_MAX",
                                    "T2M_MIN","ALLSKY_SFC_SW_DWN",
                                    "WS2M", "RH2M"),
                           INSI = "NASA",
                           ...){

  if(!is.character(start)) start <- format(start, "%Y-%m-%d")

  if(!is.character(end)) end <- format(end, "%Y-%m-%d")

  power_data <- get_power(
      community = "ag",
      lonlat = c(long, lat),
      pars = pars,
      dates = c(start, end),
      temporal_api = "daily"
    ) %>%
    rename(DATE = YYYYMMDD,
           SRAD = ALLSKY_SFC_SW_DWN,
           TMAX = T2M_MAX,
           TMIN = T2M_MIN,
           RAIN = PRECTOTCORR,
           WIND = WS2M,
           RHUM = RH2M) %>%
    mutate(DATE = as.POSIXct(force_tz(DATE, "UTC")),
           WIND = m_s_to_km_d(WIND)) %>%
    select(DATE, SRAD, TMAX, TMIN, RAIN, WIND, RHUM)

  POWER_elev <- attr(power_data, "POWER.Elevation") %>%
    str_extract("(?== )[0-9.]+") %>%
    as.numeric()

  general <- tibble(
    INSI = INSI,
    LAT = lat,
    LONG = long,
    TAV = calc_tav(power_data),
    AMP = calc_tamp(power_data),
    ELEV = POWER_elev,
    REFHT = 2,
    WNDHT = 2
  )

  pars_doc <- attr(power_data, "POWER.Parameters") %>%
    str_split(" +;\n +") %>%
    unlist() %>%
    str_replace_all(c("(ALLSKY_SFC_SW_DWN) +" = "SRAD from \\1 ",
                      "(T2M_MAX) +" = "TMAX from \\1 ",
                      "(T2M_MIN) +" = "TMIN from \\1 ",
                      "(PRECTOTCORR) +" = "RAIN from \\1 ",
                      "(WS2M) +" = "WIND from \\1",
                      "(RH2M) +" = "RHUM from \\1")) %>%
    str_c("  ", .)

  attr(power_data, "GENERAL") <- general

  attr(power_data, "comments") <- c(
    "Weather data extracted from NASA POWER agroclimatology dataset",
    "using the weathRman and nasapower R packages:",
    pars_doc,
    "See https://power.larc.nasa.gov for full documentation."
  )

  return(power_data)
}
