nasa_power_query <- function(parameters,
                                    latitude,
                                    longitude,
                                    start,
                                    end,
                                    format = "JSON",
                                    community = "ag",
                                    base_url = "https://power.larc.nasa.gov/api/temporal/daily/point"){
  paste0(base_url,
         "?parameters=", paste0(parameters, collapse = ","),
         "&community=", toupper(community),
         "&longitude=", longitude,
         "&latitude=", latitude,
         "&start=", start,
         "&end=", end,
         "&format=", format)

}
