extract_string <- function(string, pattern){
  regexpr(text = string,
          pattern = pattern,
          perl = TRUE) |>
    regmatches(x = string, m = _)
}

submit_curl_request <- function(url,
                                err_msg = "HTTP request failed",
                                valid_status_codes = 200:206){

  curl_handle <-
    curl::new_handle() |>
    curl::handle_setopt(customrequest = "GET")

  curl_response <-
    curl::curl_fetch_memory(url = url,
                            handle = curl_handle) |>
    tryCatch(error = \(.x) list(status_code = 999))

  curl_response |>
    check_curl_response(valid_status_codes = valid_status_codes,
                        err_msg = err_msg)

  curl_response
}

check_curl_response <- function(response,
                                valid_status_codes = 200:206,
                                err_msg = "http request failed!"){
  if(! response$status_code %in% valid_status_codes){
    response$content |>
      rawToChar() |>
      sprintf("%s Status Code: %i %s",
              err_msg,
              response$status_code,
              .x = _) |>
      stop()
  }
}
