#' Ensure valid HTTP response
#'
#' Checks an httr response object and gives appropriate error messages to user.
#' @param resp httr response
#' @return NULL. Call for side-effects.
.ensure_valid_http_response <- function(resp) {
  # If an http error
  if (httr::http_error(resp)) {
    error_message <- sprintf(
      "API request failed with status code %s",
      httr::status_code(resp)
    )

    stop(
      error_message,
      call. = FALSE
    )
  }

  # If response is not in JSON format
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  invisible(NULL)
}
