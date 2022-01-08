#' R6 Class for GitHub users
#'
#' @description
#' User class representing GitHub API response for that user.
#'
#' @details
#' Contains two fields \strong{username} and \strong{info} field, which
#' represents response from GitHub API, in form of a list. This class can later
#' be used to change repo parameters (such as: description, collaborators etc.)
#' @importFrom R6 R6Class
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom checkmate assertString
#' @examples
#' user <- User$new(username = "tidyverse")
#' user <- User$new("tidyverse")
#' \dontrun{
#' user <- User$new(username = "username")
#' }
#'
#' @export
User <- R6::R6Class(
  "User",
  public = list(

    #' @field username string Username of the GitHub user account.
    username = NULL,

    #' @field info list Info field which will contain response with all
    #' additional information about user.
    info = NULL,

    #' @description
    #' Create a new user object.
    #' @param username string Username of the GitHub account.
    #' @param info list Info list for the user.
    #' @return A new `User` object.
    initialize = function(username = NA, info = NULL) {
      # Check input parameters
      checkmate::assertString(username)

      # Set username field
      self$username <- username

      if (!is.null(info)) {
        checkmate::assertList(info)
        # Set info field
        self$info <- info
      } else {
        # Send GET request
        req <- GET(
          url = paste0(.github_api_paths$"users", username)
        )

        # Check if valid response
        .ensure_valid_http_response(req)

        # Set info field
        self$info <- httr::content(req)
      }
    },

    #' @description
    #' Change object printing
    print = function() {
      cat("<User> \n username: ", self$username, "\n info: list of ",
        length(self$info), " items.",
        sep = ""
      )
    }
  )
)
