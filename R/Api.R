#' R6 Class for GitHub API object
#'
#' @description
#' API class representing GitHub API object used to maniuplate users and repos.
#'
#' @details
#' Contains two fields \strong{token} and \strong{user} field, which
#' is of type `User` class. This class can later be used to change repo and user
#' parameters (such as: description, collaborators etc.)
#' @importFrom R6 R6Class
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom checkmate assertString
#' @examples
#' \dontrun{
#' api <- User$new(token = "token", username = "username")
#' api <- User$new("token", "username")
#' }
#'
#' @export
Api <- R6::R6Class(
  "Api",
  public = list(

    #' @field token string Token of the GitHub user
    token = NULL,

    #' @field user User class object.
    user = NULL,

    #' @description
    #' Create a new user object.
    #' @param token string Token for the GitHub account.
    #' @param username string Username of the GitHub account.
    #' @return A new `Api` object.
    initialize = function(token = NA, username = NA) {
      # Check input parameters
      checkmate::assertString(username)
      checkmate::assertString(token)

      # Set token field
      self$token <- token

      # Set token as a header
      headers <- c(
        "authorization" = paste("token", token)
      )

      # Send GET request and confirm token validation
      req <- httr::GET(
        url = paste0(.github_api_paths$"users", username),
        add_headers(.headers = headers)
      )

      # Check if valid response
      .ensure_valid_http_response(req)

      # Check if plan field is in response
      response <- httr::content(req)
      if ("plan" %in% names(response)) {
        # Get user field
        self$user <- response
      } else {
        stop("Wrong token was provided for this username. Please try again!")
      }
    },

    #' @description
    #' Change object printing
    print = function() {
      cat("<Api> \n token: ", self$token, "\n user (R6 object): ",
        self$user$login,
        sep = ""
      )
    },

    #' @description
    #' Listing all repositories which the authenticated user has access to
    #' @importFrom rlist list.append
    #' @return Vector of `Repository` objects.
    list_repositories = function() {
      # Set token as a header
      headers <- c(
        "authorization" = paste("token", self$token)
      )

      # Send GET request
      req <- httr::GET(
        url = paste0(.github_api_paths$"users", self$user$login, "/repos"),
        add_headers(.headers = headers)
      )


      # Check if valid response
      .ensure_valid_http_response(req)

      repositories <- httr::content(req)

      # Create a vector of repository objects
      repos <- c()
      for (repo in repositories) {
        repo_object <- Repository$new(owner = self$user$login, repo = repo$name, info = repo)
        repos <- c(repos, repo_object)
      }
      return(repos)
    }
  )
)
