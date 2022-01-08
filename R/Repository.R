#' R6 Class for GitHub repositories
#'
#' @description
#' Repository class representing GitHub API response for that repo.
#'
#' @details
#' Contains three fields \strong{owner}, \strong{name} of the
#' repository, and \strong{info} field, which represents response from GitHub API, in
#' form of a list. This class can later be used to change repo parameters (such
#' as: description, collaborators etc.)
#' @importFrom R6 R6Class
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom checkmate assertString
#' @examples
#' repo <- Repository$new(owner = "tidyverse", repo = "dplyr")
#' repo <- Repository$new("tidyverse", "dplyr")
#' \dontrun{
#' repo <- Repository$new(owner = "owner", repo = "repo", token = "token")
#' }
#'
#' @export
Repository <- R6::R6Class(
  "Repository",
  public = list(

    #' @field owner string Owner of the GitHub repo.
    owner = NULL,

    #' @field repo string Name of the Github repo.
    repo = NULL,

    #' @field info list Info field which will contain response with all
    #' additional information about repo.
    info = NULL,

    #' @description
    #' Create a new repository object.
    #' @param owner string Owner of the repo.
    #' @param repo string Name of the repo.
    #' @param info list List of info fields.
    #' @param token string Token, not a mandatory field.
    #' needed for private repos.
    #' @return A new `Repository` object.
    initialize = function(owner, repo, info = NULL, token = NA) {
      # Check input parameters
      checkmate::assertString(owner)
      checkmate::assertString(repo)

      self$owner <- owner
      self$repo <- repo

      if (!is.null(info)) {
        checkmate::assertList(info)
        self$info <- info
      } else {
        headers <- c()
        if (!is.na(token)) {
          # Check if token is string
          checkmate::assertString(token)
          # Set token as a header if present
          headers <- c(
            "authorization" = paste("token", token)
          )
        }
        # Send GET request
        req <- httr::GET(
          url = paste0(.github_api_paths$"repos", owner, "/", repo),
          add_headers(.headers = headers)
        )

        # Check if valid response
        .ensure_valid_http_response(req)

        self$info <- httr::content(req)
      }
    },

    #' @description
    #' Change object printing
    print = function() {
      cat("<User> \n owner: ", self$owner, "\n repo: ", self$repo,
        "\n info: list of ", length(self$info), " items.",
        sep = ""
      )
    },

    #' @description
    #' Update repository description
    #' @param description string New description.
    #' @param token string Github token.
    #' @return A new `Repository` object.
    update_description = function(description = NA, token = NA) {
      # Check input parameters
      checkmate::assertString(description)
      checkmate::assertString(token)

      # Set token as a header
      headers <- c(
        "authorization" = paste("token", token)
      )

      # Send POST request
      req <- httr::POST(
        url = paste0(.github_api_paths$"repos", self$owner, "/", self$repo),
        add_headers(.headers = headers),
        body = list(description = description),
        encode = "json"
      )

      # Check if valid response
      .ensure_valid_http_response(req)

      self$info <- httr::content(req)
    },

    #' @description
    #' Get repository collaborators
    #' @param token string GitHub token.
    #' @return Vector of `User` objects.
    get_collaborators = function(token = NA) {
      # Check if token is string
      checkmate::assertString(token)
      # Set token as a header
      headers <- c(
        "authorization" = paste("token", token)
      )

      req <- httr::GET(
        url = paste0(
          .github_api_paths$"repos", self$owner, "/", self$repo,
          "/collaborators"
        ),
        add_headers(.headers = headers)
      )

      # Check if valid response
      .ensure_valid_http_response(req)

      list_of_collaborators <- httr::content(req)

      collaborators <- c()
      for (collaborator in list_of_collaborators) {
        collaborator_object <- User$new(
          username = collaborator$login,
          info = collaborator
        )
        collaborators <- c(collaborators, collaborator_object)
      }
      return(collaborators)
    },

    #' @description
    #' Update repository description
    #' @param username string GitHub username.
    #' @param permission string Repo permission. "push" by default. Also
    #' possible: pull, admin, maintain, triage.
    #' @param token string GitHub token.
    #' @return A new `Repository` object.
    add_collaborator = function(username = NA, permission = "push", token = NA) {
      # Check input parameters
      checkmate::assertString(username)
      checkmate::assertString(permission)
      checkmate::assertString(token)

      # Set token as a header
      headers <- c(
        "authorization" = paste("token", token)
      )

      # Send POST request
      req <- httr::POST(
        url = paste0(
          .github_api_paths$"repos", self$owner, "/", self$repo,
          "collaborators/", username
        ),
        add_headers(.headers = headers),
        body = list(permission = permission),
        encode = "json"
      )

      # Check if valid response
      .ensure_valid_http_response(req)

      self$info <- httr::content(req)
    }
  )
)
