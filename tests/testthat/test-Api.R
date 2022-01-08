library(testthat)
library(rGithub)

test_that("Api initialization work", {
  # Prepare token and username for testing
  auth_token <- Sys.getenv("AUTH_TOKEN")
  username <- Sys.getenv("USERNAME")

  # No initialize method: throw error if wrong arguments are passed in
  expect_error(Api$new("bad_token", username))
  expect_error(Api$new(auth_token, "bad_username"))

  # Valid initialization
  api <- Api$new(auth_token, username)

  expect_identical(auth_token, api$token)
  expect_identical(username, api$user$login)

})

test_that("Api list_repositories() work", {
  # Prepare token and username for testing
  auth_token <- Sys.getenv("AUTH_TOKEN")
  username <- Sys.getenv("USERNAME")

  # Valid initialization
  api <- Api$new(auth_token, username)

  # Expect result  type and repository type
  expect_type(api$list_repositories(), "list")
  expect_type(api$list_repositories()[[1]], "environment")
  expect_identical(api$list_repositories()[[1]]$owner, username)

})
