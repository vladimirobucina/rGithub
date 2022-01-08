library(testthat)
library(rGithub)

test_that("User initialization work", {
  # Prepare token and username for testing
  username <- Sys.getenv("USERNAME")

  # No initialize method: throw error if wrong arguments are passed in
  expect_error(User$new("bad_username"))
  expect_error(User$new(username, "bad_info"))

  # Valid initialization
  user <- User$new(username)

  expect_identical(username, user$username)
  expect_type(user$info, "list")

})
