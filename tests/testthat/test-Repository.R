library(testthat)
library(rGithub)

test_that("Repository initialization work", {
  # Prepare token, username and repo for testing
  auth_token <- Sys.getenv("AUTH_TOKEN")
  owner <- Sys.getenv("USERNAME")
  repo <- Sys.getenv("REPO")

  # No initialize method: throw error if wrong arguments are passed in
  expect_error(Repository$new("bad_owner", repo, auth_token))
  expect_error(Repository$new(owner, "bad_repo", auth_token))
  expect_error(Repository$new(owner, repo, bad_token))

  # Valid initialization with token
  repo1 <- Repository$new(owner = owner,  repo = repo, token = auth_token)

  expect_identical(owner, repo1$owner)
  expect_identical(repo, repo1$repo)
  expect_type(repo1$info, "list")

  info <- repo1$info
  # Valid initialization with info
  repo2 <- Repository$new(owner = owner,  repo = repo, info = info)

  expect_identical(owner, repo2$owner)
  expect_identical(repo, repo2$repo)
  expect_type(repo2$info, "list")
})

test_that("Repository update_description() work", {
  # Prepare token, username and repo for testing
  auth_token <- Sys.getenv("AUTH_TOKEN")
  owner <- Sys.getenv("USERNAME")
  repo <- Sys.getenv("REPO")

  # Valid initialization
  repo1 <- Repository$new(owner = owner,  repo = repo, token = auth_token)
  new_description <- paste0("New Description ", runif(1))

  # Bad parameters checking
  expect_error(repo1$update_description(5, token = auth_token))
  expect_error(repo1$update_description(new_description, token = "bad_token"))

  # Valid description
  repo1$update_description(new_description, token = auth_token)
  expect_identical(repo1$info$description, new_description)


  # Expect owner, repo and repository info type
  expect_identical(owner, repo1$owner)
  expect_identical(repo, repo1$repo)
  expect_type(repo1$info, "list")

})

test_that("Repository get_collaborators() work", {
  # Prepare token, username and repo for testing
  auth_token <- Sys.getenv("AUTH_TOKEN")
  owner <- Sys.getenv("USERNAME")
  repo <- Sys.getenv("REPO")

  # Valid initialization
  repo1 <- Repository$new(owner = owner,  repo = repo, token = auth_token)
  collaborators <- repo1$get_collaborators(token = auth_token)

  # Bad parameters checking
  expect_error(repo1$get_collaborators(token = "bad_token"))
  expect_error(repo1$get_collaborators())

  # Expect collaborators to be a list
  expect_type(collaborators, "list")

})
