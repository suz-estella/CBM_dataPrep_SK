
## SET UP ----

  # Install required packages
  ## Required because module is not an R package
  install.packages(
    c("testthat", "SpaDES.core", "reproducible", "googledrive"),
    type = "binary",
    repos = unique(c("predictiveecology.r-universe.dev", getOption("repos"))))

  # Cache Google Drive authorization
  ## Set authorization email and cache location
  options(
    gargle_oauth_email = "", ## Set personal email
    gargle_oauth_cache = "~/googledrive_oauth_cache"
  )
  googledrive::drive_auth()

  # Set location of input data (optional)
  # options("reproducible.inputPaths" = "~/data")


## RUN ALL TESTS ----

  # Run all tests
  testthat::test_dir("tests/testthat")

  # Run all tests without warnings
  withr::with_options(c(warn = -1), testthat::test_dir("tests/testthat"))


## RUN INDIVIDUAL TESTS ----

  ## Run module with defaults
  testthat::test_file("tests/testthat/test-1-module_1-defaults.R")

  ## Run module with a smaller study area
  testthat::test_file("tests/testthat/test-1-module_2-withAOI.R")

