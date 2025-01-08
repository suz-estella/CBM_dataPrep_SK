
## SET UP ----

  # Install required packages
  ## Required because module is not an R package
  install.packages(
    c("testthat", "SpaDES.core", "SpaDES.project", "googledrive"),
    type = "binary",
    repos = unique(c("predictiveecology.r-universe.dev", getOption("repos"))))

  # Authorize Google Drive
  googledrive::drive_auth()


## OPTIONAL: SET TEST OPTIONS ----

  # Suppress warnings from calls to setupProject, simInit, and spades
  options("spadesCBM.test.suppressWarnings" = TRUE)

  # Set custom input data location
  options("reproducible.inputPaths" = NULL)


## RUN ALL TESTS ----

  # Run all tests
  testthat::test_dir("tests/testthat")

  # Run all tests with different reporters
  testthat::test_dir("tests/testthat", reporter = testthat::LocationReporter)
  testthat::test_dir("tests/testthat", reporter = testthat::SummaryReporter)


## RUN INDIVIDUAL TESTS ----

  ## Run module with defaults
  testthat::test_file("tests/testthat/test-1-module_1-defaults.R")

  ## Run module with a smaller study area
  testthat::test_file("tests/testthat/test-1-module_2-withAOI.R")

