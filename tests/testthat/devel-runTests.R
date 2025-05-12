
## SET UP ----

  # Install required packages
  ## Required because module is not an R package
  install.packages(
    c("testthat", "SpaDES.core", "SpaDES.project"),
    repos = unique(c("predictiveecology.r-universe.dev", getOption("repos"))))


## OPTIONS ----

  # Suppress warnings from calls to setupProject, simInit, and spades
  options("spades.test.suppressWarnings" = TRUE)

  # Set custom directory paths
  ## Speed up tests by allowing inputs, cache, and R packages to persist between runs
  options("spades.test.paths.inputs"   = NULL) # inputPath
  options("spades.test.paths.cache"    = NULL) # cachePath
  options("spades.test.paths.packages" = NULL) # packagePath


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

  ## Run multi module integration test
  testthat::test_file("tests/testthat/test-2-multiModule_SK-small_1998-2000.R")

