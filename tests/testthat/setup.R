
## SET UP ----

  # Set teardown environment
  teardownEnv <- if (testthat::is_testing()) testthat::teardown_env() else .GlobalEnv

  # Authorize Google Drive
  googledrive::drive_auth()
  withr::defer(googledrive::drive_deauth(), envir = teardownEnv)

  # Set directory paths
  testDirs <- list(
    module   = ifelse(testthat::is_testing(), dirname(dirname(getwd())), getwd()),
    testdata = file.path(testthat::test_path(), "testdata"),
    tempRoot = file.path(tempdir(), "CBM_defaults_SK_tests")
  )

  testDirs$inputs  <- file.path(testDirs$tempRoot, "inputs")
  testDirs$outputs <- file.path(testDirs$tempRoot, "outputs")
  testDirs$libPath <- file.path(testDirs$tempRoot, "library")

  # Create temporary testing directories
  dir.create(testDirs$tempRoot)
  dir.create(testDirs$outputs)
  dir.create(testDirs$libPath)

  # Remove temporary testing directory on teardown
  if (testthat::is_testing()){
    withr::defer(unlink(testDirs$tempRoot, recursive = TRUE), envir = teardownEnv)
  }

  # Prefix library paths with temporary directory path
  withr::local_libpaths(c(testDirs$libPath, .libPaths()), .local_envir = teardownEnv)

  # Set reproducible to be quiet
  withr::local_options(list("reproducible.verbose" = -1), .local_envir = teardownEnv)


## SET UNIVERSAL INPUTS ----

  ## These are standard inputs that are usually provided by CBM_defaults or CBM_vol2biomass.
  moduleInputs <- list()

  # Required input from CBM_defaults
  moduleInputs$dbPath <- reproducible::prepInputs(
    url = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db",
    targetFile = "cbm_defaults_v1.2.8340.362.db",
    destinationPath = testDirs$inputs,
    alsoExtract = NA, fun = NA)

  ## RDS data provided because creation of these outputs is more complex than simple downloads
  moduleInputs$spinupSQL  <- readRDS(file.path(testDirs$testdata, "spinupSQL.rds"))
  moduleInputs$species_tr <- readRDS(file.path(testDirs$testdata, "species_tr.rds"))

  # Required input from CBM_vol2biomass
  moduleInputs$gcMeta <- reproducible::prepInputs(
    url = "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ/view?usp=sharing",
    targetFile = "gcMetaEg.csv",
    destinationPath = testDirs$inputs,
    fun = fread,
    purge = 7
  )





