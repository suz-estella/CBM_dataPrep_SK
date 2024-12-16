
## SET UP ----

  # Set teardown environment
  teardownEnv <- if (testthat::is_testing()) testthat::teardown_env() else .GlobalEnv

  # Set module path
  testDirs <- list(
    module = ifelse(testthat::is_testing(), dirname(dirname(getwd())), getwd())
  )

  # Set input data path
  testDirs$testdata <- file.path(testthat::test_path(), "testdata")

  # Set temporary directory paths
  testDirs$tempRoot <- file.path(tempdir(), paste0(basename(testDirs$module), "_tests"))
  testDirs$inputs   <- file.path(testDirs$tempRoot, "inputs")
  testDirs$outputs  <- file.path(testDirs$tempRoot, "outputs")
  testDirs$libPath  <- file.path(testDirs$tempRoot, "library")

  # Create temporary testing directories
  dir.create(testDirs$tempRoot)
  withr::defer(unlink(testDirs$tempRoot, recursive = TRUE), envir = teardownEnv)

  dir.create(testDirs$inputs)
  dir.create(testDirs$outputs)
  dir.create(testDirs$libPath)

  # Prefix library paths with temporary directory path
  withr::local_libpaths(c(testDirs$libPath, .libPaths()), .local_envir = teardownEnv)

  # Authorize Google Drive
  googledrive::drive_auth()
  withr::defer(googledrive::drive_deauth(), envir = teardownEnv)


## SET UNIVERSAL INPUTS ----

  ## These are standard inputs that are usually provided by CBM_defaults or CBM_vol2biomass.
  moduleInputs <- list()

  # Required input from CBM_defaults
  moduleInputs$dbPath <- reproducible::prepInputs(
    url = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db",
    targetFile = "cbm_defaults_v1.2.8340.362.db",
    destinationPath = testDirs$inputs,
    alsoExtract = NA, fun = NA,
    verbose = -1)

  ## RDS data provided because creation of these outputs is more complex than simple downloads
  moduleInputs$spinupSQL  <- readRDS(file.path(testDirs$testdata, "spinupSQL.rds"))
  moduleInputs$species_tr <- readRDS(file.path(testDirs$testdata, "species_tr.rds"))

  # Required input from CBM_vol2biomass
  moduleInputs$gcMeta <- reproducible::prepInputs(
    url = "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ/view?usp=sharing",
    targetFile = "gcMetaEg.csv",
    destinationPath = testDirs$inputs,
    fun = fread,
    purge = 7,
    verbose = -1)

