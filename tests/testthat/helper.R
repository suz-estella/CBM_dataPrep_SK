
# Get a list of test directory paths
.test_directories <- function(tempDir = tempdir()){

  # Set module path (there is a better way)
  testDirs <- list(
    module = ifelse(testthat::is_testing(), dirname(dirname(getwd())), getwd())
  )

  # Set input data path (must be absolute)
  testDirs$testdata <- file.path(testDirs$module, "tests/testthat/testdata") ## this could be made better with the DESCRIPTION file.

  # Set temporary directory paths
  testDirs$temp <- list(
    root = file.path(tempDir, paste0("testthat-", basename(testDirs$module)))
  )
  testDirs$temp$inputs   <- file.path(testDirs$temp$root, "inputs")   # For shared inputs
  testDirs$temp$libPath  <- file.path(testDirs$temp$root, "library")  # R package library
  testDirs$temp$outputs  <- file.path(testDirs$temp$root, "outputs")  # For unit test or misc outputs
  testDirs$temp$projects <- file.path(testDirs$temp$root, "projects") # For project directories

  # Return
  testDirs
}

# Helper function: suppress messages; muffle common warnings
.SpaDESwithCallingHandlers <- function(expr, ...){

  if (testthat::is_testing()){

    withCallingHandlers(
      expr,
      message = function(c) tryInvokeRestart("muffleMessage"),
      packageStartupMessage = function(c) tryInvokeRestart("muffleMessage"),
      warning = function(w){
        if (grepl("^package ['\u2018]{1}[a-zA-Z0-9.]+['\u2019]{1} was built under R version [0-9.]+$", w$message)){
          tryInvokeRestart("muffleWarning")
        }
      },
      ...
    )

  }else expr
}

## Get standard inputs that are usually provided by CBM_defaults or CBM_vol2biomass.
## RDS data provided where creation of these outputs is more complex than a simple downloads
.test_defaultInputs <- function(objectName){

  if (objectName == "dbPath"){

    # From CBM_defaults
    reproducible::prepInputs(
      url = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db",
      targetFile = "cbm_defaults_v1.2.8340.362.db",
      destinationPath = testDirs$temp$inputs,
      alsoExtract = NA, fun = NA)

  }else if (objectName == "spinupSQL"){

    # From CBM_defaults
    readRDS(file.path(testDirs$testdata, "spinupSQL.rds"))

  }else if (objectName == "species_tr"){

    # From CBM_defaults
    readRDS(file.path(testDirs$testdata, "species_tr.rds"))

  }else if (objectName == "gcMeta"){

    # From CBM_vol2biomass
    reproducible::prepInputs(
      url = "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ/view?usp=sharing",
      targetFile = "gcMetaEg.csv",
      destinationPath = testDirs$temp$inputs,
      fun = fread,
      purge = 7)
  }
}

