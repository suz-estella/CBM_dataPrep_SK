
# Get a list of test directory paths
## These will need to be updated if a DESCRIPTION file is added.
.test_directories <- function(tempDir = tempdir()){

  testDirs <- list()

  # Set R project location
  testDirs$Rproj <- ifelse(testthat::is_testing(), dirname(dirname(getwd())), getwd())

  # Set input data path (must be absolute)
  testDirs$testdata <- file.path(testDirs$Rproj, "tests/testthat", "testdata")

  # Set temporary directory paths
  testDirs$temp <- list(
    root = file.path(tempDir, paste0("testthat-", basename(testDirs$Rproj)))
  )
  testDirs$temp$modules  <- file.path(testDirs$temp$root, "modules")  # For modules
  testDirs$temp$inputs   <- file.path(testDirs$temp$root, "inputs")   # For shared inputs
  testDirs$temp$libPath  <- file.path(testDirs$temp$root, "library")  # R package library
  testDirs$temp$outputs  <- file.path(testDirs$temp$root, "outputs")  # For unit test outputs
  testDirs$temp$projects <- file.path(testDirs$temp$root, "projects") # For project directories

  # Return
  testDirs
}

# Helper function: copy module
## This will hopefully be handled by testthat if a DESCRIPTION file is added.
.test_copyModule <- function(moduleDir, destDir, moduleName = basename(moduleDir)){

  modFiles <- file.info(list.files(moduleDir, full = TRUE))
  modFiles$name <- basename(row.names(modFiles))
  modFiles$path <- row.names(modFiles)

  modDir <- file.path(destDir, moduleName)
  dir.create(modDir)

  file.copy(modFiles$path[!modFiles$isdir],
            file.path(modDir, modFiles$name[!modFiles$isdir]))
  for (d in modFiles$path[modFiles$isdir & modFiles$name %in% c("R", "data")]){
    file.copy(d, modDir, recursive = TRUE)
  }
}

# Helper function: suppress output and messages; muffle common warnings
.SpaDESwithCallingHandlers <- function(expr, ...){

  if (testthat::is_testing()){

    withr::local_output_sink(tempfile())

    withCallingHandlers(
      expr,
      message = function(c) tryInvokeRestart("muffleMessage"),
      packageStartupMessage = function(c) tryInvokeRestart("muffleMessage"),
      warning = function(w){
        if (getOption("spadesCBM.test.suppressWarnings", default = FALSE)){
          tryInvokeRestart("muffleWarning")
        }else{
          if (grepl("^package ['\u2018]{1}[a-zA-Z0-9.]+['\u2019]{1} was built under R version [0-9.]+$", w$message)){
            tryInvokeRestart("muffleWarning")
          }
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
    dlURL <- "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db"
    destPath <- file.path(testDirs$temp$inputs, basename(dlURL))
    if (!file.exists(destPath)){
      download.file(url = dlURL, destfile = destPath, mode = "wb", quiet = TRUE)
    }
    destPath

  }else if (objectName == "spinupSQL"){

    # From CBM_defaults
    readRDS(file.path(testDirs$testdata, "spinupSQL.rds"))

  }else if (objectName == "species_tr"){

    # From CBM_defaults
    readRDS(file.path(testDirs$testdata, "species_tr.rds"))

  }else if (objectName == "gcMeta"){

    # From CBM_vol2biomass
    dlURL <- "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ/view?usp=sharing"
    destPath <- file.path(testDirs$temp$inputs, "gcMetaEg.csv")
    if (!file.exists(destPath)){
      withr::with_options(c(googledrive_quiet = TRUE), googledrive::drive_download(dlURL, path = destPath))
    }
    data.table::fread(destPath)
  }
}

