
# SpaDES test: Copy module to temporary test directory
SpaDEStestCopyModule <- function(testDirs, ...){

  .test_copyModule(
    modulePath = testDirs$package,
    moduleName = basename(testDirs$package),
    destDir    = testDirs$temp$modules,
    ...)
}

# Copy module files
.test_copyModule <- function(modulePath, moduleName, destDir,
                             include = c(paste0(moduleName, ".R"), "R", "data")){

  if (!file.exists(modulePath)) stop(
    "Module directory not found: ", modulePath)

  # List module files
  modFiles <- file.info(list.files(modulePath, full = TRUE))
  modFiles$path <- row.names(modFiles)
  modFiles$name <- basename(modFiles$path)

  # Create module directory
  modDir <- file.path(destDir, moduleName)
  dir.create(modDir)

  # Copy module files
  copyFiles <- subset(modFiles, name %in% include)

  if (nrow(copyFiles) == 0) stop(
    "Module files not found in directory: ",
    file.path(modulePath, moduleName))

  copySuccess <- c()
  for (i in 1:nrow(copyFiles)){
    copySuccess[copyFiles[i,]$name] <- suppressWarnings({
      if (!copyFiles[i,]$isdir){
        file.copy(copyFiles[i,]$path, file.path(modDir, copyFiles[i,]$name))
      }else{
        file.copy(copyFiles[i,]$path, modDir, recursive = TRUE)
      }
    })
  }
  if (any(!copySuccess)) stop(
    "Module file(s) failed to copy:\n- ",
    paste(file.path(moduleName, names(copySuccess)[!copySuccess]), collapse = "\n- ")
  )
}


# SpaDES test: Set up test directories
SpaDEStestSetUpDirectories <- function(
    localOutputSink  = testthat::is_testing(),
    localMessageSink = FALSE,
    teardownEnv      = testthat::teardown_env(), ...){

  # List test paths and temporary directories
  testDirs <- .test_directories(...)

  # Create temporary directories
  for (d in testDirs$temp) dir.create(d)

  # Remove temporary directories on test teardown
  withr::defer({
    unlink(testDirs$temp$root, recursive = TRUE)
    if (file.exists(testDirs$temp$root)) warning(
      "Temporary test directory could not be removed: ", testDirs$temp$root, call. = F)
  }, envir = teardownEnv, priority = "last")

  # Restore library paths after testing
  ## This likely should be where setupProject() is called (inside test_that),
  ## but the packages that are left attached after running SpaDES stops it
  libPathsInit <- .libPaths()
  withr::local_libpaths(libPathsInit, .local_envir = teardownEnv)

  # Install "testthat" with dependencies into the R packages directory
  Require::Require("testthat", libPaths = testDirs$temp$libPath,
                   dependencies = TRUE, verbose = -2)

  # Sink output and messages to file
  if (localOutputSink)  withr::local_output_sink(file.path(testDirs$temp$root, "local_output_sink.txt"))
  if (localMessageSink) withr::local_message_sink(file.path(testDirs$temp$root, "local_message_sink.txt"))

  # Return test directories
  testDirs
}

# Set test directory paths
.test_directories <- function(testPaths = NULL, tempDir = tempdir()){

  testDirs <- list()

  # Set project root
  testDirs$package <- normalizePath(testthat::test_path("../.."))

  # Set custom test paths
  for (testPath in testPaths){
    testDirs[[testPath]] <- normalizePath(testthat::test_path(testPath))
  }

  # Set temporary directory paths
  testDirs$temp <- list(
    root = file.path(tempDir, paste0("testthat-", basename(testDirs$package)))
  )
  testDirs$temp$inputs   <- file.path(testDirs$temp$root, "inputs")   # For shared inputs
  testDirs$temp$outputs  <- file.path(testDirs$temp$root, "outputs")  # For test outputs
  testDirs$temp$modules  <- file.path(testDirs$temp$root, "modules")  # For modules
  testDirs$temp$libPath  <- file.path(testDirs$temp$root, "library")  # R package library
  testDirs$temp$projects <- file.path(testDirs$temp$root, "projects") # For project directories

  # Return
  testDirs
}


# SpaDES test: Set local global options
SpaDEStestLocalOptions <- function(
    reproducible.verbose          = if (testthat::is_testing()) -2,
    Require.verbose               = if (testthat::is_testing()) -2,
    Require.cloneFrom             = if (!testthat::is_testing()) Sys.getenv("R_LIBS_USER"),
    spades.moduleCodeChecks       = if (testthat::is_testing()) FALSE,
    spades.moduleDocument         = FALSE,
    SpaDES.project.updateRprofile = FALSE,
    teardownEnv                   = testthat::teardown_env()){

  localOptions <- list(
    reproducible.verbose          = reproducible.verbose,
    Require.verbose               = Require.verbose,
    Require.cloneFrom             = Require.cloneFrom,
    spades.moduleCodeChecks       = spades.moduleCodeChecks,
    spades.moduleDocument         = spades.moduleDocument,
    SpaDES.project.updateRprofile = SpaDES.project.updateRprofile
  )
  localOptions <- localOptions[!sapply(localOptions, is.null)]
  localOptions <- localOptions[!names(localOptions) %in% names(options())]

  withr::local_options(localOptions, .local_envir = teardownEnv)
}


# SpaDES test: muffle messages and warnings
SpaDEStestMuffleConditions <- function(
    expr, ...,
    handleConditions = testthat::is_testing(),
    suppressWarnings = getOption("spades.test.suppressWarnings", default = FALSE)){

  if (handleConditions | suppressWarnings){

    withCallingHandlers(
      expr,
      message               = function(c) tryInvokeRestart("muffleMessage"),
      packageStartupMessage = function(c) tryInvokeRestart("muffleMessage"),
      warning = function(w){
        if (suppressWarnings){
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

