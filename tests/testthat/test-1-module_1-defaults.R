
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module runs with defaults", {

  ## Run simInit and spades ----

  # Restore paths on teardown
  pathsOriginal <- list(wd = getwd(), libs = .libPaths())
  withr::defer({
    setwd(pathsOriginal$wd)
    #.libPaths(pathsOriginal$libs)
  })

  # Set up project
  simInitInput <- .SpaDESwithCallingHandlers(

    SpaDES.project::setupProject(

      modules = "CBM_dataPrep_SK",
      paths   = list(
        projectPath = file.path(testDirs$temp$projects, "1-defaults"),
        modulePath  = dirname(testDirs$module)#,
        #packagePath = testDirs$temp$libPath
      ),
      require = "testthat",

      dbPath     = .test_defaultInputs("dbPath"),
      spinupSQL  = .test_defaultInputs("spinupSQL"),
      species_tr = .test_defaultInputs("species_tr"),
      gcMeta     = .test_defaultInputs("gcMeta")
    )
  )

  # Run simInit
  simTestInit <- .SpaDESwithCallingHandlers(
    SpaDES.core::simInit2(simInitInput)
  )

  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- .SpaDESwithCallingHandlers(
    SpaDES.core::spades(simTestInit)
  )

  expect_s4_class(simTest, "simList")


  ## Check output 'spatialDT' ----

  expect_true(!is.null(simTest$spatialDT))
  expect_true(inherits(simTest$spatialDT, "data.table"))

  expect_equal(ncol(simTest$spatialDT), 6)
  expect_identical(data.table::key(simTest$spatialDT), "pixelIndex")

  for (colName in c("ages", "spatial_unit_id", "pixelIndex", "gcids", "ecozones", "pixelGroup")){
    expect_true(colName %in% names(simTest$spatialDT))
  }

  expect_equal(nrow(simTest$spatialDT), 1347529)

  expect_equal(sapply(simTest$spatialDT, sum), c(
    ages            = 113392380,
    spatial_unit_id = 37277212,
    pixelIndex      = 2335033436340,
    gcids           = 60978015,
    ecozones        = 10766961,
    pixelGroup      = 473854911
  ))


  ## Check output 'level3DT' ----

  expect_true(!is.null(simTest$level3DT))
  expect_true(inherits(simTest$level3DT, "data.table"))

  expect_equal(ncol(simTest$level3DT), 6)
  expect_identical(data.table::key(simTest$level3DT), "pixelGroup")
  for (colName in c("spatial_unit_id", "ages", "gcids", "ecozones", "pixelGroup", "return_interval")){
    expect_true(colName %in% names(simTest$level3DT))
  }

  expect_equal(nrow(simTest$level3DT), 739)

  expect_equal(sapply(lapply(simTest$level3DT, as.numeric), sum), c(
    spatial_unit_id = 20371,
    ages            = 58590,
    gcids           = 5632,
    ecozones        = 5688,
    pixelGroup      = 273430,
    return_interval = 76325
  ))


  ## Check output 'curveID' ----

  expect_true(!is.null(simTest$curveID))
  expect_equal(length(simTest$curveID), 1)
  expect_identical(simTest$curveID, "gcids")


  ## Check output 'gcids' ----

  expect_true(!is.null(simTest$gcids))
  expect_true(inherits(simTest$gcids, "factor"))
  expect_equal(length(simTest$gcids), 739)
  expect_equal(sum(as.numeric(simTest$gcids)), 5632)


  ## Check output 'realAges' ----

  expect_true(!is.null(simTest$realAges))
  expect_true(class(simTest$realAges) %in% c("integer", "numeric"))
  expect_equal(length(simTest$realAges), 739)
  expect_equal(sum(as.numeric(simTest$realAges)), 58580)


  ## Check output 'delays' ----

  expect_true(!is.null(simTest$delays))
  expect_true(class(simTest$delays) %in% c("integer", "numeric"))
  expect_equal(length(simTest$delays), 739)
  expect_equal(simTest$delays, rep(0, 739))


  ## Check output 'ecozones' ----

  expect_true(!is.null(simTest$ecozones))
  expect_true(class(simTest$ecozones) %in% c("integer", "numeric"))
  expect_equal(length(simTest$ecozones), 739)
  expect_true(all(simTest$ecozones %in% c(6, 9)))


  ## Check output 'spatialUnits' ----

  expect_true(!is.null(simTest$spatialUnits))
  expect_true(class(simTest$spatialUnits) %in% c("integer", "numeric"))
  expect_equal(length(simTest$spatialUnits), 739)
  expect_true(all(simTest$spatialUnits %in% c(27, 28)))


  ## Check output 'speciesPixelGroup' ----

  expect_true(!is.null(simTest$speciesPixelGroup))
  expect_true(inherits(simTest$speciesPixelGroup, "data.table"))

  expect_equal(ncol(simTest$speciesPixelGroup), 2)
  expect_identical(data.table::key(simTest$speciesPixelGroup), NULL)
  for (colName in c("pixelGroup", "species_id")){
    expect_true(colName %in% names(simTest$speciesPixelGroup))
  }

  expect_equal(nrow(simTest$speciesPixelGroup), 739)

  expect_equal(sapply(simTest$speciesPixelGroup, sum), c(
    pixelGroup = 273430,
    species_id = 20529
  ))


  ## Check output 'mySpuDmids' ----

  expect_true(!is.null(simTest$mySpuDmids))
  expect_true(inherits(simTest$mySpuDmids, "data.table"))

  expect_equal(ncol(simTest$mySpuDmids), 8)
  expect_identical(data.table::key(simTest$mySpuDmids), NULL)

  for (colName in c("distName", "rasterID", "spatial_unit_id", "wholeStand",
                    "disturbance_type_id", "disturbance_matrix_id", "name", "description")){
    expect_true(colName %in% names(simTest$mySpuDmids))
  }

  expect_equal(nrow(simTest$mySpuDmids), 10)

  expect_identical(simTest$mySpuDmids$distName, c(
    "wildfire", "clearcut", "deforestation", "20% mortality", "20% mortality",
    "wildfire", "clearcut", "deforestation", "20% mortality", "20% mortality"))

  expect_identical(simTest$mySpuDmids$name, c(
    "Generic 20% mortality", "Generic 20% mortality", "Deforestation", "Deforestation", "Deforestation",
    "Generic 20% mortality", "Generic 20% mortality", "Deforestation", "Deforestation", "Deforestation"))

  expect_identical(simTest$mySpuDmids$description, c(
    "Generic 20% mortality", "Generic 20% mortality",
    "Deforestation Matrix #1 (Stand Replacing). Traditionally used for all ecozones across Canada.",
    "Deforestation Matrix #1 (Stand Replacing). Traditionally used for all ecozones across Canada.",
    "Deforestation Matrix #1 (Stand Replacing). Traditionally used for all ecozones across Canada.",
    "Generic 20% mortality", "Generic 20% mortality",
    "Deforestation Matrix #1 (Stand Replacing). Traditionally used for all ecozones across Canada.",
    "Deforestation Matrix #1 (Stand Replacing). Traditionally used for all ecozones across Canada.",
    "Deforestation Matrix #1 (Stand Replacing). Traditionally used for all ecozones across Canada."))

  colsSum <- c("rasterID", "spatial_unit_id", "wholeStand",
               "disturbance_type_id", "disturbance_matrix_id")
  expect_equal(sapply(simTest$mySpuDmids[,..colsSum], sum), c(
    rasterID              = 30,
    spatial_unit_id       = 275,
    wholeStand            = 6,
    disturbance_type_id   = 714,
    disturbance_matrix_id = 520
  ))


  ## Check output 'historicDMtype' ----

  expect_true(!is.null(simTest$historicDMtype))
  expect_true(class(simTest$historicDMtype) %in% c("integer", "numeric"))
  expect_equal(length(simTest$historicDMtype), 739)
  expect_equal(simTest$historicDMtype, rep(168, 739))


  ## Check output 'lastPassDMtype' ----

  expect_true(!is.null(simTest$lastPassDMtype))
  expect_true(class(simTest$lastPassDMtype) %in% c("integer", "numeric"))
  expect_equal(length(simTest$lastPassDMtype), 739)
  expect_equal(simTest$lastPassDMtype, rep(168, 739))


  ## Check output 'disturbanceRasters' ----

  expect_true(!is.null(simTest$disturbanceRasters))
  expect_true(inherits(simTest$disturbanceRasters, "character"))
  expect_equal(length(simTest$disturbanceRasters), 27)
  expect_identical(basename(simTest$disturbanceRasters), paste0("SaskDist_", 1985:2011, ".grd"))

})


