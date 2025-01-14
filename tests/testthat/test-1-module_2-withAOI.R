
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module runs with study AOI", {

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
        projectPath = file.path(testDirs$temp$projects, "2-withAOI"),
        modulePath  = testDirs$temp$modules,
        inputPath   = testDirs$temp$inputs
        #, packagePath = testDirs$temp$libPath
      ),
      require = "testthat",

      dbPath     = .test_defaultInputs("dbPath"),
      spinupSQL  = .test_defaultInputs("spinupSQL"),
      species_tr = .test_defaultInputs("species_tr"),
      gcMeta     = .test_defaultInputs("gcMeta"),

      masterRaster = terra::rast(file.path(testDirs$testdata, "masterRaster-withAOI.tif"))
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

  for (colName in c("pixelIndex", "pixelGroup", "ages", "spatial_unit_id", "gcids", "ecozones")){
    expect_true(colName %in% names(simTest$spatialDT))
    expect_true(all(!is.na(simTest$spatialDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$spatialDT), "pixelIndex")

  # Expect that there is 1 row for every non-NA cell in masterRaster
  mrValues <- terra::values(terra::rast(file.path(testDirs$testdata, "masterRaster-withAOI.tif")))
  expect_equal(nrow(simTest$spatialDT), sum(!is.na(mrValues[,1])))
  expect_equal(simTest$spatialDT$pixelIndex, which(!is.na(mrValues[,1])))


  ## Check output 'level3DT' ----

  expect_true(!is.null(simTest$level3DT))
  expect_true(inherits(simTest$level3DT, "data.table"))

  for (colName in c("pixelGroup", "ages", "spatial_unit_id", "gcids", "ecozones", "return_interval")){
    expect_true(colName %in% names(simTest$level3DT))
    expect_true(all(!is.na(simTest$level3DT[[colName]])))
  }

  expect_identical(data.table::key(simTest$level3DT), "pixelGroup")

  # Expect that there is 1 row for every unique combination of key attributes in 'spatialDT'
  expect_equal(
    nrow(simTest$level3DT),
    nrow(unique(simTest$spatialDT[, c("ages", "spatial_unit_id", "gcids", "ecozones")]))
  )


  ## Check output 'speciesPixelGroup' ----

  expect_true(!is.null(simTest$speciesPixelGroup))
  expect_true(inherits(simTest$speciesPixelGroup, "data.table"))

  for (colName in c("pixelGroup", "species_id")){
    expect_true(colName %in% names(simTest$speciesPixelGroup))
    expect_true(all(!is.na(simTest$speciesPixelGroup[[colName]])))
  }

  # Check that there is 1 for every pixel group
  expect_equal(nrow(simTest$speciesPixelGroup), nrow(simTest$level3DT))
  expect_equal(sort(simTest$speciesPixelGroup$pixelGroup), simTest$level3DT$pixelGroup)


  ## Check output 'curveID' ----

  expect_true(!is.null(simTest$curveID))
  expect_true(length(simTest$curveID) >= 1)
  expect_true("gcids" %in% simTest$curveID)
  expect_true(all(simTest$curveID %in% names(simTest$level3DT)))


  ## Check output 'gcids' ----

  expect_true(!is.null(simTest$gcids))
  expect_true(inherits(simTest$gcids, "factor"))

  # Check that there is 1 for every pixel group
  expect_equal(length(simTest$gcids), nrow(simTest$level3DT))

  # Check that there are no NAs
  expect_true(all(!is.na(simTest$gcids)))


  ## Check output 'ecozones' ----

  expect_true(!is.null(simTest$ecozones))
  expect_true(class(simTest$ecozones) %in% c("integer", "numeric"))

  # Check that there is 1 for every pixel group
  expect_equal(length(simTest$ecozones), nrow(simTest$level3DT))

  # Check that there are no NAs
  expect_true(all(!is.na(simTest$ecozones)))


  ## Check output 'spatialUnits' ----

  expect_true(!is.null(simTest$spatialUnits))
  expect_true(class(simTest$spatialUnits) %in% c("integer", "numeric"))

  # Check that there is 1 for every pixel group
  expect_equal(length(simTest$spatialUnits), nrow(simTest$level3DT))

  # Check that there are no NAs
  expect_true(all(!is.na(simTest$spatialUnits)))


  ## Check output 'realAges' ----

  expect_true(!is.null(simTest$realAges))
  expect_true(class(simTest$realAges) %in% c("integer", "numeric"))

  # Check that the real ages match the original ages where <3 now equals 3
  expect_equal(simTest$realAges[simTest$realAges >= 3], simTest$level3DT$ages[simTest$realAges >= 3])
  expect_true(all(simTest$ages[simTest$realAges < 3] == 3))


  ## Check output 'delays' ----

  expect_true(!is.null(simTest$delays))
  expect_true(class(simTest$delays) %in% c("integer", "numeric"))

  # Check that there is 1 for every pixel group
  expect_equal(length(simTest$delays), nrow(simTest$level3DT))

  # By default: no delays
  expect_true(all(simTest$delays == 0))


  ## Check output 'mySpuDmids' ----

  expect_true(!is.null(simTest$mySpuDmids))
  expect_true(inherits(simTest$mySpuDmids, "data.table"))

  for (colName in c(
    "distName", "name", "description")){
    expect_true(colName %in% names(simTest$mySpuDmids))
    expect_true(is.character(simTest$mySpuDmids[[colName]]))
    expect_true(all(!is.na(simTest$mySpuDmids[[colName]])))
  }

  for (colName in c(
    "rasterID", "spatial_unit_id", "wholeStand",
    "disturbance_type_id", "disturbance_matrix_id")){
    expect_true(colName %in% names(simTest$mySpuDmids))
    expect_true(is.numeric(simTest$mySpuDmids[[colName]]) | is.integer(simTest$mySpuDmids[[colName]]))
    expect_true(all(!is.na(simTest$mySpuDmids[[colName]])))
  }


  ## Check output 'historicDMtype' ----

  expect_true(!is.null(simTest$historicDMtype))
  expect_true(class(simTest$historicDMtype) %in% c("integer", "numeric"))

  # Check that there is 1 for every pixel group
  expect_equal(length(simTest$historicDMtype), nrow(simTest$level3DT))

  # Check that there are no NAs
  expect_true(all(!is.na(simTest$historicDMtype)))


  ## Check output 'lastPassDMtype' ----

  expect_true(!is.null(simTest$lastPassDMtype))
  expect_true(class(simTest$lastPassDMtype) %in% c("integer", "numeric"))

  # Check that there is 1 for every pixel group
  expect_equal(length(simTest$lastPassDMtype), nrow(simTest$level3DT))

  # Check that there are no NAs
  expect_true(all(!is.na(simTest$lastPassDMtype)))


  ## Check output 'disturbanceRasters' -----

  expect_true(!is.null(simTest$disturbanceRasters))
  expect_true(inherits(simTest$disturbanceRasters, "character"))

  # Check at least one file was downloaded
  expect_true(length(simTest$disturbanceRasters) >= 1)
  expect_true(all(file.exists(simTest$disturbanceRasters)))

})

