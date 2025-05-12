
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module runs with study AOI", {

  ## Run simInit and spades ----

  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "2-withAOI")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  # Set up project
  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      times = list(start = 1998, end = 2000),

      modules = "CBM_dataPrep_SK",
      paths   = list(
        projectPath = projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        packagePath = spadesTestPaths$temp$packages,
        inputPath   = spadesTestPaths$temp$inputs,
        cachePath   = spadesTestPaths$temp$cache,
        outputPath  = file.path(projectPath, "outputs")
      ),

      require = "sf",

      dbPath     = file.path(spadesTestPaths$temp$inputs, "dbPath.db"),
      ecoLocator = sf::st_read(file.path(spadesTestPaths$testdata, "ecoLocator.shp"), quiet = TRUE),
      spuLocator = sf::st_read(file.path(spadesTestPaths$testdata, "spuLocator.shp"), quiet = TRUE),
      disturbanceMatrix = read.csv(file.path(spadesTestPaths$testdata, "disturbance_matrix_association.csv")),

      masterRaster = file.path(spadesTestPaths$testdata, "masterRaster-withAOI.tif"),

      # Test matching user disturbances with CBM-CFS3 disturbances
      userDist = rbind(
        data.frame(rasterID = 1, wholeStand = 1, distName = "Wildfire"),
        data.frame(rasterID = 2, wholeStand = 1, distName = "Clearcut harvesting without salvage"),
        data.frame(rasterID = 3, wholeStand = 0, distName = "Generic 20% mortality"),
        data.frame(rasterID = 4, wholeStand = 1, distName = "Deforestation")
      )
    )
  )

  # Run simInit
  simTestInit <- SpaDEStestMuffleOutput(
    SpaDES.core::simInit2(simInitInput)
  )

  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDEStestMuffleOutput(
    SpaDES.core::spades(simTestInit)
  )

  expect_s4_class(simTest, "simList")


  ## Check output 'standDT' ----

  expect_true(!is.null(simTest$standDT))
  expect_true(inherits(simTest$standDT, "data.table"))

  for (colName in c("pixelIndex", "area", "spatial_unit_id")){
    expect_true(colName %in% names(simTest$standDT))
    expect_true(all(!is.na(simTest$standDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$standDT), "pixelIndex")

  # Expect that there is 1 row for every non-NA cell in masterRaster
  mrValues <- terra::values(terra::rast(file.path(spadesTestPaths$testdata, "masterRaster-withAOI.tif")))
  expect_equal(nrow(simTest$standDT), sum(!is.na(mrValues[,1])))
  expect_equal(simTest$standDT$pixelIndex, which(!is.na(mrValues[,1])))


  ## Check output 'cohortDT' ----

  expect_true(!is.null(simTest$cohortDT))
  expect_true(inherits(simTest$cohortDT, "data.table"))

  for (colName in c("cohortID", "pixelIndex", "gcids", "ages")){
    expect_true(colName %in% names(simTest$cohortDT))
    expect_true(all(!is.na(simTest$cohortDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$cohortDT), "cohortID")

  # Check spinup ages are all >= 3
  expect_true("ageSpinup" %in% names(simTest$cohortDT))
  expect_equal(simTest$cohortDT$ageSpinup[simTest$cohortDT$ages >= 3],
               simTest$cohortDT$ages[simTest$cohortDT$ages >= 3])
  expect_true(all(simTest$ageSpinup[simTest$cohortDT$ages < 3] == 3))

  # Expect that there is 1 row for every non-NA cell in masterRaster
  expect_equal(nrow(simTest$cohortDT), sum(!is.na(mrValues[,1])))
  expect_equal(simTest$cohortDT$pixelIndex, which(!is.na(mrValues[,1])))


  ## Check output 'curveID' ----

  expect_true(!is.null(simTest$curveID))
  expect_true(length(simTest$curveID) >= 1)
  expect_true("gcids" %in% simTest$curveID)
  expect_true(all(simTest$curveID %in% names(simTest$cohortDT)))


  ## Check output 'gcMeta' ----

  expect_true(!is.null(simTest$gcMeta))
  expect_true(inherits(simTest$gcMeta, "data.table"))

  for (colName in c("gcids", "species_id", "sw_hw")){
    expect_true(colName %in% names(simTest$gcMeta))
    expect_true(all(!is.na(simTest$gcMeta[[colName]])))
  }


  ## Check output 'userGcM3' ----

  expect_true(!is.null(simTest$userGcM3))
  expect_true(inherits(simTest$userGcM3, "data.table"))

  for (colName in c("gcids", "Age", "MerchVolume")){
    expect_true(colName %in% names(simTest$userGcM3))
    expect_true(all(!is.na(simTest$userGcM3[[colName]])))
  }


  ## Check output 'disturbanceEvents' -----

  expect_true(!is.null(simTest$disturbanceEvents))
  expect_true(inherits(simTest$disturbanceEvents, "data.table"))

  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(simTest$disturbanceEvents))
    expect_true(is.integer(simTest$disturbanceEvents[[colName]]))
    expect_true(all(!is.na(simTest$disturbanceEvents[[colName]])))
  }

  expect_true(all(simTest$disturbanceEvents$pixelIndex %in% simTest$allPixDT$pixelIndex))
  expect_true(all(simTest$disturbanceEvents$year       %in% 1998:2000))

  expect_equal(nrow(simTest$disturbanceEvents), 1393)


  ## Check output 'disturbanceMeta' ----

  expect_true(!is.null(simTest$disturbanceMeta))
  expect_true(inherits(simTest$disturbanceMeta, "data.table"))

  expect_equal(nrow(simTest$disturbanceMeta), 8)

  # Check that disturbances have been matched correctly
  rowsExpect <- rbind(
    data.frame(
      spatial_unit_id       = 28,
      rasterID              = 1,
      wholeStand            = 1,
      sw_hw                 = c("sw", "hw"),
      distName              = "Wildfire",
      disturbance_type_id   = 1,
      disturbance_matrix_id = c(371, 851)
    ),
    data.frame(
      spatial_unit_id       = 28,
      rasterID              = 2,
      wholeStand            = 1,
      sw_hw                 = c("sw", "hw"),
      distName              = "Clearcut harvesting without salvage",
      disturbance_type_id   = 204,
      disturbance_matrix_id = c(160, 640)
    ),
    data.frame(
      spatial_unit_id       = 28,
      rasterID              = 3,
      wholeStand            = 0,
      sw_hw                 = c("sw", "hw"),
      distName              = "Generic 20% mortality",
      disturbance_type_id   = 168,
      disturbance_matrix_id = c(91, 571)
    ),
    data.frame(
      spatial_unit_id       = 28,
      rasterID              = 4,
      wholeStand            = 1,
      sw_hw                 = c("sw", "hw"),
      distName              = "Deforestation",
      disturbance_type_id   = 7,
      disturbance_matrix_id = c(26, 506)
    )
  )

  for (i in 1:nrow(rowsExpect)){
    expect_equal(nrow(merge(simTest$disturbanceMeta, rowsExpect[i,], by = names(rowsExpect))), 1)
  }
})

