
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module runs with defaults", {

  ## Run simInit and spades ----

  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "1-defaults")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  # Set up project
  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      times = list(start = 1985, end = 2011),

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

      dbPath     = file.path(spadesTestPaths$temp$inputs, "cbm_defaults_v1.2.8340.362.db"),
      ecoLocator = sf::st_read(file.path(spadesTestPaths$testdata, "ecoLocator.shp"), quiet = TRUE),
      spuLocator = sf::st_read(file.path(spadesTestPaths$testdata, "spuLocator.shp"), quiet = TRUE),
      dMatrixAssociation = read.csv(file.path(spadesTestPaths$testdata, "disturbance_matrix_association.csv")),
      spinupSQL  = read.csv(file.path(spadesTestPaths$testdata, "spinupSQL.csv")),
      species_tr = read.csv(file.path(spadesTestPaths$testdata, "species_tr.csv")),
      gcMeta     = read.csv(file.path(spadesTestPaths$temp$inputs, "gcMetaEg.csv"))
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


  ## Check output 'spatialDT' ----

  expect_true(!is.null(simTest$spatialDT))
  expect_true(inherits(simTest$spatialDT, "data.table"))

  for (colName in c("pixelIndex", "pixelGroup", "ages", "spatial_unit_id", "gcids", "ecozones")){
    expect_true(colName %in% names(simTest$spatialDT))
    expect_true(all(!is.na(simTest$spatialDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$spatialDT), "pixelIndex")


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

  # Expect that 'gcids' is a factor
  expect_true(is.factor(simTest$level3DT$gcids))


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


  ## Check output 'disturbanceEvents' -----

  expect_true(!is.null(simTest$disturbanceEvents))
  expect_true(inherits(simTest$disturbanceEvents, "data.table"))

  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(simTest$disturbanceEvents))
    expect_true(is.integer(simTest$disturbanceEvents[[colName]]))
    expect_true(all(!is.na(simTest$disturbanceEvents[[colName]])))
  }

  expect_true(all(simTest$disturbanceEvents$pixelIndex %in% simTest$allPixDT$pixelIndex))
  expect_true(all(simTest$disturbanceEvents$year       %in% 1985:2011))

  expect_equal(nrow(simTest$disturbanceEvents), 295569)


  ## Check output 'disturbanceMeta' ----

  expect_true(!is.null(simTest$disturbanceMeta))
  expect_true(inherits(simTest$disturbanceMeta, "data.table"))

  expect_equal(nrow(simTest$disturbanceMeta), 20)


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

})


