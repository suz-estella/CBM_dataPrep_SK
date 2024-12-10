
## Set up ----

  if (!testthat::is_testing()){
    library(testthat)
    source(file.path(testthat::test_path(), "setup.R"))
  }

  # Set project path
  projectPath <- file.path(testDirs$outputs, "1-module_1-defaults")
  dir.create(projectPath)

  # Set inputs
  inputObjects <- moduleInputs


## Run simInit and spades ----

  sims <- list()

  # simInit
  sims$init <- tryCatch(

    SpaDES.core::simInit(
      modules = "CBM_dataPrep_SK",
      objects = inputObjects,
      paths   = list(
        modulePath  = dirname(testDirs$module),
        inputPath   = file.path(projectPath, "inputs"),
        outputPath  = file.path(projectPath, "outputs"),
        cachePath   = file.path(projectPath, "cache"),
        scratchPath = file.path(projectPath, "scratch"),
        rasterPath  = file.path(projectPath, "raster"),
        terraPath   = file.path(projectPath, "terra")
      )
    ),

    error = function(e) if (testthat::is_testing()) return(e) else stop(e)
  )

  test_that("simInit runs successfully", {
    expect_s4_class(sims$init, "simList")
  })
  if (!inherits(sims$init, "simList")) stop(
    "SpaDES.core::simInit failed",
    if (inherits(sims$init, "error")) c(":\n", sims$init$message),
    call. = F)

  # spades
  sims$spades <- tryCatch(

    SpaDES.core::spades(reproducible::Copy(sims$init)),

    error = function(e) if (testthat::is_testing()) return(e) else stop(e)
  )

  test_that("spades runs successfully", {

    expect_s4_class(sims$spades, "simList")
  })

  if (!inherits(sims$spades, "simList")) stop(
    "SpaDES.core::spades failed",
    if (inherits(sims$spades, "error")) c(":\n", sims$spades$message),
    call. = FALSE)


## Test ----

  test_that("'spatialDT' output created", {

    expect_true(!is.null(sims$spades$spatialDT))
    expect_true(inherits(sims$spades$spatialDT, "data.table"))

    expect_equal(ncol(sims$spades$spatialDT), 6)
    expect_identical(data.table::key(sims$spades$spatialDT), "pixelIndex")

    for (colName in c("ages", "spatial_unit_id", "pixelIndex", "gcids", "ecozones", "pixelGroup")){
      expect_true(colName %in% names(sims$spades$spatialDT))
    }

    expect_equal(nrow(sims$spades$spatialDT), 1347529)

    expect_equal(sapply(sims$spades$spatialDT, sum), c(
      ages            = 113392380,
      spatial_unit_id = 37277212,
      pixelIndex      = 2335033436340,
      gcids           = 60978015,
      ecozones        = 10766961,
      pixelGroup      = 473854911
    ))
  })

  test_that("'level3DT' output created", {

    expect_true(!is.null(sims$spades$level3DT))
    expect_true(inherits(sims$spades$level3DT, "data.table"))

    expect_equal(ncol(sims$spades$level3DT), 6)
    expect_identical(data.table::key(sims$spades$level3DT), "pixelGroup")
    for (colName in c("spatial_unit_id", "ages", "gcids", "ecozones", "pixelGroup", "return_interval")){
      expect_true(colName %in% names(sims$spades$level3DT))
    }

    expect_equal(nrow(sims$spades$level3DT), 739)

    expect_equal(sapply(lapply(sims$spades$level3DT, as.numeric), sum), c(
      spatial_unit_id = 20371,
      ages            = 58590,
      gcids           = 5632,
      ecozones        = 5688,
      pixelGroup      = 273430,
      return_interval = 76325
    ))
  })

  test_that("'curveID' output created", {

    expect_true(!is.null(sims$spades$curveID))
    expect_equal(length(sims$spades$curveID), 1)
    expect_identical(sims$spades$curveID, "gcids")

  })

  test_that("'gcids' output created", {

    expect_true(!is.null(sims$spades$gcids))
    expect_true(inherits(sims$spades$gcids, "factor"))
    expect_equal(length(sims$spades$gcids), 739)
    expect_equal(sum(as.numeric(sims$spades$gcids)), 5632)
  })

  test_that("'realAges' output created", {

    expect_true(!is.null(sims$spades$realAges))
    expect_true(inherits(sims$spades$realAges, "numeric"))
    expect_equal(length(sims$spades$realAges), 739)
    expect_equal(sum(as.numeric(sims$spades$realAges)), 58580)
  })

  test_that("'delays' output created", {

    expect_true(!is.null(sims$spades$delays))
    expect_true(inherits(sims$spades$delays, "numeric"))
    expect_equal(length(sims$spades$delays), 739)
    expect_equal(sims$spades$delays, rep(0, 739))
  })

  test_that("'ecozones' output created", {

    expect_true(!is.null(sims$spades$ecozones))
    expect_true(inherits(sims$spades$ecozones, "numeric"))
    expect_equal(length(sims$spades$ecozones), 739)
    expect_true(all(sims$spades$ecozones %in% c(6, 9)))
  })

  test_that("'spatialUnits' output created", {

    expect_true(!is.null(sims$spades$spatialUnits))
    expect_true(inherits(sims$spades$spatialUnits, "integer"))
    expect_equal(length(sims$spades$spatialUnits), 739)
    expect_true(all(sims$spades$spatialUnits %in% c(27, 28)))

  })

  test_that("'speciesPixelGroup' output created", {

    expect_true(!is.null(sims$spades$speciesPixelGroup))
    expect_true(inherits(sims$spades$speciesPixelGroup, "data.table"))

    expect_equal(ncol(sims$spades$speciesPixelGroup), 2)
    expect_identical(data.table::key(sims$spades$speciesPixelGroup), NULL)
    for (colName in c("pixelGroup", "species_id")){
      expect_true(colName %in% names(sims$spades$speciesPixelGroup))
    }

    expect_equal(nrow(sims$spades$speciesPixelGroup), 739)

    expect_equal(sapply(sims$spades$speciesPixelGroup, sum), c(
      pixelGroup = 273430,
      species_id = 20529
    ))
  })

  test_that("'mySpuDmids' output created", {

    expect_true(!is.null(sims$spades$mySpuDmids))
    expect_true(inherits(sims$spades$mySpuDmids, "data.table"))

    expect_equal(ncol(sims$spades$mySpuDmids), 8)
    expect_identical(data.table::key(sims$spades$mySpuDmids), NULL)

    for (colName in c("distName", "rasterID", "spatial_unit_id", "wholeStand",
                      "disturbance_type_id", "disturbance_matrix_id", "name", "description")){
      expect_true(colName %in% names(sims$spades$mySpuDmids))
    }

    expect_equal(nrow(sims$spades$mySpuDmids), 10)

    expect_identical(sims$spades$mySpuDmids$distName, c(
      "wildfire", "clearcut", "deforestation", "20% mortality", "20% mortality",
      "wildfire", "clearcut", "deforestation", "20% mortality", "20% mortality"))

    expect_identical(sims$spades$mySpuDmids$name, c(
      "Generic 20% mortality", "Generic 20% mortality", "Deforestation", "Deforestation", "Deforestation",
      "Generic 20% mortality", "Generic 20% mortality", "Deforestation", "Deforestation", "Deforestation"))

    expect_identical(sims$spades$mySpuDmids$description, c(
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
    expect_equal(sapply(sims$spades$mySpuDmids[,..colsSum], sum), c(
      rasterID              = 30,
      spatial_unit_id       = 275,
      wholeStand            = 6,
      disturbance_type_id   = 714,
      disturbance_matrix_id = 520
    ))
  })

  test_that("'historicDMtype' output created", {

    expect_true(!is.null(sims$spades$historicDMtype))
    expect_true(inherits(sims$spades$historicDMtype, "integer"))
    expect_equal(length(sims$spades$historicDMtype), 739)
    expect_equal(sims$spades$historicDMtype, rep(168, 739))

  })

  test_that("'lastPassDMtype' output created", {

    expect_true(!is.null(sims$spades$historicDMtype))
    expect_true(inherits(sims$spades$lastPassDMtype, "integer"))
    expect_equal(length(sims$spades$lastPassDMtype), 739)
    expect_equal(sims$spades$lastPassDMtype, rep(168, 739))

  })

  test_that("'disturbanceRasters' output created", {

    expect_true(!is.null(sims$spades$disturbanceRasters))
    expect_true(inherits(sims$spades$disturbanceRasters, "character"))
    expect_equal(length(sims$spades$disturbanceRasters), 27)
    expect_identical(basename(sims$spades$disturbanceRasters), paste0("SaskDist_", 1985:2011, ".grd"))

  })

