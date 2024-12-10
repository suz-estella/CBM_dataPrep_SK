
## Set up ----

  if (!testthat::is_testing()){
    library(testthat)
    source(file.path(testthat::test_path(), "setup.R"))
  }

  # Set project path
  projectPath <- file.path(testDirs$outputs, "1-module_2-withAOI")
  dir.create(projectPath)

  # Set inputs
  inputObjects <- moduleInputs

  ## Set study area
  inputObjects$masterRaster <- {
    extent = terra::ext(c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183))
    masterRaster <- terra::rast(extent, res = 30)
    terra::crs(masterRaster) <- "PROJCRS[\"Lambert_Conformal_Conic_2SP\",\n    BASEGEOGCRS[\"GCS_GRS_1980_IUGG_1980\",\n        DATUM[\"D_unknown\",\n            ELLIPSOID[\"GRS80\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-95,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",77,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"
    masterRaster[] <- rep(1, terra::ncell(masterRaster))
    mr <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1zUyFH8k6Ef4c_GiWMInKbwAl6m6gvLJW/view?usp=drive_link",
                                   destinationPath = testDirs$inputs,
                                   to = masterRaster,
                                   method = "near")
    mr[mr[] == 0] <- NA
    mr
  }


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

    expect_equal(nrow(sims$spades$spatialDT), 6763)

    expect_equal(sapply(sims$spades$spatialDT, sum), c(
      ages            = 595977,
      spatial_unit_id = 189364,
      pixelIndex      = 108042911,
      gcids           = 339027,
      ecozones        = 60867,
      pixelGroup      = 136274
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

    expect_equal(nrow(sims$spades$level3DT), 41)

    expect_equal(sapply(lapply(sims$spades$level3DT, as.numeric), sum), c(
      spatial_unit_id = 1148,
      ages            = 3418,
      gcids           = 83,
      ecozones        = 369,
      pixelGroup      = 861,
      return_interval = 5125
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
    expect_equal(length(sims$spades$gcids), 41)
    expect_identical(sims$spades$gcids, factor(
      c(49, 50, 52, 61, 49, 50, 61, 49, 50, 49, 50, 52, 50, 49, 52, 50, 50, 49,
        52, 50, 49, 58, 49, 50, 50, 49, 49, 50, 52, 49, 50, 49, 50, 49, 50, 49,
        50, 50, 49, 50, 61),
      levels = c(49, 50, 52, 58, 61)
    ))
  })

  test_that("'realAges' output created", {

    expect_true(!is.null(sims$spades$realAges))
    expect_true(inherits(sims$spades$realAges, "numeric"))
    expect_equal(length(sims$spades$realAges), 41)
    expect_identical(sims$spades$realAges, c(
      100, 100, 100, 100, 101, 101, 101, 102, 102, 109, 109, 11, 110, 12, 12,
      128, 129, 13, 13, 130, 14, 79, 81, 81, 82, 88, 89, 89, 9, 90, 90, 91, 91,
      92, 92, 93, 93, 94, 99, 99, 99
    ))
  })

  test_that("'delays' output created", {

    expect_true(!is.null(sims$spades$delays))
    expect_true(inherits(sims$spades$delays, "numeric"))
    expect_equal(length(sims$spades$delays), 41)
    expect_equal(sims$spades$delays, rep(0, 41))
  })

  test_that("'ecozones' output created", {

    expect_true(!is.null(sims$spades$ecozones))
    expect_true(inherits(sims$spades$ecozones, "numeric"))
    expect_equal(length(sims$spades$ecozones), 41)
    expect_equal(sims$spades$ecozones, rep(9, 41))
  })

  test_that("'spatialUnits' output created", {

    expect_true(!is.null(sims$spades$spatialUnits))
    expect_true(inherits(sims$spades$spatialUnits, "integer"))
    expect_equal(length(sims$spades$spatialUnits), 41)
    expect_equal(sims$spades$spatialUnits, rep(28, 41))

  })

  test_that("'speciesPixelGroup' output created", {

    expect_true(!is.null(sims$spades$speciesPixelGroup))
    expect_true(inherits(sims$spades$speciesPixelGroup, "data.table"))

    expect_equal(ncol(sims$spades$speciesPixelGroup), 2)
    expect_identical(data.table::key(sims$spades$speciesPixelGroup), NULL)
    for (colName in c("pixelGroup", "species_id")){
      expect_true(colName %in% names(sims$spades$speciesPixelGroup))
    }

    expect_equal(nrow(sims$spades$speciesPixelGroup), 41)

    expect_equal(sapply(sims$spades$speciesPixelGroup, sum), c(
      pixelGroup = 861,
      species_id = 228
    ))
  })

  test_that("'mySpuDmids' output created", {

    expect_true(!is.null(sims$spades$mySpuDmids))
    expect_true(inherits(sims$spades$mySpuDmids, "data.table"))

    expect_equal(ncol(sims$spades$mySpuDmids), 8)
    expect_identical(data.table::key(sims$spades$mySpuDmids), NULL)

    for (colName in c(
      "distName", "rasterID", "spatial_unit_id", "wholeStand",
      "disturbance_type_id", "disturbance_matrix_id", "name", "description")){
      expect_true(colName %in% names(sims$spades$mySpuDmids))
    }

    expect_equal(nrow(sims$spades$mySpuDmids), 5)

    expect_identical(sims$spades$mySpuDmids$distName, c(
      "wildfire", "clearcut", "deforestation", "20% mortality", "20% mortality"))

    expect_identical(sims$spades$mySpuDmids$name, c(
      "Wildfire", "Clearcut harvesting without salvage", "Deforestation",
      "Generic 20% mortality", "Generic 20% mortality"))

    expect_identical(sims$spades$mySpuDmids$description, c(
      "Wildfire for Saskatchewan - Boreal Plains (NIR2011)",
      "Default Stand Replacing Matrix for Clearcut Harvest without Salvage",
      "Deforestation Matrix #1 (Stand Replacing). Traditionally used for all ecozones across Canada.",
      "Generic 20% mortality", "Generic 20% mortality"))

    colsSum <- c("rasterID", "spatial_unit_id", "wholeStand",
                 "disturbance_type_id", "disturbance_matrix_id")
    expect_equal(sapply(sims$spades$mySpuDmids[,..colsSum], sum), c(
      rasterID              = 15,
      spatial_unit_id       = 140,
      wholeStand            = 3,
      disturbance_type_id   = 548,
      disturbance_matrix_id = 739
    ))
  })

  test_that("'historicDMtype' output created", {

    expect_true(!is.null(sims$spades$historicDMtype))
    expect_true(inherits(sims$spades$historicDMtype, "integer"))
    expect_equal(length(sims$spades$historicDMtype), 41)
    expect_equal(sims$spades$historicDMtype, rep(1, 41))

  })

  test_that("'lastPassDMtype' output created", {

    expect_true(!is.null(sims$spades$historicDMtype))
    expect_true(inherits(sims$spades$lastPassDMtype, "integer"))
    expect_equal(length(sims$spades$lastPassDMtype), 41)
    expect_equal(sims$spades$lastPassDMtype, rep(1, 41))

  })

  test_that("'disturbanceRasters' output created", {

    expect_true(!is.null(sims$spades$disturbanceRasters))
    expect_true(inherits(sims$spades$disturbanceRasters, "character"))
    expect_equal(length(sims$spades$disturbanceRasters), 27)
    expect_identical(basename(sims$spades$disturbanceRasters), paste0("SaskDist_", 1985:2011, ".grd"))

  })

