
if (!testthat::is_testing()){
  library(testthat)
  source(file.path(testthat::test_path(), "setup.R"))
}

test_that("module runs with study AOI", {

  ## Run simInit and spades ----

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

  # Run simInit and spades
  simTestInit <- withCallingHandlers(suppressMessages(

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
      ))

  ), warning = function(w){
    if (grepl("^package ['\u2018]{1}[a-zA-Z0-9.]+['\u2019]{1} was built under R version [0-9.]+$", w$message)){
      invokeRestart("muffleWarning")
    }
  })

  expect_s4_class(simTestInit, "simList")
  if (!inherits(simTestInit, "simList")) stop("simInit failed", call. = FALSE)

  simTest <- suppressMessages(
    SpaDES.core::spades(simTestInit)
  )

  expect_s4_class(simTest, "simList")
  if (!inherits(simTest, "simList")) stop("spades failed", call. = FALSE)


  ## Check output 'spatialDT' ----

  expect_true(!is.null(simTest$spatialDT))
  expect_true(inherits(simTest$spatialDT, "data.table"))

  expect_equal(ncol(simTest$spatialDT), 6)
  expect_identical(data.table::key(simTest$spatialDT), "pixelIndex")

  for (colName in c("ages", "spatial_unit_id", "pixelIndex", "gcids", "ecozones", "pixelGroup")){
    expect_true(colName %in% names(simTest$spatialDT))
  }

  expect_equal(nrow(simTest$spatialDT), 6763)

  expect_equal(sapply(simTest$spatialDT, sum), c(
    ages            = 595977,
    spatial_unit_id = 189364,
    pixelIndex      = 108042911,
    gcids           = 339027,
    ecozones        = 60867,
    pixelGroup      = 136274
  ))


  ## Check output 'level3DT' ----

  expect_true(!is.null(simTest$level3DT))
  expect_true(inherits(simTest$level3DT, "data.table"))

  expect_equal(ncol(simTest$level3DT), 6)
  expect_identical(data.table::key(simTest$level3DT), "pixelGroup")
  for (colName in c("spatial_unit_id", "ages", "gcids", "ecozones", "pixelGroup", "return_interval")){
    expect_true(colName %in% names(simTest$level3DT))
  }

  expect_equal(nrow(simTest$level3DT), 41)

  expect_equal(sapply(lapply(simTest$level3DT, as.numeric), sum), c(
    spatial_unit_id = 1148,
    ages            = 3418,
    gcids           = 83,
    ecozones        = 369,
    pixelGroup      = 861,
    return_interval = 5125
  ))


  ## Check output 'curveID' ----

  expect_true(!is.null(simTest$curveID))
  expect_equal(length(simTest$curveID), 1)
  expect_identical(simTest$curveID, "gcids")


  ## Check output 'gcids' ----

  expect_true(!is.null(simTest$gcids))
  expect_true(inherits(simTest$gcids, "factor"))
  expect_equal(length(simTest$gcids), 41)
  expect_identical(simTest$gcids, factor(
    c(49, 50, 52, 61, 49, 50, 61, 49, 50, 49, 50, 52, 50, 49, 52, 50, 50, 49,
      52, 50, 49, 58, 49, 50, 50, 49, 49, 50, 52, 49, 50, 49, 50, 49, 50, 49,
      50, 50, 49, 50, 61),
    levels = c(49, 50, 52, 58, 61)
  ))


  ## Check output 'realAges' ----

  expect_true(!is.null(simTest$realAges))
  expect_true(class(simTest$realAges) %in% c("integer", "numeric"))
  expect_equal(length(simTest$realAges), 41)
  expect_identical(simTest$realAges, c(
    100, 100, 100, 100, 101, 101, 101, 102, 102, 109, 109, 11, 110, 12, 12,
    128, 129, 13, 13, 130, 14, 79, 81, 81, 82, 88, 89, 89, 9, 90, 90, 91, 91,
    92, 92, 93, 93, 94, 99, 99, 99
  ))


  ## Check output 'delays' ----

  expect_true(!is.null(simTest$delays))
  expect_true(class(simTest$delays) %in% c("integer", "numeric"))
  expect_equal(length(simTest$delays), 41)
  expect_equal(simTest$delays, rep(0, 41))


  ## Check output 'ecozones' ----

  expect_true(!is.null(simTest$ecozones))
  expect_true(class(simTest$ecozones) %in% c("integer", "numeric"))
  expect_equal(length(simTest$ecozones), 41)
  expect_equal(simTest$ecozones, rep(9, 41))


  ## Check output 'spatialUnits' ----

  expect_true(!is.null(simTest$spatialUnits))
  expect_true(class(simTest$spatialUnits) %in% c("integer", "numeric"))
  expect_equal(length(simTest$spatialUnits), 41)
  expect_equal(simTest$spatialUnits, rep(28, 41))


  ## Check output 'speciesPixelGroup' ----

  expect_true(!is.null(simTest$speciesPixelGroup))
  expect_true(inherits(simTest$speciesPixelGroup, "data.table"))

  expect_equal(ncol(simTest$speciesPixelGroup), 2)
  expect_identical(data.table::key(simTest$speciesPixelGroup), NULL)
  for (colName in c("pixelGroup", "species_id")){
    expect_true(colName %in% names(simTest$speciesPixelGroup))
  }

  expect_equal(nrow(simTest$speciesPixelGroup), 41)

  expect_equal(sapply(simTest$speciesPixelGroup, sum), c(
    pixelGroup = 861,
    species_id = 228
  ))


  ## Check output 'mySpuDmids' ----

  expect_true(!is.null(simTest$mySpuDmids))
  expect_true(inherits(simTest$mySpuDmids, "data.table"))

  expect_equal(ncol(simTest$mySpuDmids), 8)
  expect_identical(data.table::key(simTest$mySpuDmids), NULL)

  for (colName in c(
    "distName", "rasterID", "spatial_unit_id", "wholeStand",
    "disturbance_type_id", "disturbance_matrix_id", "name", "description")){
    expect_true(colName %in% names(simTest$mySpuDmids))
  }

  expect_equal(nrow(simTest$mySpuDmids), 5)

  expect_identical(simTest$mySpuDmids$distName, c(
    "wildfire", "clearcut", "deforestation", "20% mortality", "20% mortality"))

  expect_identical(simTest$mySpuDmids$name, c(
    "Wildfire", "Clearcut harvesting without salvage", "Deforestation",
    "Generic 20% mortality", "Generic 20% mortality"))

  expect_identical(simTest$mySpuDmids$description, c(
    "Wildfire for Saskatchewan - Boreal Plains (NIR2011)",
    "Default Stand Replacing Matrix for Clearcut Harvest without Salvage",
    "Deforestation Matrix #1 (Stand Replacing). Traditionally used for all ecozones across Canada.",
    "Generic 20% mortality", "Generic 20% mortality"))

  colsSum <- c("rasterID", "spatial_unit_id", "wholeStand",
               "disturbance_type_id", "disturbance_matrix_id")
  expect_equal(sapply(simTest$mySpuDmids[,..colsSum], sum), c(
    rasterID              = 15,
    spatial_unit_id       = 140,
    wholeStand            = 3,
    disturbance_type_id   = 548,
    disturbance_matrix_id = 739
  ))


  ## Check output 'historicDMtype' ----

  expect_true(!is.null(simTest$historicDMtype))
  expect_true(class(simTest$historicDMtype) %in% c("integer", "numeric"))
  expect_equal(length(simTest$historicDMtype), 41)
  expect_equal(simTest$historicDMtype, rep(1, 41))


  ## Check output 'lastPassDMtype' ----

  expect_true(!is.null(simTest$lastPassDMtype))
  expect_true(class(simTest$lastPassDMtype) %in% c("integer", "numeric"))
  expect_equal(length(simTest$lastPassDMtype), 41)
  expect_equal(simTest$lastPassDMtype, rep(1, 41))


  ## Check output 'disturbanceRasters' -----

  expect_true(!is.null(simTest$disturbanceRasters))
  expect_true(inherits(simTest$disturbanceRasters, "character"))
  expect_equal(length(simTest$disturbanceRasters), 27)
  expect_identical(basename(simTest$disturbanceRasters), paste0("SaskDist_", 1985:2011, ".grd"))

})

