
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
        modulePath  = testDirs$temp$modules
        #, packagePath = testDirs$temp$libPath
      ),
      require = "testthat",

      dbPath     = .test_defaultInputs("dbPath"),
      spinupSQL  = .test_defaultInputs("spinupSQL"),
      species_tr = .test_defaultInputs("species_tr"),
      gcMeta     = .test_defaultInputs("gcMeta"),


      masterRaster = {
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

  for (colName in c("ages", "spatial_unit_id", "pixelIndex", "gcids", "ecozones", "pixelGroup")){
    expect_true(colName %in% names(simTest$spatialDT))
    expect_true(all(!is.na(simTest$spatialDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$spatialDT), "pixelIndex")


  ## Check output 'level3DT' ----

  expect_true(!is.null(simTest$level3DT))
  expect_true(inherits(simTest$level3DT, "data.table"))

  for (colName in c("spatial_unit_id", "ages", "gcids", "ecozones", "pixelGroup", "return_interval")){
    expect_true(colName %in% names(simTest$level3DT))
    expect_true(all(!is.na(simTest$level3DT[[colName]])))
  }

  expect_identical(data.table::key(simTest$level3DT), "pixelGroup")


  ## Check output 'curveID' ----

  expect_true(!is.null(simTest$curveID))
  expect_true(length(simTest$curveID) >= 1)
  expect_true("gcids" %in% simTest$curveID)


  ## Check output 'gcids' ----

  expect_true(!is.null(simTest$gcids))
  expect_true(inherits(simTest$gcids, "factor"))

  # Check that there is 1 for every pixel group
  expect_equal(length(simTest$gcids), nrow(simTest$level3DT))

  # Check that there are no NAs
  expect_true(all(!is.na(simTest$gcids)))


  ## Check output 'delays' ----

  expect_true(!is.null(simTest$delays))
  expect_true(class(simTest$delays) %in% c("integer", "numeric"))

  # Check that there is 1 for every pixel group
  expect_equal(length(simTest$delays), nrow(simTest$level3DT))

  # By default: no delays
  expect_true(all(simTest$delays == 0))


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


  ## Check output 'speciesPixelGroup' ----

  expect_true(!is.null(simTest$speciesPixelGroup))
  expect_true(inherits(simTest$speciesPixelGroup, "data.table"))

  for (colName in c("pixelGroup", "species_id")){
    expect_true(colName %in% names(simTest$speciesPixelGroup))
    expect_true(all(!is.na(simTest$speciesPixelGroup[[colName]])))
  }

  # Check that there is 1 for every pixel group
  expect_equal(nrow(simTest$speciesPixelGroup), nrow(simTest$level3DT))


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

