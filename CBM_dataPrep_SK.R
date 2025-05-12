defineModule(sim, list(
  name = "CBM_dataPrep_SK",
  description = "A data preparation module to format and prepare user-provided input to the SpaDES forest-carbon modelling family.",
  keywords = NA,
  authors = c(
    person("Celine", "Boisvenue", email = "Celine.Boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.2", CBM_dataPrep_SK = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("CBM_dataPrep_SK.Rmd"),
  reqdPkgs = list(
    "data.table", "sf", "terra",
    "reproducible (>=2.1.2)" ,
    "PredictiveEcology/CBMutils@development (>=2.0.1)",
    "PredictiveEcology/LandR@development"
  ),
  parameters = rbind(
    defineParameter(".useCache", "character", c(".inputObjects", "Init"), NA, NA, "Cache module events")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "dbPath", objectClass = "character",
      desc = "Path to the CBM defaults databse",
      sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db"), # FROM DEFAULTS
    expectsInput(
      objectName = "disturbanceMatrix", objectClass = "data.frame",
      desc = "Table of disturbances with columns 'spatial_unit_id', 'disturbance_type_id', 'disturbance_matrix_id'",
      sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_exn/disturbance_matrix_association.csv"), # FROM DEFAULTS
    expectsInput(
      objectName = "gcMeta", objectClass = "data.frame",
      sourceURL = "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ",
      desc = "Growth curve metadata"),
    expectsInput(
      objectName = "gcMetaURL", objectClass = "character",
      desc = "URL for gcMeta"),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = "Growth curve volumes by age",
      sourceURL = "https://drive.google.com/file/d/1u7o2BzPZ2Bo7hNcC8nEctNpDmp7ce84m"),
    expectsInput(
      objectName = "userGcM3URL", objectClass = "character",
      desc = "URL for userGcM3"),
    expectsInput(
      objectName = "masterRaster", objectClass = "SpatRaster",
      desc = "Raster has NAs where there are no species and the pixel groupID where the pixels were simulated. It is used to map results",
      sourceURL = "https://drive.google.com/file/d/1zUyFH8k6Ef4c_GiWMInKbwAl6m6gvLJW"),
    expectsInput(
      objectName = "masterRasterURL", objectClass = "character",
      desc = "URL for `masterRaster` - optional, need this or a `masterRaster` object."),
    expectsInput(
      objectName = "ageRaster", objectClass = "SpatRaster",
      desc = "Raster ages for each pixel",
      sourceURL = "https://drive.google.com/file/d/1hylk0D1vO19Dpg4zFtnSNhnyYP4j-bEA"),
    expectsInput(
      objectName = "ageRasterURL", objectClass = "character",
      desc = "URL for ageRaster - optional, need this or a ageRaster"),
    expectsInput(
      objectName = "gcIndexRaster", objectClass = "SpatRaster",
      desc = "Raster giving the growth curve value for each pixel",
      sourceURL = "https://drive.google.com/file/d/1yunkaYCV2LIdqej45C4F9ir5j1An0KKr"),
    expectsInput(
      objectName = "gcIndexRasterURL", objectClass = "character",
      desc = "URL for gcIndexRaster"),
    expectsInput(
      objectName = "spuLocator", objectClass = "sf|SpatRaster",
      desc = paste(
        "Spatial data source from which spatial unit IDs can be extracted.",
        "An output of CBM_defaults.")),
    expectsInput(
      objectName = "ecoLocator", objectClass = "sf|SpatRaster",
      desc = paste(
        "Spatial data source from which ecozone IDs extracted.",
        "An output of CBM_defaults.")),
    expectsInput(
      objectName = "disturbanceRasters", objectClass = "list",
      sourceURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt",
      desc = paste(
        "One or more sets of rasters containing locations of disturbance events for each year.",
        "If the list is named with disturbance event IDs, all non-NA cells will be considered events.",
        "If the list is length 1 and unnamed, the disturbance rasters must have pixel values matching event IDs.",
        "Each set of disturbance rasters must be a list or SpatRaster stack named with 4 digit years",
        "such that a single raster layer can be accessed for each disturbance year",
        "(e.g.  `disturbanceRasters[[\"1\"]][[\"2025\"]]`).",
        "The default rasters are the Wulder and White disturbance rasters for SK covering 1984-2011."
      )),
    expectsInput(
      objectName = "disturbanceRastersURL", objectClass = "character",
      desc = paste(
        "One or more URL for disturbanceRasters.",
        "If the vector is named, it must be named with the disturbance event IDs the raster includes events for.",
        "If the vector is not named, the raster values must be event IDs.")),
    expectsInput(
      objectName = "userDist", objectClass = "data.table",
      sourceURL = "https://drive.google.com/file/d/1n4fXwUkX5GPyWJgr0QQx65roAIaxmcWJ",
      desc = paste(
        "Table defines the values present in the user provided disturbance rasters.",
        "The user will be prompted to match these with CBM-CFS3 disturbances",
        "to create the 'disturbanceMeta' table input to CBM_core.",
        "The default is a table defining the values in the default 'disturbanceRasters'."),
      columns = c(
        eventID    = "Event type ID",
        wholeStand = "Specifies if the whole stand is disturbed (1 = TRUE; 0 = FALSE)",
        name       = "Disturbance name (e.g. 'Wildfire')"
      )),
    expectsInput(
      objectName = "userDistURL", objectClass = "character",
      desc = "URL for userDist")
  ),

  outputObjects = bindrows(
    createsOutput(
      objectName = "allPixDT", objectClass = "data.table",
      desc = "Table summarizing raster input data with 1 row for every 'masterRaster' pixel (including NAs)",
      columns = c(
        pixelIndex      = "'masterRaster' cell index",
        ages            = "Cohort ages extracted from input 'ageRaster'",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'",
        gcids           = "Growth curve IDs extracted from input 'gcIndexRaster'",
        ecozones        = "Ecozone IDs extracted from input 'ecoRaster'"
      )),
    createsOutput(
      objectName = "standDT", objectClass = "data.table",
      desc = paste(
        "Table summarizing raster input data with 1 row for every 'masterRaster' pixel that is not NA",
        "Required input to CBM_core."),
      columns = c(
        pixelIndex      = "'masterRaster' cell index",
        area            = "Stand area in meters",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'"
      )),
    createsOutput(
      objectName = "cohortDT", objectClass = "data.table",
      desc = paste(
        "Table summarizing raster input data with 1 row for every 'masterRaster' pixel that is not NA",
        "Required input to CBM_core."),
      columns = c(
        cohortID        = "Cohort ID",
        pixelIndex      = "'masterRaster' cell index",
        ages            = "Cohort ages extracted from input 'ageRaster'",
        ageSpinup       = "Cohort ages raised to minimum of age 3 to use in the spinup",
        gcids           = "Growth curve IDs extracted from input 'gcIndexRaster'"
      )),
    createsOutput(
      objectName = "spatialDT", objectClass = "data.table",
      desc = "Required by CBM_vol2biomass",
      columns = c(
        pixelIndex      = "'masterRaster' cell index",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'",
        ecozones        = "Ecozone IDs extracted from input 'ecoRaster'",
        gcids           = "Growth curve IDs extracted from input 'gcIndexRaster'"
      )),
    createsOutput(
      objectName = "curveID", objectClass = "character",
      desc = paste(
        "Column names in 'cohortDT' that uniquely define each pixel group growth curve ID.",
        "Required input to CBM_vol2biomass")),
    createsOutput(
      objectName = "gcMeta", objectClass = "data.frame",
      desc = "Growth curve metadata"),
    createsOutput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = "Growth curve volumes by age"),
    createsOutput(
      objectName = "disturbanceEvents", objectClass = "data.table",
      desc = paste(
        "Table with disturbance events for each simulation year.",
        "The inputs 'disturbanceRasters' are aligned with the 'masterRaster'",
        "and the events are summarized into this table.",
        "Required input to CBM_core.")),
    createsOutput(
      objectName = "disturbanceMeta", objectClass = "data.frame",
      desc = paste(
        "Table defining the disturbance event types.",
        "This is created by matching the input 'userDist' table with CBM-CFS3 disturbance types.",
        "Required input to CBM_core."),
      columns = c(
        eventID               = "Event type ID from 'userDist'",
        wholeStand            = "wholeStand flag from 'userDist'",
        spatial_unit_id       = "Spatial unit ID",
        disturbance_type_id   = "Disturbance type ID",
        disturbance_matrix_id = "Disturbance matrix ID",
        name                  = "Disturbance name",
        description           = "Disturbance description"
      ))
  )
))

doEvent.CBM_dataPrep_SK <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,

    init = {

      sim <- Init(sim)

      # Read annual disturbances
      sim <- scheduleEvent(sim, start(sim), "CBM_dataPrep_SK", "readDisturbanceEvents")
    },

    readDisturbanceEvents = {

      if (!is.null(sim$disturbanceRasters)){

        # Align disturbances with masterRaster and summarize in table
        newEvents <-  mapply(
          CBMutils::dataPrep_disturbanceRasters,
          disturbanceRasters = sim$disturbanceRasters,
          eventID  = lapply(1:length(sim$disturbanceRasters), function(i) names(sim$disturbanceRasters)[i]),
          MoreArgs = list(
            templateRast = sim$masterRaster,
            year         = time(sim)
          ),
          SIMPLIFY = FALSE) |> Cache()

        sim$disturbanceEvents <- do.call(rbind, c(
          if (!is.null(sim$disturbanceEvents)) list(sim$disturbanceEvents),
          newEvents
        ))
      }

      # Schedule for next year
      sim <- scheduleEvent(sim, time(sim) + 1, "CBM_dataPrep_SK", "readDisturbanceEvents")
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {

  ## Create sim$standDT and sim$cohortDT ----

  # Set which pixel group columns are assigned from which spatial inputs
  pgCols <- c(
    ages            = "ageRaster",
    gcids           = "gcIndexRaster",
    ecozones        = "ecoLocator",
    spatial_unit_id = "spuLocator"
  )

  # Read spatial inputs
  inRast <- list()
  for (rName in c("masterRaster", pgCols)){
    inRast[[rName]] <- sim[[rName]]
    if (is.null(inRast[[rName]])) stop(shQuote(rName), " input not found")
  }

  ## Convert masterRaster to SpatRaster
  for (rName in "masterRaster"){
    if (!inherits(inRast[[rName]], "SpatRaster")){
      inRast[[rName]] <- tryCatch(
        terra::rast(inRast[[rName]]),
        error = function(e) stop(
          shQuote(rName), " could not be converted to SpatRaster: ", e$message,
          call. = FALSE))
    }
  }

  ## Convert spatial inputs to SpatRaster and align with masterRaster
  for (rName in pgCols){

    if (inherits(inRast[[rName]], "sf")){

      inRast[[rName]] <- terra::rasterize(
        postProcess(
          inRast[[rName]],
          cropTo    = inRast$masterRaster,
          projectTo = inRast$masterRaster
        ) |> Cache(),
        inRast$masterRaster,
        field = names(inRast[[rName]])[[1]]
      )

    }else{

      inRast[[rName]] <- postProcess(
        inRast[[rName]],
        to     = inRast$masterRaster,
        method = "near"
      ) |> Cache()
    }
  }

  # Create sim$allPixDT: Summarize input values into table
  allPixDT <- data.table::data.table(
    pixelIndex = 1:terra::ncell(inRast$masterRaster),
    area       = terra::values(terra::cellSize(inRast$masterRaster, unit = "m", mask = TRUE, transform = FALSE))[,1]
  )
  for (i in 1:length(pgCols)){
    allPixDT[[names(pgCols)[[i]]]] <- terra::values(inRast[[pgCols[[i]]]])[,1]
  }
  data.table::setkey(allPixDT, pixelIndex)

  # Save allPixDT
  sim$allPixDT <- allPixDT

  # Create sim$spatialDT: Summarize input raster values where masterRaster is not NA
  spatialDT <- sim$allPixDT[!is.na(terra::values(inRast$masterRaster)[,1]),]

  # For CBM_vol2biomass
  sim$spatialDT <- spatialDT[, .SD, .SDcols = c("spatial_unit_id", "ecozones", "gcids")]
  sim$spatialDT <- unique(sim$spatialDT[!apply(is.na(sim$spatialDT), 1, any),])

  # For CBM_core
  sim$standDT <- spatialDT[, .SD, .SDcols = c("pixelIndex", "area", "spatial_unit_id")]
  data.table::setkey(sim$standDT, pixelIndex)

  sim$cohortDT <- cbind(cohortID = spatialDT$pixelIndex,
                        spatialDT[, .SD, .SDcols = c("pixelIndex", "gcids", "ages")])
  data.table::setkey(sim$cohortDT, cohortID)

  # Alter ages for the spinup
  ## Temporary fix to CBM_core issue: https://github.com/PredictiveEcology/CBM_core/issues/1
  sim$cohortDT[, ageSpinup := ages]
  sim$cohortDT[ageSpinup < 3, ageSpinup := 3]

  rm(spatialDT)


  ## Create sim$curveID ----=

  # Create sim$curveID
  sim$curveID <- c("gcids") #, "ecozones" # "id_ecozone"
  ##TODO add to metadata -- use in multiple modules


  ## gcMeta: get species attributes ----

  if (any(!c("species_id", "sw_hw", "canfi_species", "genus") %in% names(sim$gcMeta))){

    if (!"species_name" %in% names(sim$gcMeta)) stop(
      "gcMeta requires the 'species_name' column to retrieve species data with CBMutils::sppMatch")

    if (!inherits(sim$gcMeta, "data.table")){
      sim$gcMeta <- tryCatch(
        data.table::as.data.table(sim$gcMeta),
        error = function(e) stop(
          "gcMeta could not be converted to data.table: ", e$message, call. = FALSE))
    }

    sppMatchTable <- CBMutils::sppMatch(
      sim$gcMeta$species_name, return = c("CBM_speciesID", "Broadleaf", "CanfiCode", "NFI"))[, .(
        species_id    = CBM_speciesID,
        sw_hw         = data.table::fifelse(Broadleaf, "hw", "sw"),
        canfi_species = CanfiCode,
        genus         = sapply(strsplit(NFI, "_"), `[[`, 1)
      )]

    sim$gcMeta <- cbind(
      sim$gcMeta[, .SD, .SDcols = setdiff(names(sim$gcMeta), names(sppMatchTable))],
      sppMatchTable)
    rm(sppMatchTable)
  }


  ## Create sim$disturbanceMeta ----

  # List disturbances possible within in each spatial unit
  spuIDs <- sort(unique(sim$standDT$spatial_unit_id))
  listDist <- CBMutils::spuDist(
    spuIDs = spuIDs,
    dbPath = sim$dbPath,
    disturbance_matrix_association = sim$disturbanceMatrix
  )

  # Check if userDist already has all the required IDs
  if (all(c("spatial_unit_id", "disturbance_type_id", "disturbance_matrix_id") %in% names(sim$userDist))){
    sim$disturbanceMeta <- sim$userDist
  }

  if (!suppliedElsewhere("disturbanceMeta", sim)){

    # Read user disturbances
    userDist <- sim$userDist

    if (!inherits(userDist, "data.table")){
      userDist <- tryCatch(
        data.table::as.data.table(userDist),
        error = function(e) stop(
          "'userDist' could not be converted to data.table: ", e$message, call. = FALSE))
    }

    # Match user disturbances with CBM-CFS3 disturbance matrices
    userDistSpu <- userDist
    if (!"spatial_unit_id" %in% names(userDistSpu)){
      userDistSpu <- do.call(rbind, lapply(spuIDs, function(spuID){
        cbind(spatial_unit_id = spuID, userDistSpu)
      }))
    }

    askUser <- interactive() & !identical(Sys.getenv("TESTTHAT"), "true")
    if (askUser) message(
      "Prompting user to match input disturbances with CBM-CFS3 disturbances:")

    sim$disturbanceMeta <- do.call(rbind, lapply(1:nrow(userDistSpu), function(i){

      userDistMatch <- CBMutils::spuDistMatch(
        userDistSpu[i,], listDist = listDist,
        ask = askUser
      ) |> Cache()

      cbind(
        userDistSpu[i, setdiff(names(userDist), names(userDistMatch)), with = FALSE],
        userDistMatch)
    }))
  }


  ## Return simList ----

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  ## Read inputs ----

  # Growth and yield metadata
  if (!suppliedElsewhere("gcMeta", sim)){

    if (suppliedElsewhere("gcMetaURL", sim) &
        !identical(sim$gcMetaURL, extractURL("gcMeta"))){

      sim$gcMeta <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$gcMetaURL,
        fun = data.table::fread
      )

    }else{

      if (!suppliedElsewhere("gcMetaURL", sim, where = "user")) message(
        "User has not supplied growth curve metadata ('gcMeta' or 'gcMetaURL'). ",
        "Default for Saskatchewan will be used.")

      sim$gcMeta <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("gcMeta"),
        targetFile = "gcMetaEg.csv",
        fun        = data.table::fread
      )
      data.table::setkey(sim$gcMeta, gcids)

      # Create column for matching species names with CBMutils::sppMatch
      ## TODO: Consider adding these matches to CBMutils::sppMatch
      sim$gcMeta[, species_name := species]
      sim$gcMeta$species_name[sim$gcMeta$species_name == "White birch"] <- "Paper birch"
    }
  }

  # Growth and yield table
  ## TODO add a data manipulation to adjust if the m3 are not given on a yearly basis.
  if (!suppliedElsewhere("userGcM3", sim)){

    if (suppliedElsewhere("userGcM3URL", sim) &
        !identical(sim$userGcM3URL, extractURL("userGcM3"))){

      sim$userGcM3 <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$userGcM3URL,
        fun = data.table::fread
      )

    }else{

      if (!suppliedElsewhere("userGcM3URL", sim, where = "user")) message(
        "User has not supplied growth curves ('userGcM3' or 'userGcM3URL'). ",
        "Default for Saskatchewan will be used.")

      sim$userGcM3 <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("userGcM3"),
        targetFile = "userGcM3.csv",
        fun        = data.table::fread
      )
      data.table::setnames(sim$userGcM3, names(sim$userGcM3), c("gcids", "Age", "MerchVolume"))
      data.table::setkeyv(sim$userGcM3, c("gcids", "Age"))
    }
  }

  # Master raster
  if (!suppliedElsewhere("masterRaster", sim)){

    if (suppliedElsewhere("masterRasterURL", sim) &
        !identical(sim$masterRasterURL, extractURL("masterRaster"))){

      sim$masterRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$masterRasterURL
      )

    }else{

      if (!suppliedElsewhere("masterRasterURL", sim, where = "user")) message(
        "User has not supplied a master raster ('masterRaster' or 'masterRasterURL'). ",
        "Default for Saskatchewan will be used.")

      masterRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("masterRaster"),
        targetFile = "ldSp_TestArea.tif",
        fun        = terra::rast
      )

      sim$masterRaster <- terra::classify(
        masterRaster, cbind(0, NA)
      ) |> Cache()
    }
  }

  # Stand ages
  if (!suppliedElsewhere("ageRaster", sim)){

    if (suppliedElsewhere("ageRasterURL", sim) &
        !identical(sim$ageRasterURL, extractURL("ageRaster"))){

      sim$ageRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$ageRasterURL
      )

    }else{

      if (!suppliedElsewhere("ageRasterURL", sim, where = "user")) message(
        "User has not supplied an age raster ('ageRaster' or 'ageRasterURL'). ",
        "Default for Saskatchewan will be used.")

      sim$ageRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("ageRaster"),
        targetFile = "age_TestArea.tif",
        fun        = terra::rast
      ) |> Cache()
    }
  }

  # Growth curves
  if (!suppliedElsewhere("gcIndexRaster", sim)){

    if (suppliedElsewhere("gcIndexRasterURL", sim) &
        !identical(sim$gcIndexRasterURL, extractURL("gcIndexRaster"))){

      sim$gcIndexRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$gcIndexRasterURL
      )

    }else{

      if (!suppliedElsewhere("gcIndexRasterURL", sim, where = "user")) message(
        "User has not supplied a growth curve raster ('gcIndexRaster' or 'gcIndexRasterURL'). ",
        "Default for Saskatchewan will be used.")

      sim$gcIndexRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("gcIndexRaster"),
        targetFile = "gcIndex.tif",
        fun        = terra::rast
      ) |> Cache()
    }
  }

  # Disturbances
  if (suppliedElsewhere("userDistURL", sim) & !suppliedElsewhere("userDist", sim) &
      !suppliedElsewhere("disturbanceMeta", sim)){

    sim$userDist <- prepInputs(
      destinationPath = inputPath(sim),
      url = sim$userDistURL,
      fun = data.table::fread
    )
  }

  if (!suppliedElsewhere("disturbanceRasters", sim) & !suppliedElsewhere("disturbanceEvents", sim)){

    if (suppliedElsewhere("disturbanceRastersURL", sim) &
        !identical(sim$disturbanceRastersURL, extractURL("disturbanceRasters"))){

      sim$disturbanceRasters <- lapply(
        sim$disturbanceRastersURL,
        CBMutils::dataPrep_disturbanceRastersURL,
        destinationPath = inputPath(sim)
      )

    }else{

      if (!suppliedElsewhere("disturbanceRastersURL", sim, where = "user")) message(
        "User has not supplied disturbance rasters ('disturbanceRasters' or 'disturbanceRastersURL'). ",
        "Default for Saskatchewan will be used.")

      sim$disturbanceRasters <- list(
        CBMutils::dataPrep_disturbanceRastersURL(
          destinationPath       = inputPath(sim),
          disturbanceRastersURL = extractURL("disturbanceRasters"),
          archive               = "disturbance_testArea.zip",
          targetFile            = "disturbance_testArea",
          alsoExtract           = do.call(c, lapply(1985:2011, function(simYear){
            paste0("disturbance_testArea/SaskDist_", simYear, c(".grd", ".gri", ".tif"))
          })))
      )

      # Disturbance information
      if (!suppliedElsewhere("userDist", sim) & !suppliedElsewhere("userDistURL", sim) &
          !suppliedElsewhere("disturbanceMeta", sim)){

        sim$userDist <- prepInputs(
          destinationPath = inputPath(sim),
          url        = extractURL("userDist"),
          targetFile = "SK_disturbances.csv",
          fun        = data.table::fread
        )
        data.table::setnames(sim$userDist, "rasterID", "eventID")
      }
    }
  }


  ## Return simList ----

  return(invisible(sim))

}
