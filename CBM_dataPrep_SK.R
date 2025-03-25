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
    defineParameter(".useCache", "logical", TRUE, NA, NA,
                    "Should caching of events or module be used?")
  ),

  inputObjects = bindrows(
    expectsInput(
      objectName = "dbPath", objectClass = "character",
      desc = "Path to the CBM defaults databse",
      sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db"), # FROM DEFAULTS
    expectsInput(
      objectName = "dMatrixAssociation", objectClass = "data.frame",
      desc = "Disturbance table matching different disturbance IDs",
      sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_exn/disturbance_matrix_association.csv"), # FROM DEFAULTS
    expectsInput(
      objectName = "spinupSQL", objectClass = "dataset",
      desc = "Table containing many necesary spinup parameters", sourceURL = NA), # FROM DEFAULTS
    expectsInput(
      objectName = "species_tr", objectClass = "dataset", desc = NA, sourceURL = NA), # FROM DEFAULTS
    expectsInput(
      objectName = "gcMeta", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids"),
      sourceURL =
        "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ/view?usp=sharing"), # FROM VOL2BIOMASS
    expectsInput(
      objectName = "gcMetaURL", objectClass = "character",
      desc = "URL for gcMeta"),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = paste("User file containing:",
                   "`gcids`, `Age`, `MerchVolume`.",
                   "Default name `userGcM3`."),
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
      desc = "URL for gcIndexRaste - optional, need this or a ageRaster"),
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
        ages            = "Stand ages extracted from input 'ageRaster'",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'",
        gcids           = "Growth curve IDs extracted from input 'gcIndexRaster'",
        ecozones        = "Ecozone IDs extracted from input 'ecoRaster'"
      )),
    createsOutput(
      objectName = "spatialDT", objectClass = "data.table",
      desc = paste(
        "Table summarizing raster input data with 1 row for every 'masterRaster' pixel that is not NA",
        "Required input to CBM_vol2biomass and CBM_core."),
      columns = c(
        pixelIndex      = "'masterRaster' cell index",
        pixelGroup      = "Pixel group ID",
        ages            = "Stand ages extracted from input 'ageRaster'",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'",
        gcids           = "Growth curve IDs extracted from input 'gcIndexRaster'",
        ecozones        = "Ecozone IDs extracted from input 'ecoRaster'"
      )),
    createsOutput(
      objectName = "level3DT", objectClass = "data.table",
      desc = paste(
        "Table associating pixel groups with their key attributes.",
        "Required input to CBM_vol2biomass and CBM_core."),
      columns = c(
        pixelGroup      = "Pixel group ID",
        ages            = "Stand ages extracted from input 'ageRaster' modified such that all ages are >=3",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'",
        gcids           = "Factor of growth curve IDs extracted from input 'gcIndexRaster'",
        ecozones        = "Ecozone IDs extracted from input 'ecoRaster'"
      )),
    createsOutput(
      objectName = "speciesPixelGroup", objectClass = "data.frame",
      desc = paste(
        "Table connecting pixel groups to species IDs.",
        "Required input to CBM_core."),
      columns = c(
        pixelGroup = "Pixel group ID",
        species_id = "Species ID"
      )),
    createsOutput(
      objectName = "curveID", objectClass = "character",
      desc = paste(
        "Column names in 'level3DT' that uniquely define each pixel group growth curve ID.",
        "Required input to CBM_vol2biomass")),
    createsOutput(
      objectName = "ecozones", objectClass = "numeric",
      desc = paste(
        "Ecozone IDs extracted from input 'ecoRaster' for each pixel group.",
        "Required input to CBM_vol2biomass")),
    createsOutput(
      objectName = "spatialUnits", objectClass = "numeric",
      desc = paste(
        "Spatial unit IDs extracted from input 'spuRaster' for each pixel group.",
        "Required input to CBM_vol2biomass")),
    createsOutput(
      objectName = "realAges", objectClass = "numeric",
      desc = paste(
        "Stand ages extracted from input 'ageRaster' for each pixel group.",
        "Required input to CBM_core.")),
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
      )),
    createsOutput(
      objectName = "historicDMtype", objectClass = "numeric",
      desc = paste(
        "Historical disturbance type for each pixel group.",
        "Examples: 1 = wildfire; 2 = clearcut.",
        "Required input to CBM_core.")),
    createsOutput(
      objectName = "lastPassDMtype", objectClass = "numeric",
      desc = paste(
        "Last pass disturbance type for each pixel group.",
        "Examples: 1 = wildfire; 2 = clearcut.",
        "Required input to CBM_core."))
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

  ## Create sim$allPixDT and sim$spatialDT ----

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
  sim$allPixDT <- data.table::data.table(
    pixelIndex = 1:terra::ncell(inRast$masterRaster)
  )
  for (i in 1:length(pgCols)){
    sim$allPixDT[[names(pgCols)[[i]]]] <- terra::values(inRast[[pgCols[[i]]]])[,1]
  }
  setkeyv(sim$allPixDT, "pixelIndex")

  # Create sim$spatialDT: Summarize input raster values where masterRaster is not NA
  spatialDT <- sim$allPixDT[!is.na(terra::values(inRast$masterRaster)[,1]),]

  spatialDT_isNA <- is.na(spatialDT)
  if (any(spatialDT_isNA)){
    for (i in 1:length(pgCols)){
      if (any(spatialDT_isNA[, names(pgCols)[[i]]])) warning(
        "Pixels have been excluded from the simulation where there are no values in ",
        shQuote(pgCols[[i]]))
    }
    spatialDT <- spatialDT[!apply(spatialDT_isNA, 1, any),]
  }

  # Create pixel groups: groups of pixels with the same attributes
  spatialDT$pixelGroup <- LandR::generatePixelGroups(
    spatialDT, maxPixelGroup = 0, columns = names(pgCols)
  )

  # Keep only essential columns
  sim$spatialDT <- spatialDT[, c("pixelIndex", "pixelGroup", names(pgCols)), with = FALSE]


  ## Create sim$level3DT, sim$realAges, and sim$curveID ----

  level3DT <- unique(sim$spatialDT[, -("pixelIndex")])
  setkeyv(level3DT, "pixelGroup")

  # Create sim$curveID
  sim$curveID <- c("gcids") #, "ecozones" # "id_ecozone"
  ##TODO add to metadata -- use in multiple modules

  # Set sim$level3DT$gcids to be a factor
  set(level3DT, j = "gcids",
      value = factor(CBMutils::gcidsCreate(level3DT[, sim$curveID, with = FALSE])))

  # Create 'realAges' output object and set ages to be >= 3
  ## Temporary fix to CBM_core issue: https://github.com/PredictiveEcology/CBM_core/issues/1
  sim$realAges <- level3DT[, ages]
  level3DT[ages <= 3, ages := 3]
  setorderv(level3DT, "pixelGroup")

  # Join with spinup parameters
  setkeyv(level3DT, "spatial_unit_id")
  spinupParameters <- as.data.table(sim$spinupSQL[, c(1, 7)])

  setkeyv(spinupParameters,"id")
  spinupParameters <- setNames(spinupParameters, replace(names(spinupParameters), names(spinupParameters) == 'id', 'spatial_unit_id'))
  retInt <- merge.data.table(level3DT, spinupParameters,
                             by = "spatial_unit_id", all.x = TRUE)
  setkeyv(retInt, "pixelGroup")
  setkeyv(level3DT, "pixelGroup")
  sim$level3DT <- retInt


  ## Create sim$ecozones and sim$spatialUnits ----

  # create sim$ecozones and sim$spatialUnits to subset vol2biomass growth curves
  sim$ecozones <- sim$level3DT$ecozones
  sim$spatialUnits <- sim$level3DT$spatial_unit_id


  ## Create sim$speciesPixelGroup ----

  gcMeta <- sim$gcMeta
  if (!inherits(gcMeta, "data.table")){
    gcMeta <- tryCatch(
      data.table::as.data.table(gcMeta),
      error = function(e) stop(
        "'gcMeta' could not be converted to data.table: ", e$message, call. = FALSE))
  }

  speciesPixelGroup <- gcMeta[sim$species_tr, on = .(species = name)]
  speciesPixelGroup <- speciesPixelGroup[gcids >= 1,]
  speciesPixelGroup <- speciesPixelGroup[,.(gcids, species_id)]
  speciesPixelGroup <- speciesPixelGroup[sim$spatialDT, on = .(gcids=gcids)]
  speciesPixelGroup <- unique(speciesPixelGroup[,.(pixelGroup, species_id)])
  sim$speciesPixelGroup <- speciesPixelGroup


  ## Create sim$disturbanceMeta, sim$historicDMtype, and sim$lastPassDMtype ----

  # List disturbances possible within in each spatial unit
  spuIDs <- sort(unique(sim$level3DT$spatial_unit_id))
  listDist <- CBMutils::spuDist(
    spuIDs = spuIDs,
    dbPath = sim$dbPath,
    disturbance_matrix_association = sim$dMatrixAssociation
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

  # Set sim$historicDMtype to be wildfire
  sim$historicDMtype <- data.table::merge.data.table(
    sim$level3DT,
    unique(subset(listDist[, .(spatial_unit_id, disturbance_type_id, name)], tolower(name) == "wildfire")),
    by = "spatial_unit_id"
  )$disturbance_type_id

  # Set sim$lastPassDMtype to be wildfire
  ## TODO: this is where it could be something else then fire
  sim$lastPassDMtype <- sim$historicDMtype


  ## Return simList ----

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  ## Read inputs ----

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
      names(sim$userGcM3) <- c("gcids", "Age", "MerchVolume")
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
        names(sim$userDist)[names(sim$userDist) == "rasterID"] <- "eventID"
      }
    }
  }


  ## Return simList ----

  return(invisible(sim))

}
