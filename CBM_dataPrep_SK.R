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
    "data.table", "fasterize", "magrittr", "RSQLite", "sf", "terra",
    "reproducible (>=2.1.2)" ,
    "PredictiveEcology/CBMutils@development",
    "PredictiveEcology/LandR@development"
  ),
  parameters = rbind(
    defineParameter(".useCache", "logical", TRUE, NA, NA,
                    "Should caching of events or module be used?")
  ),

  inputObjects = bindrows(
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA,
                 sourceURL = NA), # FROM DEFAULTS
    expectsInput(
      objectName = "spinupSQL", objectClass = "dataset", desc = NA, sourceURL = NA), # FROM DEFAULTS
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
      objectName = "userDist", objectClass = "data.table",
      desc = "User provided file that identifies disturbances for simulation, if not there it will use userDistFile",
      sourceURL = "https://docs.google.com/spreadsheets/d/1fOikb83aOuLlFYIn6pjmC7Jydjcy77TH"),
    expectsInput(
      objectName = "userDistURL", objectClass = "character",
      desc = "URL for userDist"),
    expectsInput(
      objectName = "cbmAdmin", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries,",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids. This is used in the CBM_vol2biomass module"),
      sourceURL = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz"),
    expectsInput(
      objectName = "cbmAdminURL", objectClass = "character",
      desc = "URL for cbmAdmin"),
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
      objectName = "spuRaster", objectClass = "SpatRaster",
      desc = "Raster has spatial units for each pixel",
      sourceURL = "https://drive.google.com/file/d/1D3O0Uj-s_QEgMW7_X-NhVsEZdJ29FBed"),
    expectsInput(
      objectName = "spuRasterURL", objectClass = "character",
      desc = "URL for spuRaster"),
    expectsInput(
      objectName = "ecoRaster", objectClass = "SpatRaster",
      desc = "Raster has ecozones for each pixel",
      sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"),
    expectsInput(
      objectName = "ecoRasterURL", objectClass = "character",
      desc = "URL for ecoRaster"),
    expectsInput(
      objectName = "disturbanceRasters", objectClass = "character",
      desc = "Character vector of the disturbance rasters for use in simulations - defaults are the Wulder and White rasters for SK.",
      sourceURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt"
    ),
    expectsInput(
      objectName = "disturbanceRastersURL", objectClass = "character",
      desc = "URL for disturbanceRasters"
    )
  ),

  outputObjects = bindrows(
    createsOutput(
      objectName = "allPixDT", objectClass = "data.table",
      desc = "Table summarizing raster input data with 1 row for every 'masterRaster' pixel (including NAs)",
      columns = c(
        pixelIndex      = "'masterRaster' cell index",
        ages            = "Stand ages extracted from input 'ageRaster'",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuRaster'",
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
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuRaster'",
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
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuRaster'",
        gcids           = "Growth curve IDs extracted from input 'gcIndexRaster'",
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
      objectName = "gcids", objectClass = "factor",
      desc = paste(
        "Factor of the growth curve IDs for each pixel group.",
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
      objectName = "delays", objectClass = "numeric",
      desc = paste(
        "Regeneration delay post disturbance for each pixel group.",
        "Required input to CBM_core.")),
    createsOutput(
      objectName = "realAges", objectClass = "numeric",
      desc = paste(
        "Stand ages extracted from input 'ageRaster' for each pixel group.",
        "Required input to CBM_core.")),
    createsOutput(
      objectName = "mySpuDmids", objectClass = "data.frame",
      desc = paste(
        "Table summarizing disturbances possible within the spatial units.",
        "Required input to CBM_core."),
      columns = c(
        distName              = "Disturbance name from 'userDist'",
        rasterID              = "Raster ID from 'userDist'",
        wholeStand            = "wholStand flag from 'userDist'",
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
        "Required input to CBM_core.")),
    createsOutput(
      objectName = "disturbanceRasters", objectClass = "character",
      desc = paste(
        "List of disturbance rasters named by the disturbance year.",
        "This is either downloaded from the default URL or a user provided URL.",
        "Required input to CBM_core."))
  )
))

doEvent.CBM_dataPrep_SK <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {

      # Initialize module
      sim <- Init(sim)

    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {

  ## Create sim$allPixDT and sim$spatialDT ----

  # Set which pixel group columns are assigned from input rasters
  pgCols <- c(
    "ages"            = "ageRaster",
    "spatial_unit_id" = "spuRaster",
    "gcids"           = "gcIndexRaster",
    "ecozones"        = "ecoRaster"
  )

  # Check input rasters
  inRast <- list()
  for (rName in c("masterRaster", pgCols)){

    inRast[[rName]] <- sim[[rName]]

    if (is.null(inRast[[rName]])) stop(shQuote(rName), " input raster missing")

    if (!inherits(inRast[[rName]], "SpatRaster")){
      inRast[[rName]] <- tryCatch(
        terra::rast(inRast[[rName]]),
        error = function(e) stop(
          shQuote(rName), " could not be converted to SpatRaster: ", e$message,
          call. = FALSE))
    }

    if (rName %in% pgCols && (
      terra::ncol(inRast[[rName]]) != terra::ncol(inRast$masterRaster) ||
      terra::nrow(inRast[[rName]]) != terra::nrow(inRast$masterRaster) ||
      !all(abs(c(
        terra::res(inRast[[rName]]) - terra::res(inRast$masterRaster),
        terra::ext(inRast[[rName]])$xmin - terra::ext(inRast$masterRaster)$xmin,
        terra::ext(inRast[[rName]])$xmax - terra::ext(inRast$masterRaster)$xmax,
        terra::ext(inRast[[rName]])$ymin - terra::ext(inRast$masterRaster)$ymin,
        terra::ext(inRast[[rName]])$ymax - terra::ext(inRast$masterRaster)$ymax
      )) < 0.01)
    )) stop(shQuote(rName), " does not align with ", shQuote("masterRaster"))
  }

  # Create sim$allPixDT: Summarize input raster values into table
  sim$allPixDT <- data.table::data.table(
    pixelIndex = 1:terra::ncell(inRast$masterRaster)
  )
  for (i in 1:length(pgCols)){
    sim$allPixDT[[names(pgCols)[[i]]]] <- terra::values(inRast[[pgCols[[i]]]])[,1]
  }
  setkeyv(sim$allPixDT, "pixelIndex")

  # Create sim$spatialDT: Summarize input raster values where masterRaster is not NA
  spatialDT <- sim$allPixDT[!is.na(terra::values(sim$masterRaster)[,1]),]

  spatialDT_isNA <- is.na(spatialDT)
  if (any(spatialDT_isNA)){
    for (i in 1:length(pgCols)){
      if (any(spatialDT_isNA[[names(pgCols)[[i]]]])) warning(
        "Pixels have had to be excluded from the simulation where ",
        shQuote(pgCols[[i]]), " contains NAs")
    }
    spatialDT <- spatialDT[!apply(spatialDT_isNA, 1, any),]
  }

  # Create pixel groups: groups of pixels with the same attributes
  spatialDT$pixelGroup <- LandR::generatePixelGroups(
    spatialDT, maxPixelGroup = 0, columns = names(pgCols)
  )

  # Keep only essential columns
  sim$spatialDT <- spatialDT[, c("pixelIndex", "pixelGroup", names(pgCols)), with = FALSE]


  ## Create sim$level3DT, sim$realAges, sim$gcids, and sim$gcids ----

  level3DT <- unique(sim$spatialDT[, -("pixelIndex")])
  setkeyv(level3DT, "pixelGroup")

  # Create sim$curveID
  sim$curveID <- c("gcids") #, "ecozones" # "id_ecozone"
  ##TODO add to metadata -- use in multiple modules

  # Create sim$gcids and sim$level3DT$gcids as a factor
  curveID <- sim$curveID
  sim$gcids <- factor(gcidsCreate(level3DT[, ..curveID]))
  set(level3DT, NULL, "gcids", sim$gcids)

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


  ## Create sim$delays ----

  if(!suppliedElsewhere(sim$delays)){
    sim$delays <- rep.int(0, dim(level3DT)[1])
    ##TODO insert message saying that regen delays are set at 0
  }


  ## Create sim$ecozones and sim$spatialUnits ----

  # create sim$ecozones and sim$spatialUnits to subset vol2biomass growth curves
  sim$ecozones <- sim$level3DT$ecozones
  sim$spatialUnits <- sim$level3DT$spatial_unit_id


  ## Create sim$speciesPixelGroup ----

  speciesPixelGroup <- sim$gcMeta[sim$species_tr, on = .(species = name)]
  speciesPixelGroup <- speciesPixelGroup[gcids >= 1,]
  speciesPixelGroup <- speciesPixelGroup[,.(gcids, species_id)]
  speciesPixelGroup <- speciesPixelGroup[sim$spatialDT, on = .(gcids=gcids)]
  speciesPixelGroup <- unique(speciesPixelGroup[,.(pixelGroup, species_id)])
  sim$speciesPixelGroup <- speciesPixelGroup


  ## Create sim$mySpuDmids ----

  # Matching the disturbances with the Disturbance Matrix IDs in CBM-CFS3 defaults
  # make the disturbance look-up table to the disturbance_matrix_id(s)
  # making sim$mySpuDmids
  userDist <- sim$userDist

  if (!inherits(userDist, "data.table")){
    userDist <- tryCatch(
      data.table::as.data.table(userDist),
      error = function(e) stop(
        "'userDist' could not be converted to data.table: ", e$message, call. = FALSE))
  }

  # Most cases will only require fire (wildfire) and a clearcut. There are 426
  # disturbance matrices identified in the archive of CBM
  # (sim$cbmData@disturbanceMatrix). Matrices are associated with spatial units
  # (sim$cbmData@disturbanceMatrixAssociation). User can select any disturbance
  # they want to represent. Some disturbance matrices are based on data but most
  # are expert opinion in the CBM-CFS3 archive.
  # Disturbance Matrices are specific to spatial_units_ids
  spu <- unique(sim$spatialDT$spatial_unit_id)
  # what disturbances in those spu(s)?
  # spuDist() function is in CBMutils package
  # it lists all the possible disturbances in the CBM-CFS3 archive for that/those
  # spatial unit with the name of the disturbance in the 3rd colum.
  listDist <- CBMutils::spuDist(spu, sim$dbPath)

  ##TODO make this more generalized so user can customize this to their study
  ##area
  ##TODO this is a section that needs to change as we figure out if
  ##disturbance_type_id needs to be used here instead of disturbance_matrix_id

  # make mySpuDmids (distNames,rasterId,spatial_unit_id,disturbance_matrix_id)
##CELINE HERE: trying to make mySpyDmids from userDist
  #if(!suppliedElsewhere(sim$mySpuDmids)){
    ##repeating each user identified disturbance name for each spu, adding the
    ##user defined link between the disturbance type and the provided raster (or
    ##spatial layer), adding the user specified wholeStand flag.
    distName <- c(rep(userDist$distName, length(spu)))
    rasterID <- c(rep(userDist$rasterID, length(spu)))
    wholeStand <- c(rep(userDist$wholeStand, length(spu)))
    spatial_unit_id <- c(sort(rep(spu, length(userDist$distName))))

    mySpuDmids <- data.table(distName, rasterID, spatial_unit_id, wholeStand)

    #dmid <- data.frame(spatial_unit_id = integer(), disturbance_matrix_id = integer())
    dmType <- data.frame(disturbance_type_id = integer(),
                         spatial_unit_id = integer(),
                         disturbance_matrix_id = integer(),
                         name = character(),
                         description = character())

 #   for (i in 1:length(mySpuDmids$distName)) {

      ## TODO: present the user with options that live in listDist for the
      ## specific spu or in sim$cbmData@disturbanceMatrix
      ## Start with code below. For SK, Celine selected:
      ##   disturbance_type_id spatial_unit_id disturbance_matrix_id                                name
      ##1                   1              28                   371                            Wildfire
      ##2                 204              28                   160 Clearcut harvesting without salvage
      ##3                   7              28                    26 Deforestation
      ##4                 168              28                    91 Generic 20% mortality Generic 20% mortality
      ### DANGER HARD CODED FIXES:
      distMatid <- c(371, 160, 26, 91)
      match1 <- listDist[disturbance_matrix_id %in% distMatid,]
      match2 <- match1[c(4,3,1,2,2),]
      sim$mySpuDmids <- cbind(mySpuDmids, match2[,-2])
      #
      # if (mySpuDmids$distName[i] == "clearcut") {
      #   ##there is most likely more than one clearcut
      #   getCut <- listDist[grep("clear", listDist$name, ignore.case = TRUE), ]
      #   ##TODO here is where a message to the user with the name and description
      #   ##columns so they can choose which fits better to there management
      #   ##interventions.
      #
      #   dmType[i, ] <- getCut[4,]
      # } else {
      #   getDist <- listDist[grep(sim$mySpuDmids$distName[i], listDist$name, ignore.case = TRUE), ]
      #   ## Next line is if there are more then one spu
      #   getDist <- getDist[getDist$spatial_unit_id == mySpuDmids$spatial_unit_id[i], ]
      #   dmType[i, ] <- getDist[1, ]
      # }
  #  }


  ## Create sim$historicDMtype and sim$lastPassDMtype ----

  ## TODO: in Canada historic disturbance will always be fire, but the last past may
  ## not, it could be harvest. Make this optional (the user being able to pick
  ## the historical and last pass disturbance). If defaults are picked (fire for
  ## both), write the user a message saying these are the defaults.

  ##to remove...
  # mySpuFires <- sim$mySpuDmids[grep("wildfire", sim$mySpuDmids$distName, ignore.case = TRUE), ]
  # myFires <- mySpuFires[spatial_unit_id %in% unique(sim$level3DT$spatial_unit_id), ]
  # setkey(myFires, spatial_unit_id)
  # setkey(sim$level3DT, spatial_unit_id)
  ###DANGER HARDCODED
  sim$historicDMtype <- rep(sim$mySpuDmids[1,]$disturbance_type_id, dim(sim$level3DT)[1])
  ## TODO: this is where it could be something else then fire
  sim$lastPassDMtype <- sim$historicDMtype


  ## Return simList ----

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  ## Data table inputs ----

  # 1. Growth and yield
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

  # 2. Disturbance information
  if (!suppliedElsewhere("userDist", sim)){

    if (suppliedElsewhere("userDistURL", sim) &
        !identical(sim$userDistURL, extractURL("userDist"))){

      sim$userDist <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$userDistURL,
        fun = data.table::fread
      )

    }else{

      if (!suppliedElsewhere("userDistURL", sim, where = "user")) message(
        "User has not supplied disturbance information ('userDist' or 'userDistURL'). ",
        "Default for Saskatchewan will be used.")

      sim$userDist <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("userDist"),
        targetFile = "userDist.csv",
        fun        = data.table::fread
      )
    }
  }

  # 3. CBM admin
  if (!suppliedElsewhere("cbmAdmin", sim)){

    if (suppliedElsewhere("cbmAdminURL", sim) &
        !identical(sim$cbmAdminURL, extractURL("cbmAdmin"))){

      sim$cbmAdmin <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$cbmAdminURL,
        fun = data.table::fread
      )

    }else{

      if (!suppliedElsewhere("cbmAdminURL", sim, where = "user")) message(
        "User has not supplied CBM admin ('cbmAdmin' or 'cbmAdminURL'). ",
        "Default for Canada will be used.")

      sim$cbmAdmin <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("cbmAdmin"),
        targetFile = "cbmAdmin.csv",
        fun        = data.table::fread
      )
    }
  }


  ## Spatial inputs ----

  # 1. Master raster
  if (!suppliedElsewhere("masterRaster", sim)){

    if (suppliedElsewhere("masterRasterURL", sim) &
        !identical(sim$masterRasterURL, extractURL("masterRaster"))){

      sim$masterRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$masterRasterURL,
        fun = terra::rast
      ) |> Cache()

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

  # 2. Age raster
  if (!suppliedElsewhere("ageRaster", sim)){

    if (suppliedElsewhere("ageRasterURL", sim) &
        !identical(sim$ageRasterURL, extractURL("ageRaster"))){

      sim$ageRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url    = sim$ageRasterURL,
        fun    = terra::rast,
        to     = sim$masterRaster,
        method = "near"
      ) |> Cache()

    }else{

      if (!suppliedElsewhere("ageRasterURL", sim, where = "user")) message(
        "User has not supplied an age raster ('ageRaster' or 'ageRasterURL'). ",
        "Default for Saskatchewan will be used.")

      sim$ageRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("ageRaster"),
        targetFile = "age_TestArea.tif",
        fun        = terra::rast,
        to         = sim$masterRaster,
        method     = "near"
      ) |> Cache()
    }
  }

  # 3. Growth curves
  if (!suppliedElsewhere("gcIndexRaster", sim)){

    if (suppliedElsewhere("gcIndexRasterURL", sim) &
        !identical(sim$gcIndexRasterURL, extractURL("gcIndexRaster"))){

      sim$gcIndexRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url    = sim$gcIndexRasterURL,
        fun    = terra::rast,
        to     = sim$masterRaster,
        method = "near"
      ) |> Cache()

    }else{

      if (!suppliedElsewhere("gcIndexRasterURL", sim, where = "user")) message(
        "User has not supplied a growth curve raster ('gcIndexRaster' or 'gcIndexRasterURL'). ",
        "Default for Saskatchewan will be used.")

      sim$gcIndexRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("gcIndexRaster"),
        targetFile = "gcIndex.tif",
        fun        = terra::rast,
        to         = sim$masterRaster,
        method     = "near"
      ) |> Cache()
    }
  }

  # 4. Spatial units raster
  if (!suppliedElsewhere("spuRaster", sim)){

    if (suppliedElsewhere("spuRasterURL", sim) &
        !identical(sim$spuRasterURL, extractURL("spuRaster"))){

      sim$spuRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url    = sim$spuRasterURL,
        fun    = terra::rast,
        to     = sim$masterRaster,
        method = "near"
      ) |> Cache()

    }else{

      if (!suppliedElsewhere("spuRasterURL", sim, where = "user")) message(
        "User has not supplied a spatial units raster ('spuRaster' or 'spuRasterURL'). ",
        "Default for Canada will be used.")

      spuSF <- prepInputs(
        destinationPath = inputPath(sim),
        url         = extractURL("spuRaster"),
        filename1   = "spUnit_Locator.zip",
        targetFile  = "spUnit_Locator.shp",
        alsoExtract = "similar",
        fun         = sf::st_read(targetFile, quiet = TRUE),
        projectTo   = sim$masterRaster,
        cropTo      = sim$masterRaster
      ) |> Cache()

      sim$spuRaster <- terra::rasterize(
        terra::vect(spuSF),
        sim$masterRaster,
        field = "spu_id"
      ) |> Cache()
    }
  }

  # 5. Ecozones raster
  if (!suppliedElsewhere("ecoRaster", sim)){

    if (suppliedElsewhere("ecoRasterURL", sim) &
        !identical(sim$ecoRasterURL, extractURL("ecoRaster"))){

      sim$ecoRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url    = sim$ecoRasterURL,
        fun    = terra::rast,
        to     = sim$masterRaster,
        method = "near"
      ) |> Cache()

    }else{

      if (!suppliedElsewhere("ecoRasterURL", sim, where = "user")) message(
        "User has not supplied an ecozones raster ('ecoRaster' or 'ecoRasterURL'). ",
        "Default for Canada will be used.")

      ## 2024-12-04 NOTE:
      ## Multiple users had issues downloading and extracting this file via prepInputs.
      ## Downloading the ZIP directly and saving it in the inputs directory works OK.
      ecoSF <- tryCatch(

        prepInputs(
          destinationPath = inputPath(sim),
          url         = extractURL("ecoRaster"),
          filename1   = "ecozone_shp.zip",
          targetFile  = "ecozones.shp",
          alsoExtract = "similar",
          fun         = sf::st_read(targetFile, quiet = TRUE),
          projectTo   = sim$masterRaster,
          cropTo      = sim$masterRaster
        ) |> Cache(),

        error = function(e) stop(
          "Canada ecozones Shapefile failed be downloaded and extracted:\n", e$message, "\n\n",
          "If this error persists, download the ZIP file directly and save it to the inputs directory.",
          "\nDownload URL: ", extractURL("ecoRaster"),
          "\nInputs directory: ", normalizePath(inputPath(sim), winslash = "/"),
          call. = F))

      sim$ecoRaster <- terra::rasterize(
        terra::vect(ecoSF),
        sim$masterRaster,
        field = "ECOZONE"
      ) |> Cache()
    }
  }

  # 6. Disturbance rasters
  if (!suppliedElsewhere("disturbanceRasters", sim)){

    if (suppliedElsewhere("disturbanceRastersURL", sim) &
        !identical(sim$disturbanceRastersURL, extractURL("disturbanceRasters"))){

      drPaths <- preProcess(
        destinationPath = inputPath(sim),
        url = sim$disturbanceRastersURL,
        fun = NA
      )$targetFilePath

      # If extracted archive: list all files in directory
      if (dirname(drPaths) != inputPath(sim)){
        drPaths <- list.files(dirname(drPaths), full = TRUE)
      }

      # List files by year
      drInfo <- data.frame(
        path = drPaths,
        name = tools::file_path_sans_ext(basename(drPaths)),
        ext  = tolower(tools::file_ext(drPaths))
      )
      drInfo$year_regexpr <- regexpr("[0-9]{4}", drInfo$name)
      drInfo$year <- sapply(1:nrow(drInfo), function(i){
        if (drInfo[i,]$year_regexpr != -1){
          paste(
            strsplit(drInfo[i,]$name, "")[[1]][0:3 + drInfo[i,]$year_regexpr],
            collapse = "")
        }else NA
      })

      if (all(is.na(drInfo$year))) stop(
        "Disturbance raster(s) from 'disturbanceRasterURL' must be named with 4-digit years")
      drInfo <- drInfo[!is.na(drInfo$year),, drop = FALSE]

      # Choose file type to return for each year
      drYears <- unique(sort(drInfo$year))
      sim$disturbanceRasters <- sapply(setNames(drYears, drYears), function(drYear){
        drInfoYear <- subset(drInfo, year == drYear)
        if (nrow(drInfoYear) > 1){
          if ("grd" %in% drInfoYear$ext) return(subset(drInfoYear, ext == "grd")$path)
          if ("tif" %in% drInfoYear$ext) return(subset(drInfoYear, ext == "grd")$path)
          drInfoYear$size <- file.size(drInfoYear$path)
          drInfoYear$path[drInfoYear$size == max(drInfoYear$size)][[1]]
        }else drInfoYear$path
      })

    }else{

      if (!suppliedElsewhere("disturbanceRastersURL", sim, where = "user")) message(
        "User has not supplied disturbance rasters ('disturbanceRasters'). ",
        "Default for Saskatchewan will be used.")

      simYears <- start(sim):end(sim)
      if (!all(simYears %in% 1985:2011)) simYears <- 1985:2011
      sim$disturbanceRasters <- sapply(simYears, function(simYear){
        setNames(
          preProcess(
            destinationPath = inputPath(sim),
            url         = if (simYear == simYears[[1]]) extractURL("disturbanceRasters"),
            archive     = if (simYear != simYears[[1]]) file.path(inputPath(sim), "disturbance_testArea.zip"),
            targetFile  = sprintf("disturbance_testArea/SaskDist_%s.grd", simYear),
            alsoExtract = "similar",
            fun         = NA
          )$targetFilePath,
          simYear)
      })
    }
  }


  ## Return simList ----

  return(invisible(sim))

}
