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
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("CBM_dataPrep_SK.Rmd"),
  reqdPkgs = list(
    "data.table", "terra","fasterize", "magrittr", "raster", "RSQLite", "sf",
    "PredictiveEcology/CBMutils",
    "PredictiveEcology/LandR"
  ),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(
      ".plotInitialTime", "numeric", NA, NA, NA,
      "This describes the simulation time at which the first plot event should occur"
    ),
    defineParameter(
      ".plotInterval", "numeric", NA, NA, NA,
      "This describes the simulation time interval between plot events"
    ),
    defineParameter(
      ".saveInitialTime", "numeric", NA, NA, NA,
      "This describes the simulation time at which the first save event should occur"
    ),
    defineParameter(
      ".saveInterval", "numeric", NA, NA, NA,
      "This describes the simulation time interval between save events"
    ),
    defineParameter(
      ".useCache", "logical", TRUE, NA, NA,
      paste(
        "Should this entire module be run with caching activated?",
        "This is generally intended for data-type modules,",
        "where stochasticity and time are not relevant"
      )
    )
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "cbmData", objectClass = "dataset",
      desc = "S4 object created from selective reading in of cbm_default.db in CBM_efaults module",
      sourceURL = NA
    ),
    expectsInput(
      objectName = "pooldef", objectClass = "character",
      desc = "Vector of names (characters) for each of the carbon pools, with `Input` being the first one",
      sourceURL = NA
    ),
    expectsInput(
      objectName = "PoolCount", objectClass = "numeric",
      desc = "count of the length of the Vector of names (characters) for each of the carbon pools, with `Input` being the first one",
      sourceURL = NA
    ),
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "sqlDir", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(
      objectName = "userDistFile", objectClass = "character", ## TODO: should be a param
      desc = paste("User provided file name that identifies disturbances for simulation",
                   "(key words for searching CBM files, if not there the userDist will be created with defaults"),
      sourceURL = NA
    ),
    expectsInput(
      objectName = "userDist", objectClass = "data.table",
      desc = "User provided file that identifies disturbances for simulation, if not there it will use userDistFile",
      sourceURL = "https://docs.google.com/spreadsheets/d/1fOikb83aOuLlFYIn6pjmC7Jydjcy77TH"
    ),
    expectsInput(
      objectName = "ageRasterURL", objectClass = "character", ## TODO: url provided below
      desc = "URL for ageRaster - optional, need this or a ageRaster"
    ),
    expectsInput(
      objectName = "ageRaster", objectClass = "raster",
      desc = "Raster ages for each pixel",
      sourceURL = "https://drive.google.com/file/d/1hylk0D1vO19Dpg4zFtnSNhnyYP4j-bEA"
    ),
    expectsInput(
      objectName = "gcIndexRasterURL", objectClass = "character", ## TODO: url provided below
      desc = "URL for ageRaster - optional, need this or a ageRaster"
    ),
    expectsInput(
      objectName = "gcIndexRaster", objectClass = "raster",
      desc = "Raster ages for each pixel",
      sourceURL = "https://drive.google.com/file/d/1yunkaYCV2LIdqej45C4F9ir5j1An0KKr"
    ),
    expectsInput(
      objectName = "spuRaster", objectClass = "raster",
      desc = "Raster has spatial units for each pixel"
    ),
    expectsInput(
      objectName = "ecoRaster", objectClass = "raster",
      desc = "Raster has ecozones for each pixel"
    ),
    expectsInput(
      objectName = "userGcM3File", objectClass = "character",## TODO: should be a param
      desc = paste("User file name for the files containing: GrowthCurveComponentID,Age,MerchVolume.",
                   "Default name userGcM3"),
      sourceURL = NA
    ),
    expectsInput(
      objectName = "userGcM3", objectClass = "dataframe",
      desc = "User file containing: GrowthCurveComponentID,Age,MerchVolume. Default name userGcM3",
      sourceURL = "https://drive.google.com/file/d/1u7o2BzPZ2Bo7hNcC8nEctNpDmp7ce84m"
    ),
    expectsInput(
      objectName = "disturbanceRasters", objectClass = "vector",
      desc = "Character vector of the disturbance rasters for use in simulations - defaults are the Wulder and White rasters for SK.",
      sourceURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt"
      ## user-navigable here: https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt
    ),
    expectsInput(
      objectName = "masterRasterURL", objectClass = "character",
      desc = "URL for `masterRaster` - optional, need this or a `masterRaster` object."
    ),
    expectsInput(
      objectName = "allPixDT", objectClass = "data.table",
      desc = paste("Data table built for all pixels (incluing NAs) for the four essential raster-based information,",
                   "growth curve location (`gcID`), ages, ecozones and spatial unit id (CBM-parameter link)")
    ),
    expectsInput(
      objectName = "masterRaster", objectClass = "raster",
      desc = "Raster has NAs where there are no species and the pixel groupID where the pixels were simulated. It is used to map results",
      sourceURL = "https://drive.google.com/file/d/1zUyFH8k6Ef4c_GiWMInKbwAl6m6gvLJW"
    )
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "pools", objectClass = "matrix", desc = NA), ## TODO
    createsOutput(
      objectName = "curveID", objectClass = "character",
      desc = "Vector of column names that together, uniquely define growth curve id"
    ),
    createsOutput(
      objectName = "ages", objectClass = "numeric",
      desc = "Ages of the stands from the inventory in 1990 with with ages <=1 changes to 3 for the spinup"
    ),
    createsOutput(
      objectName = "realAges", objectClass = "numeric",
      desc = "Ages of the stands from the inventory in 1990 saved to replace the ages post spinup"
    ),
    createsOutput(
      objectName = "nStands", objectClass = "numeric",
      desc = "not really the number of stands, but the number of pixel groups"
    ),
    createsOutput(
      objectName = "gcids", objectClass = "numeric",
      desc = "The identification of which growth curves to use on the specific stands provided by the user."
    ),
    createsOutput(
      objectName = "historicDMIDs", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating historical disturbance type, linked to the S4 table called `cbmData`. Only Spinup."
    ),
    createsOutput(
      objectName = "lastPassDMIDS", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating final disturbance type, linked to the S4 table called `cbmData`. Only Spinup."
    ),
    createsOutput(
      objectName = "delays", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating regeneration delay post disturbance. Only Spinup."
    ),
    createsOutput(
      objectName = "minRotations", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating minimum number of rotations. Only Spinup."
    ),
    createsOutput(
      objectName = "maxRotations", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating maximum number of rotations. Only Spinup."
    ),
    createsOutput(
      objectName = "returnIntervals", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating the fixed fire return interval. Only Spinup."
    ),
    createsOutput(
      objectName = "spatialUnits", objectClass = "numeric",
      desc = "The id given to the intersection of province and ecozones across Canada, linked to the S4 table called cbmData"
    ),
    createsOutput(
      objectName = "ecozones", objectClass = "numeric",
      desc = paste("Vector, one for each stand, indicating the numeric representation",
                   "of the Canadian ecozones, as used in CBM-CFS3")
    ),
    createsOutput(
      objectName = "level3DT", objectClass = "data.table",
      desc = paste("the table linking the spu id, with the disturbance_matrix_id and the events.",
                   "The events are the possible raster values from the disturbance rasters of Wulder and White.")
    ),
    createsOutput(
      objectName = "spatialDT", objectClass = "data.table",
      desc = "the table containing one line per pixel"
    ),
    createsOutput(
      objectName = "mySpuDmids", objectClass = "data.frame",
      desc = "the table containing one line per pixel"
    )
  )
))

doEvent.CBM_dataPrep_SK <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "CBM_dataPrep_SK", "save")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
      "' in module '", current(sim)[1, "moduleName", with = FALSE], "'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  ## Rasters----------------------------------------------------------------------
  ## user provides raster to match (masterRaster) which is a raster for the
  ## study area, it will define the crs etc, for all other layers. The user also
  ## provides age raster, and a raster linking each growth curve to pixels (gcIndex).
  ## Using the masterRaster, the ecozone raster is made (Canadian ecozones) and the
  ## spatial unit raster. The spatial units are a CBM-CFS3 specific location
  ## that is the intersection of the ecozones and administrative boundaries.
  ## These spatial units (or spu) and the ecozones link the CBM-CFS3 ecological
  ## parameters to the right location (example: decomposition rates).
  ##

  io <- inputObjects(sim, currentModule(sim))
  objectNamesExpected <- io$objectName
  available <- objectNamesExpected %in% ls(sim)

  ## TODO: these aren't required
  omit <- which(objectNamesExpected %in% c("userDistFile", "userGcM3File"))
  available <- available[-omit]
  objectNamesExpected <- objectNamesExpected[-omit]

  if (any(!available)) {
    stop(
      "The inputObjects for CBM_core are not all available:",
      "These are missing:", paste(objectNamesExpected[!available], collapse = ", "),
      ". \n\nHave you run ",
      paste0("CBM_", c("defaults"), collapse = ", "),
      "?"
    )
  }

  spatialDT <- sim$allPixDT[!is.na(ages) & !is.na(growth_curve_id),]

  ## Create the pixel groups: groups of pixels with the same attributes ---------------
  setkeyv(spatialDT, "pixelIndex")
  spatialDT$pixelGroup <- Cache(LandR::generatePixelGroups, spatialDT,
                                maxPixelGroup = 0,
                                columns = c("ages", "spatial_unit_id", "growth_curve_component_id", "ecozones")
  )
  setkeyv(spatialDT, "pixelIndex")

  spatialDT <- spatialDT[, .(
    ages, spatial_unit_id, pixelIndex,
    growth_curve_component_id, growth_curve_id, ecozones, pixelGroup
  )]
  setkeyv(spatialDT, "pixelIndex")
  # spatialDT <- spatialDT[order(pixelIndex), ]
  sim$spatialDT <- spatialDT
  # end create pixel groups-------------



  ## Data.table for simulations (one row per pixel group)---------------------
  # this table will be the pixel groups that are used in the spinup procedure in
  # the CBM_core spinup event

  level3DT <- unique(spatialDT[, -("pixelIndex")])
  setkeyv(level3DT, "pixelGroup")
  # in the SK runs, each pixels has a unique growth curve, the ecozone does not
  # change the parameters (they do in other project likes the RIA). So only one
  # column is needed for creating the $gcids as a factor
  sim$curveID <- c("growth_curve_component_id") #, "ecozones" # "id_ecozone"
  ##TODO: add to metadata -- use in multiple modules
  curveID <- sim$curveID

  ##TODO CBMutils::gcidsCreate
  # Error: 'gcidsCreate' is not an exported object from 'namespace:CBMutils'
  # work around until Alex can fix it, putting this in global
  # gcidsCreate <- function(...) {
  #   do.call(paste, c(list(...)[[1]], sep= "_"))
  # }

  sim$gcids <- factor(gcidsCreate(level3DT[, ..curveID]))
  set(level3DT, NULL, "gcids", sim$gcids)
  sim$level3DT <- level3DT

  ## End data.table for simulations-------------------------------------------

  ###HERE
  ## TODO: problem with ages<=1
  ##################################################### # SK example: can't seem
  # in SK: to solve why growth curve id 52 (white birch, good # productivity) will not
  #run with ages= c(0,1,2) it gets stuck in the spinup need to set ages to 3. Tried ages==1, and
  #ages==2. Maybe because the first few years of growth are 0 ? (to check) it
  #does not grow and it does not fill-up the soil pools.
  #work for this problem for most curves for now: this is from SK runs
  #sim$level3DT[ages==0 & growth_curve_component_id==52,ages:=3]
  #sim$level3DT[ages <= 1, ages := 3]
  # in RIA: won't run for ages 0 or 1 with growth 0, had to change it to two
  # realAges are used to restore the correct ages in CBM_core postspinup event
  ######################################
  ## TOOLS TO DEBUG C++ Spinup() fnct
  #level3DT <- level3DT[ages>0,]

  sim$realAges <- sim$level3DT[, ages]
  sim$level3DT[ages <= 3, ages := 3]
  ## TOOLS TO DEBUG C++ Spinup() fnct
  #sim$gcids <- sim$level3DT$gcids

  setorderv(sim$level3DT, "pixelGroup")

  ## Creating all the vectors for the spinup --------------------------------
  sim$ages <- sim$level3DT[, ages]
  sim$nStands <- length(sim$ages)
  sim$pools <- matrix(ncol = sim$PoolCount, nrow = sim$nStands, data = 0)
  colnames(sim$pools) <- sim$pooldef
  sim$pools[, "Input"] <- rep(1.0, nrow(sim$pools))
  sim$delays <- rep.int(0, sim$nStands)
  sim$minRotations <- rep.int(10, sim$nStands)
  sim$maxRotations <- rep.int(30, sim$nStands)
  setkeyv(sim$level3DT, "spatial_unit_id")
  spinupParameters <- as.data.table(sim$cbmData@spinupParameters[, c(1, 2)])
  setkeyv(spinupParameters,"spatial_unit_id")
  retInt <- merge.data.table(sim$level3DT, spinupParameters,
                             by = "spatial_unit_id", all.x = TRUE)
  setkeyv(retInt, "pixelGroup")
  setkeyv(sim$level3DT, "pixelGroup")
  sim$returnIntervals <- retInt[, "return_interval"]
  sim$spatialUnits <- sim$level3DT[, spatial_unit_id]
  sim$ecozones <- sim$level3DT$ecozones

  ################################################################################
  ## matching the disturbances with the Disturbance Matrix IDs in CBM-CFS3 defaults
  ################################################################################
  # Matching disturbances to CBM disturbance matrix id---------------------------------
  # make the disturbance look-up table to the disturbance_matrix_id(s)
  # making sim$mySpuDmids

  userDist <- sim$userDist

  # Most cases will only require fire (wildfire) and a clearcut. There are 426
  # disturbance matrices identified in the archive of CBM
  # (sim$cbmData@disturbanceMatrix). Matrices are associated with spatial units
  # (sim$cbmData@disturbanceMatrixAssociation). User can select any disturbance
  # they want to represent. Some disturbance matrices are based on data but most
  # are expert opinion in the CBM-CFS3 archive.
  # Disturbance Matrices are specific to spatial_units_ids--------------
  spu <- unique(sim$spatialDT$spatial_unit_id)
  # what disturbances in those spu(s)?
  # spuDist() function is in CBMutils package
  # it lists all the possible disturbances in the CBM-CFS3 archive for that/those
  # spatial unit with the name of the disturbance in the 3rd colum.
  listDist <- spuDist(spu, sim$dbPath)

  ## Example specific for SK (as per Boisvenue et al 2016)
  # Disturbances are from White and Wulder and provided as yearly rasters
  # raster values 1 to 5
  # #C:\Celine\GitHub\CBM_\data\forIan\SK_data\disturbance_Sask\ReadMe.txt
  # # Fire =  1
  # # Harvest = 2
  # # Lcondition = 3
  # # Road = 4
  # # Unclass = 5
  # Whatever number of disturbances identified that will be used in the
  # simulation, each disturbance has to have one one disturbance matrix id
  # associated with it.
  # make mySpuDmids (distNames,rasterId,spatial_unit_id,disturbance_matrix_id)
  distName <- c(rep(userDist$distName, length(spu)))
  rasterID <- c(rep(userDist$rasterID, length(spu)))
  wholeStand <- c(rep(userDist$wholeStand, length(spu)))
  spatial_unit_id <- c(sort(rep(spu, length(userDist$distName))))

  mySpuDmids <- data.table(distName, rasterID, spatial_unit_id, wholeStand)

  dmid <- data.frame(spatial_unit_id = integer(), disturbance_matrix_id = integer())

  for (i in 1:length(mySpuDmids$distName)) {
    ### DANGER HARD CODED FIXES
    ## TODO: present the user with options that live in listDist for the
    ## specific spu or in sim$cbmData@disturbanceMatrix
    if (mySpuDmids$distName[i] == "clearcut") {
      dmid[i, ] <- cbind(mySpuDmids$spatial_unit_id[i], 409)
    } else {
      getDist <- listDist[grep(mySpuDmids$distName[i], listDist[, 3], ignore.case = TRUE), 1:2]
      getDist <- getDist[getDist$spatial_unit_id == mySpuDmids$spatial_unit_id[i], ]
      dmid[i, ] <- getDist[1, ]
    }
  }

  ## this creates a bunch of warnings here...
  mySpuDmids <- data.table(mySpuDmids, dmid$disturbance_matrix_id)
  names(mySpuDmids) <- c("distName", "rasterID", "spatial_unit_id", "wholeStand", "disturbance_matrix_id")
  sim$mySpuDmids <- mySpuDmids
  # need to match the historic and last past dist to the spatial unit
  # DECISION: both the last pass and the historic disturbance will be the same
  # for these runs

  ## TODO: in Canada historic DMIDs will always be fire, but the last past may
  ## not, it could be harvest. Make this optional and give the user a message
  ## saying these are the defaults.

  # sim$mySpuDmids <- fread(file.path("data", "mySpuDmids.csv"))

  mySpuFires <- sim$mySpuDmids[grep("wildfire", sim$mySpuDmids$distName, ignore.case = TRUE), ]

  myFires <- mySpuFires[spatial_unit_id %in% unique(sim$level3DT$spatial_unit_id), ]
  setkey(myFires, spatial_unit_id)
  setkey(sim$level3DT, spatial_unit_id)
  # this is mainly to make them the same length at the number of pixel groups
  histLastDMIDs <- merge(sim$level3DT, myFires)
  sim$historicDMIDs <- histLastDMIDs$disturbance_matrix_id
  ## TODO: this is where it could be something else then fire
  sim$lastPassDMIDS <- histLastDMIDs$disturbance_matrix_id

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # if we chose to not use the RSQLite library in this module, and extract
  # disturbance matrix id (dmid) from sim$cbmData@disturbanceMatrixAssociation,
  # then $sqlDir and $dbPath are not needed.
  if (!suppliedElsewhere(sim$sqlDir)) {
    sim$sqlDir <- file.path(dPath, "cbm_defaults")
  }

  if (!suppliedElsewhere(sim$dbPath)) {
    sim$dbPath <- file.path(dPath, "cbm_defaults", "cbm_defaults.db")
  }

  if (!suppliedElsewhere(sim$cbmData)) {
    spatialUnitIds <- as.matrix(getTable("spatialUnitIds.sql", sim$dbPath, sim$sqlDir))
    disturbanceMatrix <- as.matrix(getTable("disturbanceMatrix.sql", sim$dbPath, sim$sqlDir))
    sim$cbmData <- new("dataset",
      turnoverRates = as.matrix(getTable("turnoverRates.sql", sim$dbPath, sim$sqlDir)),
      rootParameters = as.matrix(getTable("rootParameters.sql", sim$dbPath, sim$sqlDir)),
      decayParameters = as.matrix(getTable("decayParameters.sql", sim$dbPath, sim$sqlDir)),
      spinupParameters = as.matrix(getTable("spinupParameters.sql", sim$dbPath, sim$sqlDir)),
      climate = as.matrix(getTable("climate.sql", sim$dbPath, sim$sqlDir)),
      spatialUnitIds = spatialUnitIds,
      slowAGtoBGTransferRate = as.matrix(0.006),
      biomassToCarbonRate = as.matrix(0.5),
      stumpParameters = as.matrix(getTable("stumpParameters.sql", sim$dbPath, sim$sqlDir)),
      overmatureDeclineParameters = as.matrix(getTable("overmaturedecline.sql", sim$dbPath, sim$sqlDir)),
      disturbanceMatrix = disturbanceMatrix,
      disturbanceMatrixAssociation = as.matrix(getTable("disturbanceMatrixAssociation.sql", sim$dbPath, sim$sqlDir)),
      disturbanceMatrixValues = as.matrix(getTable("disturbanceMatrixValues.sql", sim$dbPath, sim$sqlDir)),
      landclasses = as.matrix(getTable("landclasses.sql", sim$dbPath, sim$sqlDir)),
      pools = as.matrix(getTable("pools.sql", sim$dbPath, sim$sqlDir)),
      domPools = as.matrix(getTable("domPools.sql", sim$dbPath, sim$sqlDir))
    )
  }
  if (!suppliedElsewhere(sim$pooldef)) {
    sim$pooldef <- CBMutils::.pooldef
    sim$PoolCount <- length(sim$pooldef)
  }

  # user provided data tables (3)------------------------------------------------------

  # 1. growth and yield information
  # userGcM3 and userGcM3File, these files are the m3/ha and age info by growth
  # curve ID, columns should be GrowthCurveComponentID	Age	MerchVolume
  ## TODO add a data manipulation to adjust if the m3 are not given on a yearly basis
  if (!suppliedElsewhere("userGcM3", sim)) {
    if (!suppliedElsewhere("userGcM3File", sim)) {
      sim$userGcM3 <- prepInputs(url = extractURL("userGcM3"),
                                 fun = "data.table::fread",
                                 destinationPath = dPath,
                                 #purge = 7,
                                 filename2 = "userGcM3.csv")

      message(
        "User has not supplied growth curves (m3 by age or the file name for the growth curves). ",
        "The default will be used which is for a region in Saskatchewan."
      )
    } else {
      sim$userGcM3 <- fread(sim$userGcM3File)
    }
    names(sim$userGcM3) <- c("GrowthCurveComponentID", "Age", "MerchVolume")
  }

  # 2. Disturbance information - see disturbance raster below
  # this may be provided by the user, by the defaults or by other modules/family
  # of modules. It is the link between the spatial location of the disturbance
  # (like a raster value) and the disturbance name.
  if (!suppliedElsewhere(sim$userDist)) {
    if (!suppliedElsewhere(sim$userDistFile)) {
      message("There is no disturbance information provided; defaults for the Saskatchewan example run will be used.")
      # DO THIS: make a default of the basic ones
      # distName <- c("fire", "clearcut")
      # rasterId <- c(1,2)
      # sim$userDist <- data.table(distName,rasterId)
      # warning("Default disturbances will be used. They are fire and clearcut, assigned raster values of 1 and 2 respectively.")

      ## TODO: workaround failing prepInputs call (reproducible/issues/287)
      # sim$userDist <- prepInputs(url = extractURL("userDist"),
      #                            destinationPath = dPath,
      #                            fun = "data.table::fread")
      if (!file.exists(file.path(dPath, "userDist.csv"))) {
        drive_download(as_id(extractURL("userDist")),
                       path = file.path(dPath, "userDist.csv"),
                       type = "spreadsheet")
      }
      sim$userDist <- fread(file.path(dPath, "userDist.csv"))
    } else {
      sim$userDist <- fread(sim$userDistFile)
    }
  }

  # 3. cbmAdmin needed to create spuRaster below (a bit convoluted ## TODO )
  # for the SK simulations, this info is provided directly as a raster (raster
  # #4 below)
  # if (!suppliedElsewhere("cbmAdmin", sim)) {
  #   sim$cbmAdmin <- fread(file.path(dPath, "cbmAdmin.csv")) ## TODO: use prepInputs with url
  # }

  # END user provided data tables (3)------------------------------------------------------

  # user provided rasters or spatial information------------------------

  ## Rasters
  ## user provides raster to match (masterRaster) which is a raster for the
  ## study area, it will define the crs etc, for all other layers. The user also
  ## provides age raster, and a raster linking each growth curve to pixels (gcIndex).
  ## Using the masterRaster, the ecozone raster is made (Canadian ecozones) and the
  ## spatial unit raster. The spatial units are a CBM-CFS3 specific location
  ## that is the intersection of the ecozones and administrative boundaries.
  ## These spatial units (or spu) and the ecozones link the CBM-CFS3 ecological
  ## parameters to the right location (example: decomposition rates).
  ##

  ## Jan 2023 addition from Eliot from Zulip R-help
  options("reproducible.useTerra" = TRUE)
  # 1. Raster to match (masterRaster). This is the study area.
  if (!suppliedElsewhere("masterRasterURL", sim)) {
    sim$masterRasterURL <- extractURL("masterRaster")
    if (!suppliedElsewhere("masterRaster", sim)) {
      ## TO DO: why is this
      message(
        "User has not supplied a masterRaster or a URL for a masterRaster (masterRasterURL object).\n",
        "masterRaster is going to be read from the default URL given in the inputObjects for ",
        currentModule(sim)
      )
      sim$masterRaster <- Cache(
        prepInputs,
        url = sim$masterRasterURL,
        fun = "terra::rast",
        destinationPath = dPath
      )

    }
    sim$masterRaster[sim$masterRaster == 0] <- NA
  }

  # 2. Age raster from inventory
  if (!suppliedElsewhere(sim$ageRaster)) {
    if (!suppliedElsewhere(sim$ageRasterURL)) {
      sim$ageRasterURL <- extractURL("ageRaster")
    }
    sim$ageRaster <- Cache(prepInputs,
                           url = sim$ageRasterURL,
                           fun = "terra::rast",
                           to = sim$masterRaster,
                           method = "near", # need integers
                           destinationPath = dPath
    )
    ## TODO: put in a message to out pointing out the max age (this has to be
    ## sinked to the max age on the growth curve max age for the spinup)
    # maxAge <- max(sim$ageRaster)
    # message(max age on the raster is XX)
  }

  # 3. What growth curve should be applied to what pixels?
  if (!suppliedElsewhere(sim$gcIndexRaster)) {
    if (!suppliedElsewhere(sim$gcIndexRasterURL)) {
      sim$gcIndexRasterURL <- extractURL("gcIndexRaster")
    }
    sim$gcIndexRaster <- Cache(prepInputs,
                               url = sim$gcIndexRasterURL,
                               fun = "terra::rast",
                               to = sim$masterRaster,
                               method = "near", # need integers
                               destinationPath = dPath)
  }

  # 4. Spatial Unit raster. This takes the masterRaster (study area) and figures
  # out what CBM-specific spatial units each pixels. This determines some
  # defaults CBM-parameters across Canada.
  if (!suppliedElsewhere(sim$spuRaster)) {

    canadaSpu <- Cache(prepInputs,
                            targetFile = "spUnit_Locator.shp",
                            url = "https://drive.google.com/file/d/1D3O0Uj-s_QEgMW7_X-NhVsEZdJ29FBed",
                            destinationPath = dPath,
                            alsoExtract = "similar")

    spuShp <- Cache(postProcess,
                    canadaSpu,
                    to = sim$masterRaster,
                    method = "near", # need integers
                    #targetCRS = terra::crs(sim$masterRaster),
                    useCache = FALSE, filename2 = NULL
    ) |> st_collection_extract("POLYGON")

    sim$spuRaster <- terra::rasterize(terra::vect(spuShp),
                                      terra::rast(sim$masterRaster),
                                      field = "spu_id") |> raster::raster()
  }

  # 5. Ecozone raster. This takes the masterRaster (study area) and figures
  # out what ecozones each pixels are in. This determines some
  # defaults CBM-parameters across Canada.

  if (!suppliedElsewhere(sim$ecoRaster)) {
    ecozones <- Cache(prepInputs,
                      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                      alsoExtract = "similar",
                      destinationPath = dPath,
                      filename1 = "ecozone_shp.zip",
                      # overwrite = TRUE, ## not needed if filename1 specified
                      fun = "sf::st_read", #"terra::vect",
                      rasterToMatch = sim$masterRaster
    ) ## ecozones is a SpatVect class object
    ## TODO: terra::vect fails on some windows machines. Windows does not
    ## recognize some of the french characters.
    # ecozones <- terra::vect(ecozones)

    sim$ecoRaster <- terra::rasterize(ecozones, sim$masterRaster, field = "ECOZONE")
  }

  ## TODO: hard stop here if there are NA values and have user fix them to prevent issues downstream
  if (any(is.na(values(sim$ecoRaster)))) {
    stop("ecoRaster cannot contain NA values. Please fix these and rerun.")
  }

  dtRasters <- as.data.table(cbind(growth_curve_component_id = sim$gcIndexRaster[],
                                   ages = sim$ageRaster[],
                                   ecozones = sim$ecoRaster[],
                                   spatialUnitID = sim$spuRaster[]))

  # assertion -- if there are both NAs or both have data, then the columns with be the same, so sum is either 0 or 2
  if (isTRUE(P(sim)$doAssertions)) {
    bbb <- apply(dtRasters, 1, function(x) sum(is.na(x)))
    if (!all(names(table(bbb)) %in% c("0", "4")))
      stop("should be only 0 or 4s")
  }

  ## There seems to be a caching problem here, the name adjustments

  ##
  sim$allPixDT <- as.data.table(cbind(dtRasters,
                                      pixelIndex = 1:ncell(sim$gcIndexRaster),
                                      growth_curve_id = sim$gcIndexRaster[]))

  chgNamesTo <- c("growth_curve_component_id", "ages", "ecozones", "spatial_unit_id",
                  "pixelIndex", "growth_curve_id")
  setnames(sim$allPixDT,names(sim$allPixDT),chgNamesTo)

  # 6. Disturbance rasters. The default example is a list of rasters, one for each year.
  #    But these can be provided by another family of modules in the annual event.

  ## TODO: add a message if no information is provided asking the user if
  ## disturbances will be provided on a yearly basis.
  if (!suppliedElsewhere("disturbanceRasters", sim)) {
    ## download the data and identify the .grd files present
    out <- preProcess(url = extractURL("disturbanceRasters"),
                      destinationPath = file.path(dPath),
                      filename1 = "disturbance_testArea.zip")

    sim$disturbanceRasters <- list.files(
      file.path(dPath, "disturbance_testArea"),
      pattern = "[.]grd$",
      full.names = TRUE
    )
  }

  stopifnot(length(sim$disturbanceRasters) > 0)

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}
