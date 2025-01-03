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
      objectName = "allPixDT", objectClass = "data.table",
      desc = paste("Data table built for all pixels (incluing NAs) for the four essential raster-based information,",
                   "growth curve location (`gcID`), ages, ecozones and spatial unit id (CBM-parameter link)")),
    expectsInput(
    objectName = "disturbanceRasters", objectClass = "character",
    desc = "Character vector of the disturbance rasters for use in simulations - defaults are the Wulder and White rasters for SK.",
    sourceURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt"
  )
  ),

  outputObjects = bindrows(
    createsOutput(
      objectName = "spatialDT", objectClass = "data.table",
      desc = "the table containing one line per pixel"),
    createsOutput(
      objectName = "level3DT", objectClass = "data.table",
      desc = paste("the table linking the spu id, with the disturbance_matrix_id and the events.",
                   "The events are the possible raster values from the disturbance rasters of Wulder and White.")),
    createsOutput(
      objectName = "curveID", objectClass = "character",
      desc = "Vector of column names that together, uniquely define growth curve id"),
    createsOutput(
      objectName = "gcids", objectClass = "numeric",
      desc = "The identification of which growth curves to use on the specific stands provided by the user."),
    createsOutput(
      objectName = "realAges", objectClass = "numeric",
      desc = "Ages of the stands from the inventory in 1990 saved to replace the ages post spinup"),
    createsOutput(
      objectName = "delays", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating regeneration delay post disturbance. Only Spinup."),
    createsOutput(
      objectName = "ecozones", objectClass = "numeric",
      desc = paste("Vector, one for each stand, indicating the numeric representation",
                   "of the Canadian ecozones, as used in CBM-CFS3")),
    createsOutput(
      objectName = "spatialUnits", objectClass = "numeric",
      desc = "The id given to the intersection of province and ecozones across Canada, linked to the S4 table called cbmData"), # TODO: description out of date
    createsOutput(
      objectName = "speciesPixelGroup", objectClass = "data.frame",
      desc = "This table connects species codes to PixelGroups"),
    createsOutput(
      objectName = "mySpuDmids", objectClass = "data.frame",
      desc = "the table containing one line per pixel"),
    createsOutput(
      objectName = "historicDMtype", objectClass = "numeric",
      desc = "Vector, one for each stand/pixelGroup, indicating historical disturbance type (1 = wildfire). Only used in the spinup event."),
    createsOutput(
      objectName = "lastPassDMtype", objectClass = "numeric",
      desc = "Vector, one for each stand/pixelGroup, indicating historical disturbance type (1 = wildfire). Only used in the spinup event.")
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

  spatialDT <- sim$allPixDT[!is.na(ages) & !is.na(gcids),]


  ## Create the pixel groups: groups of pixels with the same attributes ---------------
  setkeyv(spatialDT, "pixelIndex")
  if(!"pixelGroup" %in% names(spatialDT)){
     spatialDT$pixelGroup <- LandR::generatePixelGroups(spatialDT,
                                maxPixelGroup = 0,
                                columns = c("ages", "spatial_unit_id", "gcids", "ecozones")
    )
  }
  setkeyv(spatialDT, "pixelIndex")

  spatialDT <- spatialDT[, .(
    ages, spatial_unit_id, pixelIndex,
    gcids, ecozones, pixelGroup
  )]
  setkeyv(spatialDT, "pixelIndex")
  sim$spatialDT <- spatialDT
  # end create pixel groups-------------



  ## Data.table for simulations (one row per pixel group)---------------------
  # this table will be the pixel groups that are used in the spinup procedure in
  # the CBM_core spinup event

  level3DT <- unique(spatialDT[, -("pixelIndex")])
  setkeyv(level3DT, "pixelGroup")
  sim$curveID <- c("gcids") #, "ecozones" # "id_ecozone"
  ##TODO add to metadata -- use in multiple modules
  curveID <- sim$curveID

  sim$gcids <- factor(gcidsCreate(level3DT[, ..curveID]))
  set(level3DT, NULL, "gcids", sim$gcids)

  ## End data.table for simulations-------------------------------------------

  # Create 'realAges' output object and set ages to be >= 3
  ## Temporary fix to CBM_core issue: https://github.com/PredictiveEcology/CBM_core/issues/1
  sim$realAges <- level3DT[, ages]
  level3DT[ages <= 3, ages := 3]

  setorderv(level3DT, "pixelGroup")

  ## Creating all the vectors for the spinup --------------------------------
  ##TODO Do we need all these vectors for the spinup?? Check CBM_core.
   if(!suppliedElsewhere(sim$delays)){
     sim$delays <- rep.int(0, dim(level3DT)[1])
     ##TODO insert message saying that regen delays are set at 0
  }

  setkeyv(level3DT, "spatial_unit_id")
  spinupParameters <- as.data.table(sim$spinupSQL[, c(1, 7)])

  setkeyv(spinupParameters,"id")
  spinupParameters <- setNames(spinupParameters, replace(names(spinupParameters), names(spinupParameters) == 'id', 'spatial_unit_id'))
  retInt <- merge.data.table(level3DT, spinupParameters,
                             by = "spatial_unit_id", all.x = TRUE)
  setkeyv(retInt, "pixelGroup")
  setkeyv(level3DT, "pixelGroup")
  sim$level3DT <- retInt

  # create sim$ecozones and sim$spatialUnits to subset vol2biomass growth curves
  sim$ecozones <- sim$level3DT$ecozones
  sim$spatialUnits <- sim$level3DT$spatial_unit_id

  # create sim$speciesPixelGroup
  speciesPixelGroup <- sim$gcMeta[sim$species_tr, on = .(species = name)]
  speciesPixelGroup <- speciesPixelGroup[gcids >= 1,]
  speciesPixelGroup <- speciesPixelGroup[,.(gcids, species_id)]
  speciesPixelGroup <- speciesPixelGroup[sim$spatialDT, on = .(gcids=gcids)]
  speciesPixelGroup <- unique(speciesPixelGroup[,.(pixelGroup, species_id)])
  sim$speciesPixelGroup <- speciesPixelGroup

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


 # }

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

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  ##OLD - delete once everything works for the SK managed forests.
  # # if we chose to not use the RSQLite library in this module, and extract
  # # disturbance matrix id (dmid) from sim$cbmData@disturbanceMatrixAssociation,
  # # then $sqlDir and $dbPath are not needed.
  # if (!suppliedElsewhere(sim$sqlDir)) {
  #   sim$sqlDir <- file.path(dPath, "cbm_defaults") ##TODO: this needs to be updated with the new version of cbm_defaults
  # }
  #
  # if (!suppliedElsewhere(sim$dbPath)) {
  #   sim$dbPath <- file.path(dPath, "cbm_defaults", "cbm_defaults.db") ##TODO: this needs to be updated with the new version of cbm_defaults
  # }
  #
  # # if (!suppliedElsewhere(sim$cbmData)) {
  # #   spatialUnitIds <- as.matrix(getTable("spatialUnitIds.sql", sim$dbPath, sim$sqlDir))
  # #   disturbanceMatrix <- as.matrix(getTable("disturbanceMatrix.sql", sim$dbPath, sim$sqlDir))
  # #   sim$cbmData <- new("dataset",
  # #     turnoverRates = as.matrix(getTable("turnoverRates.sql", sim$dbPath, sim$sqlDir)),
  # #     rootParameters = as.matrix(getTable("rootParameters.sql", sim$dbPath, sim$sqlDir)),
  # #     decayParameters = as.matrix(getTable("decayParameters.sql", sim$dbPath, sim$sqlDir)),
  # #     spinupParameters = as.matrix(getTable("spinupParameters.sql", sim$dbPath, sim$sqlDir)),
  # #     climate = as.matrix(getTable("climate.sql", sim$dbPath, sim$sqlDir)),
  # #     spatialUnitIds = spatialUnitIds,
  # #     slowAGtoBGTransferRate = as.matrix(0.006),
  # #     biomassToCarbonRate = as.matrix(0.5),
  # #     stumpParameters = as.matrix(getTable("stumpParameters.sql", sim$dbPath, sim$sqlDir)),
  # #     overmatureDeclineParameters = as.matrix(getTable("overmaturedecline.sql", sim$dbPath, sim$sqlDir)),
  # #     disturbanceMatrix = disturbanceMatrix,
  # #     disturbanceMatrixAssociation = as.matrix(getTable("disturbanceMatrixAssociation.sql", sim$dbPath, sim$sqlDir)),
  # #     disturbanceMatrixValues = as.matrix(getTable("disturbanceMatrixValues.sql", sim$dbPath, sim$sqlDir)),
  # #     landclasses = as.matrix(getTable("landclasses.sql", sim$dbPath, sim$sqlDir)),
  # #     pools = as.matrix(getTable("pools.sql", sim$dbPath, sim$sqlDir)),
  # #     domPools = as.matrix(getTable("domPools.sql", sim$dbPath, sim$sqlDir))
  # #   )
  # # }
  # if (!suppliedElsewhere(sim$pooldef)) {
  #   sim$pooldef <- CBMutils::.pooldef
  #   sim$poolCount <- length(sim$pooldef)
  # }

  # user provided data tables (3)------------------------------------------------------

  # 1. growth and yield information
  # userGcM3 and userGcM3URL, these files are the m3/ha and age info by growth
  # curve ID, columns should be GrowthCurveComponentID	Age	MerchVolume
  ## TODO add a data manipulation to adjust if the m3 are not given on a yearly basis
  if (!suppliedElsewhere("userGcM3", sim)) {
    if (!suppliedElsewhere("userGcM3URL", sim)) {
      sim$userGcM3URL <- extractURL("userGcM3")
    }
    sim$userGcM3 <- prepInputs(url = sim$userGcM3URL,
                               destinationPath = inputPath(sim),
                               targetFile = "userGcM3.csv",
                               fun = "data.table::fread")
    names(sim$userGcM3) <- c("gcids", "Age", "MerchVolume")
    message(
      "User has not supplied growth curves (m3 by age or the file name for the growth curves). ",
      "The default will be used which is for a region in Saskatchewan."
    )
  }



  # 2. Disturbance information - see disturbance raster below
  # this may be provided by the user, by the defaults or by other modules/family
  # of modules. It is the link between the spatial location of the disturbance
  # (like a raster value) and the disturbance name.
  if (!suppliedElsewhere("userDist", sim)) {
    if (!suppliedElsewhere(sim$userDistFile)) {
      message("There is no disturbance information provided; defaults for the Saskatchewan example run will be used.")

    if (!suppliedElsewhere("userDistURL", sim)) {
      sim$userDistURL <- extractURL("userDist")
    }
    sim$userDist <- prepInputs(url = sim$userDistURL,
                               targetFile = "userDist.csv",
                               destinationPath = inputPath(sim),
                               fun = fread)
    }
  }

  # 3. cbmAdmin needed if the user is not running CBM_vol2biomass module.

  if (!suppliedElsewhere("cbmAdmin", sim)) {
    if (!suppliedElsewhere("cbmAdminURL", sim)) {
      sim$cbmAdminURL <- extractURL("cbmAdmin")
    }
    sim$cbmAdmin <- prepInputs(url = sim$cbmAdminURL,
                               targetFile = "cbmAdmin.csv",
                               destinationPath = inputPath(sim),
                               fun = fread)
  }

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

  # 1. Raster to match (masterRaster). This is the study area.
  ##TODO remove this note when we are done making everything work for the small
  ##study area in SK.
  #NOTE: we are providing the masterRaster in the globalCore1.R. This section is
  #being slipped.
      if (!suppliedElsewhere("masterRaster", sim)) {
        if (!suppliedElsewhere("masterRasterURL", sim)) {
    sim$masterRasterURL <- extractURL("masterRaster")

      ##TODO: why is this
      message(
        "User has not supplied a masterRaster or a URL for a masterRaster (masterRasterURL object).\n",
        "masterRaster is going to be read from the default URL given in the inputObjects for ",
        currentModule(sim)
      )

      ##TODO this is the masterRaster for all of SK managed forests..why is it
      ##not exactly 30 m res? Need to fix that.
      sim$masterRaster <- prepInputs(
        url = sim$masterRasterURL,
        fun = "terra::rast",
        destinationPath = inputPath(sim)
      )|> Cache()

        }

    }

  # 2. Age raster from inventory or from user as a vector
  if(!suppliedElsewhere(sim$ages)){
    if (!suppliedElsewhere(sim$ageRaster)) {
      if (!suppliedElsewhere(sim$ageRasterURL)) {
        sim$ageRasterURL <- extractURL("ageRaster")
      }
      sim$ageRaster <- prepInputs(
        url = sim$ageRasterURL,
        fun = "terra::rast",
        to = sim$masterRaster,
        method = "near", # need integers
        destinationPath = inputPath(sim)
      )|> Cache()
      ## TODO: put in a message to out pointing the max age (this has to be
      ## sinked to the max age on the growth curve max age for the spinup)
      # maxAge <- max(sim$ageRaster)
      # message(max age on the raster is XX)
    }
    ##or max age from vector
  }

  # 3. What growth curve should be applied to what pixels? Provide a raster or a
  # vector
  if(!suppliedElsewhere(sim$gcids)){
    if (!suppliedElsewhere(sim$gcIndexRaster)) {
      if (!suppliedElsewhere(sim$gcIndexRasterURL)) {
        sim$gcIndexRasterURL <- extractURL("gcIndexRaster")
      }
      sim$gcIndexRaster <- prepInputs(
        url = sim$gcIndexRasterURL,
        fun = "terra::rast",
        to = sim$masterRaster,
        method = "near", # need integers
        destinationPath = inputPath(sim))|> Cache()
    }
  }

  # 4. Spatial Unit raster. This takes the masterRaster (study area) and figures
  # out what CBM-specific spatial units each pixels. This determines some
  # defaults CBM-parameters across Canada.
  # if(!suppliedElsewhere(sim$spatialUnits)){
  if (!suppliedElsewhere(sim$spuRaster)) {
    if (!suppliedElsewhere(sim$spuRasterURL)) {
      sim$spuRasterURL <- extractURL("spuRaster")
    }
    ##TODO Need to check that there SPU match what the CAT is using.
    # The current PSPU data for the CAT is here:
    #\\vic-fas2\cat\NFCMARS_admin\Data\SpatialFramework\PSPUS.zip
    # - Scott put that same shapefile on a googleDrive here is a link to PSPUS.zip
    # https://drive.google.com/file/d/1Z_pMwhylqMkKZfOArATaAz9pUVNXtBAd/view?usp=sharing
    # - we need to compare those two files.
    canadaSpu <- prepInputs(
      targetFile = "spUnit_Locator.shp",
      url = sim$spuRasterURL,
      destinationPath = inputPath(sim),
      alsoExtract = "similar")|> Cache()

    spuShp <- Cache(postProcess,
                    canadaSpu,
                    to = sim$masterRaster,
                    method = "near", # need integers
                    #targetCRS = terra::crs(sim$masterRaster),
                    useCache = FALSE, filename2 = NULL
    ) |> st_collection_extract("POLYGON")

    sim$spuRaster <- terra::rasterize(terra::vect(spuShp),
                                      terra::rast(sim$masterRaster),
                                      field = "spu_id")
  }
  # }

  # 5. Ecozone raster. This takes the masterRaster (study area) and figures
  # out what ecozones each pixels are in. This determines some
  # defaults CBM-parameters across Canada.
  # if(!suppliedElsewhere(sim$ecozones)){
  if (!suppliedElsewhere(sim$ecoRaster)) {
    if (!suppliedElsewhere(sim$ecoRasterURL)) {
      sim$ecoRasterURL <- extractURL("ecoRaster")
    }
    ecozones <- Cache(prepInputs,
                      url = sim$ecoRasterURL,
                      alsoExtract = "similar",
                      destinationPath = inputPath(sim),
                      filename1 = "ecozone_shp.zip",
                      # overwrite = TRUE, ## not needed if filename1 specified
                      fun = sf::st_read(targetFile, quiet = TRUE), #"terra::vect",
                      rasterToMatch = sim$masterRaster
    ) ## ecozones is a SpatVect class object
    ## TODO: terra::vect fails on some windows machines. Windows does not
    ## recognize some of the french characters.
    # ecozones <- terra::vect(ecozones)

    ## DEC 4 2024: this prepInputs call current doesn't work. It is an issue with one of the packages in prepInputs and as of right now only affects this file.
    ## either there will be a prepInputs fix for this or Camille will find an alternative url to download, whichever comes first.
    ## For now, download the shapefile here: http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip and save to spadesCBM/inputs.

    sim$ecoRaster <- terra::rasterize(ecozones, sim$masterRaster, field = "ECOZONE")
  }
  # }



  ## TODO: hard stop here if there are NA values and have user fix them to prevent issues downstream
  if (any(is.na(values(sim$ecoRaster)))) {
    stop("ecoRaster cannot contain NA values. Please fix these and rerun.")
  }

  # Summarize raster values into table
  dtRasters <- data.table(
    ages            = terra::values(sim$ageRaster)[,1],
    spatial_unit_id = terra::values(sim$spuRaster)[,1],
    gcids           = terra::values(sim$gcIndexRaster)[,1],
    ecozones        = terra::values(sim$ecoRaster)[,1]
  )

  # assertion -- if there are both NAs or both have data, then the columns with be the same, so sum is either 0 or 2
  if (isTRUE(P(sim)$doAssertions)) {
    bbb <- apply(dtRasters, 1, function(x) sum(is.na(x)))
    if (!all(names(table(bbb)) %in% c("0", "4")))
      stop("should be only 0 or 4s")
  }

  ## There seems to be a caching problem here, the name adjustments
  ##
  sim$allPixDT <- as.data.table(cbind(dtRasters,
                                      pixelIndex = 1:ncell(sim$gcIndexRaster)))

    ##TODO get rid of this name change but keeping it to track "wrong names" through the scripts.
    # chgNamesTo <- c("growth_curve_component_id", "ages", "ecozones", "spatial_unit_id",
    #                 "pixelIndex", "growth_curve_id")
    # setnames(sim$allPixDT,names(sim$allPixDT),chgNamesTo)


  # 6. Disturbance rasters. The default example is a list of rasters, one for each year.
  #    But these can be provided by another family of modules in the annual event.

  ## TODO: add a message if no information is provided asking the user if
  ## disturbances will be provided on a yearly basis.
  if (!suppliedElsewhere("disturbanceRasters", sim)) {
    ## download the data and identify the .grd files present
    out <- preProcess(url = extractURL("disturbanceRasters"),
                      destinationPath = inputPath(sim),
                      filename1 = "disturbance_testArea.zip")

    sim$disturbanceRasters <- list.files(
      file.path(inputPath(sim), "disturbance_testArea"),
      pattern = "[.]grd$",
      full.names = TRUE
    )
    stopifnot(length(sim$disturbanceRasters) > 0)
  }


  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}
