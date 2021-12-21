---
title: "CBM_dataPrep_SK"
author:
  - Celine Boisvenue
  - Alex Chubaty
date: "September 2021"
output:
  html_document:
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

This module is to read-in user-provided information or provide defaults.
It reads-in rasters (`ageRaster`, `ecoRaster`, `gcIndexRaster`, `spuRaster`, and `masterRaster`) from either defaults of provided by the user.
From the rasters, `pixelGroup` are created which are unique combinations of the rasters values.
`pixelGroup` is a main processing unit in SpaDES `CBM` simulations. In a first step, a `spatialDT` which is a `data.table` listing all pixels with their respective values of `raster`, `pixelIndex` and `pixelGroup` is created (`sim$spatialDT`).
From the `spatialDT`, a reduced `data.table` is create (`sim$level3DT`) which is the data.table from which processing will start in `CBM_core`.
The number of records in this data.table (`sim$level3DT`) should equal the number of pixel groups that will be processed in the spinup event of the `CBM_core` module.
This present module also creates variables of the same length as the rows in `level3DT` for use in other events of the `CBM_core` module.
These are: `returnIntervals`, `maxRotations`, `minRotations`, `lastPassDMIDs`, `historicDMIDs`, and delays all stored in the `simList.`

Another important object created in this module is `mySpuDmids`.
This `data.table` links the user-defined disturbances (`$userDist`) with a spatial unit and a disturbance matrix.
This will be used to apply disturbances to pixel groups in the annual event of the `CBM_core` module.
The `mySpuDmids` object is created starting from a user provided list of disturbances (`userDist`) that matches the `rasterId` of the disturbance raster to the disturbance name, and speficies if the disturbance is stand-replacing (`userDist$wholeStand == 1`) or not (`userDist$wholeStand == 1`).
The disturbance names (`userDist$distName`) and their location of the disturbance (linked via the rasterID to the `sim$mySpuDmids$spatial_unit id`) are used to associate a disturbance matrix identification number to the disturbed `pixelGroup`.
Disturbance Matrices (DM) determine what proportion of a carbon pool gets transferred to another carbon pool via disturbance.
There are 426 matrix IDs in the present default data (`sim$processes$disturbanceMatrices`).
DMIDs (Disturbance Matrix IDs) are part of the default data of CBM-CFS3.
DMs are specific to spatial units which are a numbering (48 of them `sim$cbmData@spatialUnitIds`) of the overlay of the administrative boundaries and ecozones in Canada. 
Spatial units are central units in CBM-CFS3, as are ecozones because both determining various ecological and other parameters that will be used in simulations via the `CBM_core` module. 
The proportion of carbon transferred by a specific DMID can be found here `sim$cbmData@disturbanceMatrixValues`.
A series of R-functions were built to help users associate the correct disturbance matrices (`spuDist()`, `mySpu()`, `seeDist()`, `simDist()`) and are searchable in this package.

Note: 
* CBM_defaults objects are recreated in the `.inputObject` of this module
* nothing is in carbon or carbon increments at this point. This module feeds into the CBM_core module as does the CBM_vol2biomass.R module. 

# Usage


```r
library(igraph)
library(SpaDES.core)

moduleDir <- getwd()
inputDir <- file.path(moduleDir, "inputs") %>% reproducible::checkPath(create = TRUE)
outputDir <- file.path(moduleDir, "outputs")
cacheDir <- file.path(outputDir, "cache")
times <- list(start = 0, end = 10)
parameters <- list(
  #CBM_dataPrep = list(.useCache = ".inputObjects")
 #.progress = list(type = "text", interval = 1), # for a progress bar
 ## If there are further modules, each can have its own set of parameters:
 #module1 = list(param1 = value1, param2 = value2),
 #module2 = list(param1 = value1, param2 = value2)
 )
modules <- list("CBM_dataPrep_SK")
objects <- list(
  userDistFile = file.path(moduleDir, "CBM_dataPrep_SK", "data", "userDist.csv")
)
paths <- list(
  cachePath = cacheDir,
  modulePath = moduleDir,
  inputPath = inputDir,
  outputPath = outputDir
)

myInputs <- simInit(times = times, params = parameters, modules = modules,
                    objects = objects, paths = paths)

outInputs <- spades(myInputs)
```

# Events

There is only when event (init) is this module.

# Data dependencies

## Module parameters


|paramName        |paramClass |default |min |max |paramDesc                                                                                                                                                |
|:----------------|:----------|:-------|:---|:---|:--------------------------------------------------------------------------------------------------------------------------------------------------------|
|.plotInitialTime |numeric    |NA      |NA  |NA  |This describes the simulation time at which the first plot event should occur                                                                            |
|.plotInterval    |numeric    |NA      |NA  |NA  |This describes the simulation time interval between plot events                                                                                          |
|.saveInitialTime |numeric    |NA      |NA  |NA  |This describes the simulation time at which the first save event should occur                                                                            |
|.saveInterval    |numeric    |NA      |NA  |NA  |This describes the simulation time interval between save events                                                                                          |
|.useCache        |logical    |TRUE    |NA  |NA  |Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant |

## Input data


|objectName         |objectClass |desc                                                                                                                                                                                  |sourceURL                                                         |
|:------------------|:-----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------|
|cbmData            |dataset     |S4 object created from selective reading in of cbm_default.db in CBM_efaults module                                                                                                   |NA                                                                |
|pooldef            |character   |Vector of names (characters) for each of the carbon pools, with `Input` being the first one                                                                                           |NA                                                                |
|PoolCount          |numeric     |count of the length of the Vector of names (characters) for each of the carbon pools, with `Input` being the first one                                                                |NA                                                                |
|dbPath             |character   |NA                                                                                                                                                                                    |NA                                                                |
|sqlDir             |character   |NA                                                                                                                                                                                    |NA                                                                |
|userDistFile       |character   |User provided file name that identifies disturbances for simulation (key words for searching CBM files, if not there the userDist will be created with defaults                       |NA                                                                |
|userDist           |data.table  |User provided file that identifies disturbances for simulation, if not there it will use userDistFile                                                                                 |https://drive.google.com/file/d/1Tsm1a1HJr8DIxLURqc9vb1Qis-TK56Xh |
|ageRasterURL       |character   |URL for ageRaster - optional, need this or a ageRaster                                                                                                                                |NA                                                                |
|ageRaster          |raster      |Raster ages for each pixel                                                                                                                                                            |https://drive.google.com/file/d/1hylk0D1vO19Dpg4zFtnSNhnyYP4j-bEA |
|gcIndexRasterURL   |character   |URL for ageRaster - optional, need this or a ageRaster                                                                                                                                |NA                                                                |
|gcIndexRaster      |raster      |Raster ages for each pixel                                                                                                                                                            |https://drive.google.com/file/d/1yunkaYCV2LIdqej45C4F9ir5j1An0KKr |
|spuRaster          |raster      |Raster has spatial units for each pixel                                                                                                                                               |NA                                                                |
|ecoRaster          |raster      |Raster has ecozones for each pixel                                                                                                                                                    |NA                                                                |
|userGcM3File       |character   |User file name for the files containing: GrowthCurveComponentID,Age,MerchVolume. Default name userGcM3                                                                                |NA                                                                |
|userGcM3           |dataframe   |User file containing: GrowthCurveComponentID,Age,MerchVolume. Default name userGcM3                                                                                                   |https://drive.google.com/file/d/1u7o2BzPZ2Bo7hNcC8nEctNpDmp7ce84m |
|disturbanceRasters |vector      |Character vector of the disturbance rasters for use in simulations - defaults are the Wulder and White rasters for SK.                                                                |https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt |
|masterRasterURL    |character   |URL for masterRaster - optional, need this or a masterRaster                                                                                                                          |NA                                                                |
|allPixDT           |data.table  |Data table built for all pixels (incluing NAs) for the four essential raster-based information, growth curve location (gcID), ages, ecozones and spatial unit id (CBM-parameter link) |NA                                                                |
|masterRaster       |raster      |Raster has NAs where there are no species and the pixel groupID where the pixels were simulated. It is used to map results                                                            |https://drive.google.com/file/d/1zUyFH8k6Ef4c_GiWMInKbwAl6m6gvLJW |

An example with all the user-provided rasters and `.csv` files is provided by default.
The example simulates a region of the managed forests of SK.
All rasters and data frames for this example are on a cloud-drive (`userDefaultData_CBM_SK`).
Unless using this example, the user most provide: 

* a raster of the study area at the desired resolution for simulation (`sim$masterRaster`)
* an age raster (`sim$ageRaster`)
* a raster indicating which growth curve should be applied to which pixels (`sim$gcIndexRaster`) or a URL for this raster (`sim$gcIndexRasterURL`).
* raster of disturbances for each year the user wants disturbances to be simulated.
  This information could come from other SpaDES modules (fireSence, other fire modules, insects modules, etc.).
  For retrospective simulation (past to present), rasters found here can be used anywhere in Canada <https://opendata.nfis.org/downloads/forest_change/CA_forest_harvest_mask_year_1985_2015.zip>.
* a `.csv` file of the growth curve for the study area (with links to the `sim$gcIndexRaster`), `sim$userGcM3.csv` or the location of this file (`sim$userGcM3File`).
  The `sim$userGcM3.csv` file is required to have three columns: 
    + "GrowthCurveComponentID", which will be the link to the raster `sim$gcIndexRaster`, 
    + "Age" ranging from 0 to the maximum age of the growth curve, and 
    + "MerchVolume" which is the cumulative value of m3/ha at each age along each growth curve.
* a file with the disturbances to be applied as well as their raster values (`sim$userDist`) or its location (`sim$userDistFile`).
  The `userDist.csv` file must have three columns:
    + "distName" representing a simple description of the disturbance type (e.g., fire, clearcut, deforestation, etc.).
    + "rasterId" which indications the value that this specific disturbance will have on the disturbance raster.
    + "wholeStand" indicating if the disturbance is stand-replacing disturbance (1) or a partial disturbance (0).

The user could provide:
* a raster of the ecozones in their study area (`sim$ecoRaster`), but the script will calculate this raster based on the `sim$masterRaster` if it is not provided.
* a raster of the spatial units (`sim$spuRaster`) but the script will calculate this raster based on the `sim$masterRaster` if it is not provided. 

## Output data


|objectName      |objectClass |desc                                                                                                                                                                     |
|:---------------|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|pools           |matrix      |NA                                                                                                                                                                       |
|curveID         |character   |Vector of column names that together, uniquely define growth curve id                                                                                                    |
|ages            |numeric     |Ages of the stands from the inventory in 1990 with with ages <=1 changes to 3 for the spinup                                                                             |
|realAges        |numeric     |Ages of the stands from the inventory in 1990 saved to replace the ages post spinup                                                                                      |
|nStands         |numeric     |not really the number of stands, but the number of pixel groups                                                                                                          |
|gcids           |numeric     |The identification of which growth curves to use on the specific stands provided by...                                                                                   |
|historicDMIDs   |numeric     |Vector, one for each stand, indicating historical disturbance type, linked to the S4 table called cbmData. Only Spinup.                                                  |
|lastPassDMIDS   |numeric     |Vector, one for each stand, indicating final disturbance type, linked to the S4 table called cbmData. Only Spinup.                                                       |
|delays          |numeric     |Vector, one for each stand, indicating regeneration delay post disturbance. Only Spinup.                                                                                 |
|minRotations    |numeric     |Vector, one for each stand, indicating minimum number of rotations. Only Spinup.                                                                                         |
|maxRotations    |numeric     |Vector, one for each stand, indicating maximum number of rotations. Only Spinup.                                                                                         |
|returnIntervals |numeric     |Vector, one for each stand, indicating the fixed fire return interval. Only Spinup.                                                                                      |
|spatialUnits    |numeric     |The id given to the intersection of province and ecozones across Canada, linked to the S4 table called cbmData                                                           |
|ecozones        |numeric     |Vector, one for each stand, indicating the numeric represenation of the Canadian ecozones, as used in CBM-CFS3                                                           |
|level3DT        |data.table  |the table linking the spu id, with the disturbance_matrix_id and the events. The events are the possible raster values from the disturbance rasters of Wulder and White. |
|spatialDT       |data.table  |the table containing one line per pixel                                                                                                                                  |
|mySpuDmids      |data.frame  |the table containing one line per pixel                                                                                                                                  |

# Links to other modules

- [`CBM_core`](https://github.com/PredictiveEcology/CBM_core)
- [`CBM_defaults`](https://github.com/PredictiveEcology/CBM_defaults)
- [`CBM_vol2biomass`](https://github.com/PredictiveEcology/CBM_vol2biomass)
