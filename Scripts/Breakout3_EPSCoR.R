
#########################################################
# EPSCoR Survey Design Workshop April 2, 2006
# See https://cran.r-project.org/web/packages/spsurvey/vignettes/start-here.html
#     for more information on drawing samples
# See https://usepa.github.io/spsurvey/articles/EDA.html for more info 
#     on spsurvey package plotting
# LA Starcevich & Jared Swenson
# Western EcoSystems Technology, Inc.
#########################################################

rm(list=ls())

###############################################################################
# paths
###############################################################################

path <- getwd()  
framePath <- file.path(path,'Frames')
outPath <- file.path(path,'Output')
workPath <- file.path(path,'Workspaces')
datPath <- file.path(path,'Data')
codePath <- file.path(path,'Code')


###############################################################################
# load libraries
###############################################################################
library(tidyverse)  
require(spsurvey)

######################################
# Read function script
######################################
file.sources <- list.files(path = codePath, pattern = "*.R")
setwd(codePath)
sapply(file.sources,source,.GlobalEnv)
setwd(path)


###############################################################################
# set random seed for reproducibility
###############################################################################
#runif(1,0,10000000)  # 2776671
set.seed(2776671)

###############################################################################
# Read data
###############################################################################

Chinook <- readRDS(file.path(datPath, 'Chinook.rds'))
names(Chinook)
adjWgt <- readRDS(file.path(datPath, 'adjWgt.rds'))
Chinook <- merge(Chinook, adjWgt[,c('basin','wgtAdj')])

# create sf 
Chinook_sf <- st_as_sf(Chinook,geometry=Chinook$geometry, crs="EPSG:5070")


#########################
# Estimate total redds
#########################


###################
# HT estimator  
###################
# use spsurvey - vignette("start-here", "spsurvey")
# use only responding reaches within the frame

# By Basin/Stratum
basin_redd_ests_ByBasin <- cont_analysis(
  Chinook_sf[Chinook_sf$Notes=='surveyed',],
  siteID = "Reach",
  vars = "Y", 
  weight = "wgtAdj",
  subpops = "basin",
  stratumID = "basin",
  All_Sites = TRUE
)
basin_redd_ests_ByBasin$Total



###################
# Ratio estimator
###################

Frame <- as.data.frame(st_read(file.path(framePath,'LewisRiver_Frame_Point.shp')))
Frame$StrataCode <- as.numeric(as.factor(Frame$basin))
Frame$stratum <- Frame$basin

RatioEstTotal_Strata(data.frame(Chinook), Frame, 'Y')


save.image(file.path(workPath,'Breakout3.RData'))

