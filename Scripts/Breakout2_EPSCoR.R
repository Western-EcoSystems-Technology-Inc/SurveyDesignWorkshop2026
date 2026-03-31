
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


###############################################################################
# load libraries
###############################################################################
library(tidyverse)  
require(spsurvey)

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


#########################
# Examine survey notes
#########################

table(Chinook$Notes)
table(Chinook$basin, Chinook$Notes)


#########################
# Adjust weights
#########################

# What proportion of the sampled reaches are in the target population for each basin? 

# What is the response rate for each basin? 

# What is the inclusion probability for each basin? 

# What is the design weight for each basin? 

# create an indicator for each type of nonsampling error
Chinook$frameInd <- ifelse(Chinook$Notes=='upstream of natural barrier',0,1)
Chinook$respInd <- ifelse(Chinook$Notes=='inaccessible',0,1)

adjWgt <- Chinook |>
   group_by(basin) |>
   summarize(n=n(),
             n_prime = XXX,
             m = XXX,
             targetRate = XXX,
             respRate = XXX,
             wgt= XXX,
             inclProbAdj = XXX,
             wgtAdj = XXX) |>   
   as.data.frame()
adjWgt[,-which(names(adjWgt)=='geometry')]

saveRDS(adjWgt,file.path(datPath, 'adjWgt.rds'))


save.image(file.path(workPath,'Breakout2.RData'))

