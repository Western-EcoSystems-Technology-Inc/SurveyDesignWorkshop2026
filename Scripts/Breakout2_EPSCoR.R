
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
# After surveying we two different issues occurred in the field
# 1) Some streams had natural barriers that Chinook couldn't cross
# 2) Some reaches were inaccessible to surveyors

# create an indicator for each type of nonsampling error,
# What type of nonsampling error are these?
Chinook$targetInd <- ifelse(Chinook$Notes=='upstream of natural barrier',0,1)
Chinook$responseInd <- ifelse(Chinook$Notes=='inaccessible',0, 1)


# What proportion of the sampled reaches are in the target population for each basin? (target rate) 

# What is the response rate for each basin? 

# What is the inclusion probability for each basin? 

# What is the design weight for each basin? 

## Answer pool for the code below
# m/n_prime
# unique(wgt)
# sum(targetInd)
# sum(wgt) * n_prime/n
# respRate/unique(wgt)
# unique(wgt)/respRate
# n_prime/n
# sum(responseInd)

# How do we calculate the adjusted weights from the new sample frame and nonresponse information?
adjWgt <- Chinook |>
   group_by(basin) |>
  # Add in answers from above pool of answers
   summarize(N = sum(wgt), # original sample frame
             n = n(),      # original sample units 
             n_prime = FILL_IN_FROM_ABOVE, # 
             N_prime = FILL_IN_FROM_ABOVE,  
             m = FILL_IN_FROM_ABOVE,
             targetRate = FILL_IN_FROM_ABOVE,
             respRate = FILL_IN_FROM_ABOVE,
             wgt = FILL_IN_FROM_ABOVE,
             inclProbAdj = FILL_IN_FROM_ABOVE,
             wgtAdj = FILL_IN_FROM_ABOVE) |>   
   as.data.frame()

# Review the outputs
adjWgt[,-which(names(adjWgt)=='geometry')]



# Save the files for next Breakout Session
saveRDS(adjWgt,file.path(datPath, 'adjWgt.rds'))
save.image(file.path(workPath,'Breakout2.RData'))

