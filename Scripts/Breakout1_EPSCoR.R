
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
# Read frames - point and linear
###############################################################################


#########################
# Point Frame
#########################

LewisRiver_Frame_Point <- st_read(file.path(framePath,'LewisRiver_Frame_Point.shp'))
names(LewisRiver_Frame_Point)
sum(LewisRiver_Frame_Point$length_mi)  # 74.21

# explore sampling frame - sum stream reach lengths by basin
PointFrame <- LewisRiver_Frame_Point |>
   group_by(basin) |>
   summarize(totalLength = sum(length_mi)) |>   
   as.data.frame()
PointFrame[,-which(names(PointFrame)=='geometry')]

#########################
# Linear Frame
#########################

LewisRiver_Frame_Linear <- st_read(file.path(framePath,'LewisRiver_Frame_Linear.shp'))
names(LewisRiver_Frame_Linear)
sum(LewisRiver_Frame_Linear$length_mi)  # 74.21

# sampling frame - sum stream reach lengths by basin
LinearFrame <- LewisRiver_Frame_Linear |>
   group_by(basin) |>
   summarize(totalLength = sum(length_mi)) |>   
   as.data.frame()
LinearFrame[,-which(names(LinearFrame)=='geometry')]


# Examine histograms of reach lengths
par(mfrow=c(2,1))
hist(LewisRiver_Frame_Point$length_mi,main='Point Frame',breaks=seq(0,0.4,.05)) # almost all ~0.3 miles
hist(LewisRiver_Frame_Linear$length_mi,main='Linear Frame',breaks=seq(0,0.4,.05)) # almost all ~0.3 miles

# Examine projections of each sampling frame - want equal area projection
st_crs(LewisRiver_Frame_Point)  # EPSG:5070, NAD83 / Conus Albers 
st_crs(LewisRiver_Frame_Linear)  # EPSG:5070, NAD83 / Conus Albers 
# These are equal area projections, so we can use this as-is for spatially balanced sampling
# otherwise, we would transform the projection to an equal area projection
#  so that distance N-S would be equal to distance E-W for spatial balance
#LewisRiver_Frame_Point <- st_transform(LewisRiver_Frame_Point,  "EPSG:5070")

# create sp_frame objects for sampling from the frames
class(LewisRiver_Frame_Point)  # "sf"         "data.frame"
LewisRiver_Frame_Point <- sp_frame(LewisRiver_Frame_Point)
class(LewisRiver_Frame_Point)  # "sp_frame"   "sf"         "data.frame"

class(LewisRiver_Frame_Linear)  # "sf"         "data.frame"
LewisRiver_Frame_Linear <- sp_frame(LewisRiver_Frame_Linear)
class(LewisRiver_Frame_Linear)  # "sp_frame"   "sf"         "data.frame"

####################################################################
####################################################################
# Draw equiprobable GRTS sample of 100 reaches 
####################################################################
####################################################################

#######################
# Point Frame
#######################
LewisRiver_Point_samp_equi <- spsurvey::grts(LewisRiver_Frame_Point, 
                       n_base = 100,  
                       seltype = 'equal'
)
names(LewisRiver_Point_samp_equi)
# "sites_legacy" "sites_base"   "sites_over"   "sites_near"   "design"

LewisRiver_Point_samp_equi$sites_base
# each point represents 2.53 reaches

# Plot sample with frame
plot(LewisRiver_Point_samp_equi,sframe=LewisRiver_Frame_Point,pch=19)  

# how many times is each reach selected?
table(table(LewisRiver_Point_samp_equi$sites_base$Reach))
#   1 
# 100


# examine design weights
# Point sample - sum weights by basin
wgts_Point_equi <- LewisRiver_Point_samp_equi$sites_base |>
   group_by(basin) |>
   summarize(totalWgt = sum(wgt),uniqueWgt = unique(wgt),n=n()) |>   
   as.data.frame()
wgts_Point_equi[,-which(names(wgts_Point_equi)=='geometry')]

# How is the weight calculated?
N <- nrow(LewisRiver_Frame_Point)
n <- nrow(LewisRiver_Point_samp_equi$sites_base)
N/n
# 



#######################
# Linear Frame
#######################
LewisRiver_Linear_samp_equi <- spsurvey::grts(LewisRiver_Frame_Linear, 
                       n_base = 100,  
                       seltype = 'equal'
)
names(LewisRiver_Linear_samp_equi)
# "sites_legacy" "sites_base"   "sites_over"   "sites_near"   "design"

# Check projection and class
st_crs(LewisRiver_Linear_samp_equi$sites_base) # EPSG:5070, NAD83 / Conus Albers
class(LewisRiver_Linear_samp_equi$sites_base)  # "sf"         "data.frame"

# Plot sample with frame
plot(LewisRiver_Linear_samp_equi,sframe=LewisRiver_Frame_Linear,pch=19,lwd=2)  

# how many times is each reach selected?
table(table(LewisRiver_Linear_samp_equi$sites_base$Reach))
# 1  2 
#88  6


# Linear sample
# spsurvey calculates weights in meters to match the projection
#  we recalulcate the weights in miles to match our reach units
wgts_Linear_equi <- LewisRiver_Linear_samp_equi$sites_base |>
   group_by(basin) |>
   summarize(totalWgt = sum(wgt),uniqueWgt = unique(wgt),
             totalWgtMi = sum(wgt*0.000621371),uniqueWgtMi = unique(wgt*0.000621371),n=n()) |>   
   as.data.frame()
wgts_Linear_equi[,-which(names(wgts_Linear_equi)=='geometry')]

# How is the weight calculated?
R <- sum(LewisRiver_Frame_Point$length_mi)
n <- nrow(LewisRiver_Point_samp_equi$sites_base)
R/n
# 
 
####################################################################
####################################################################
# Draw stratified GRTS sample of 18 to 32 reaches per basin 
####################################################################
####################################################################

# Define stratum sample sizes
strata_n <- c('Muddy' = 32, 'NFLewis' = 25, 'Pine' = 25, 'Swift' = 18)

#######################
# Point frame
#######################

# Draw stratified GRTS sample with equi selection within strata
LewisRiver_Point_samp_str <- spsurvey::grts(LewisRiver_Frame_Point, 
                       n_base = strata_n, 
                       stratum_var = 'basin',  
                       seltype = 'equal'
)


# Examine points by strata
table(LewisRiver_Point_samp_str$sites_base$basin)

# plot sample for all strata combined
plot(LewisRiver_Point_samp_str,
     formula = siteuse ~ 1,
     sframe=LewisRiver_Frame_Point, pch=19)


# examine design weights 
# Point sample
wgts_Point_str <- LewisRiver_Point_samp_str$sites_base |>
   group_by(basin) |>
   summarize(totalWgt = sum(wgt),
             uniqueWgt = unique(wgt),
             totalWgtMi = sum(wgt*mean(LewisRiver_Frame_Point$length_mi)),
             uniqueWgtMi = unique(wgt*mean(LewisRiver_Frame_Point$length_mi)),
             n=n()) |>   
   as.data.frame()
wgts_Point_str[,-which(names(wgts_Point_str)=='geometry')]
#    basin totalWgt uniqueWgt totalWgtMi uniqueWgtMi   n
#1   Muddy      124     3.875  36.371700   1.1366156  32
#2 NFLewis       58     2.320  17.012569   0.6805028  25
#3    Pine       53     2.120  15.545968   0.6218387  25
#4   Swift       18     1.000   5.279763   0.2933202  18

# How is the weight calculated?
Nh <- table(LewisRiver_Frame_Point$basin)
nh <- table(LewisRiver_Point_samp_str$sites_base$basin)
Nh/nh


#######################
# Linear frame
#######################

# Draw stratified GRTS sample with equi selection within strata
LewisRiver_Linear_samp_str <- spsurvey::grts(LewisRiver_Frame_Linear, 
                       n_base = strata_n, 
                       stratum_var = 'basin',  
                       seltype = 'equal'
)

# plot sample for all strata combined
plot(LewisRiver_Linear_samp_str,
     formula = siteuse ~ 1,
     sframe=LewisRiver_Frame_Linear)  


# examine design weights 
# Weights are given in meters -- let's convert to miles.
# 1 m = 0.000621371 mi
# Linear sample
wgts_Linear_str <- LewisRiver_Linear_samp_str$sites_base |>
   group_by(basin) |>
   summarize(totalWgt = sum(wgt),uniqueWgt = unique(wgt),
             totalWgtMi = sum(wgt*0.000621371),uniqueWgtMi = unique(wgt*0.000621371),n=n()) |>   
   as.data.frame()
wgts_Linear_str[,-which(names(wgts_Linear_str)=='geometry')]

# How is the weight calculated?
Rh <- aggregate(LewisRiver_Frame_Point$length_mi,list(LewisRiver_Frame_Point$basin),sum)[,2]
nh <- table(LewisRiver_Point_samp_str$sites_base$basin)
Rh/nh



save.image(file.path(workPath,'Breakout1.RData'))






