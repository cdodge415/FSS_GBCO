library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
# setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
#---------------------
# Read in SC qw Data ####
#---------------------
SC <- readRDS("USGS_SC_dv_data.rds")
colnames(SC)
SC <- select(SC, -c("agency_cd"))
colnames(SC) <- c("SiteID", "DateTime", "Specific.Conductance", "dqi")
sapply(SC, class)
SC$dqi <- as.factor(SC$dqi)
levels(SC$dqi)
# [1] "A"     "A [4]" "A e"   "P"     "P [4]" "P Dis" "P Dry" "P Eqp" "P Ice"
# ask Phil about these, but for now just filter for each one that has A
SC <- subset(SC, dqi == "A" | dqi == "A [4]" | dqi == "A e")
sapply(SC, class)
SC$SiteID <- as.factor(SC$SiteID)
levels(SC$SiteID)
# 132 dv sites
# save the data quality filtered data
saveRDS(SC, "USGS_SC_dv_dqi.rds")
