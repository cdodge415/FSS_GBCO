library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
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
# count(SC$dqi)
# x   freq
# A          365715   ~ 98% Approved for publication -- Processing and review completed.
# A [4]      236      ??Discharge less than indicated value which is Minimum Recordable Discharge at this site
# A e        307      Value has been edited or estimated by USGS personnel and is write protected
# P          6832     Provisional data subject to revision.
# P [4]      84       ??Discharge less than indicated value which is Minimum Recordable Discharge at this site
# P Dis      36       Data-collection discontinued
# P Dry      12       Dry
# P Eqp      33       Equipment malfunction
# P Ice      3        Ice affected
# Many values for these dqi's are NA -- filter these out:
SC <- SC[!is.na(SC$Specific.Conductance),]
SC$dqi <- factor(SC$dqi)
levels(SC$dqi)
# "A"     "A [4]" "A e"   "P"     "P [4]"
count(SC$dqi)
# x   freq
# 1   A        365715 ~ 98%
# 2   A [4]    236
# 3   A e      307
# 4   P        6832
# 5   P [4]    84

SC <- subset(SC, dqi == "A")
sapply(SC, class)
SC$SiteID <- as.factor(SC$SiteID)
levels(SC$SiteID)
# 132 dv sites
# save the data quality filtered data
saveRDS(SC, "USGS_SC_dv_dqi.rds")

