rm(list = ls())
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
# setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
#---------------------
# Read in SC qw Data ####
#---------------------
SC <- readRDS("USGS_SC_qw_data.rds")
# write.csv(SC, "USGS_SC_qw_data.csv") # To share with Phil
# How many rows have a range of dates as opposed to just one? # ~40%
# colSums(is.na(SC)) # sample_end_dt: ~60%


SC$dqi_cd <- as.factor(SC$dqi_cd)
levels(SC$dqi_cd)
# count(SC$dqi_cd)
# x freq
# 1 A 35969 ~ 87% of data   A = historical data
# 2 R  4804 ~ 11%           R = reviewed and approved
# 3 S   349                 S = provisional

# filter out provisional data
SC <- SC[-which(SC$dqi_cd == "S"),]

colnames(SC)
SC <- select(SC, c("site_no", "sample_dt", "sample_end_dt","sample_tm", "sample_end_tm", "result_va", "dqi_cd"))
colnames(SC) <- c("SiteID", "Date","End_Date","Time", "End_Time","Specific.conductance", "dqi")

# SC$DateTime <- ifelse(!is.na(SC$Time), paste0(SC$Date, " ", SC$Time), paste(SC$Date))
sapply(SC, class)
SC$SiteID <- as.factor(SC$SiteID)
levels(SC$SiteID)
# 46 qw sites

# we need to see if there are days with multiple values and if they are, average them to get one value per day of sampling
SC$SiteDate <- paste(SC$SiteID,SC$Date, sep = " ")
class(SC$SiteDate)
SC$SiteDate <- as.factor(SC$SiteDate)
levels(SC$SiteDate) # 38,268
unique(SC$SiteDate) # 38,268
SC_sub <- SC
SC_sub <- SC[duplicated(SC[7]),] # look at these and confirm that there are days with multiple measurements
# confirmed: we have days of point sampling where multiple samples were taken, so average by date to get daily data
rm(SC_sub)
SC <- SC %>%
  group_by(SiteID, Date, SiteDate) %>%
  summarise_at(.vars = "Specific.conductance", .funs = c("mean"=mean))
SC$mean <- round(SC$mean, digits = 0) # create a dataframe of data made from point data with no more than 1 value per day

# write new file with naming convention of other data quality filtered files just to avoid confusion
# even though we ended up keeping all of the data
saveRDS(SC, "USGS_SC_qw_dqi.rds")
# NOTE: right now, the data is just using the first day of a multi-day sample to represent the value of SC
# ask Phil if this is correct