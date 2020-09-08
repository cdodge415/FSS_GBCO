rm(list = ls())
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
# setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
#---------------------
# Read in SC Data ####
#---------------------
SC <- readRDS("USGS_SC_uv_data.rds")
colnames(SC)
SC <- select(SC, -c("agency_cd"))
colnames(SC) <- c("SiteID", "DateTime", "Specific.Conductance", "dqi", "tz_cd")
sapply(SC, class)
SC$dqi <- as.factor(SC$dqi)
levels(SC$dqi)
# [1] "A"      "A [0]"  "A [4]"  "A [92]" "A [93]" "A e"    "A R"    "P"      "P Dis"  "P Dry" 
# [11] "P Eqp"  "P Ice"  "P Mnt"  "P Ssn" 
SC <- SC[!is.na(SC$Specific.Conductance),]
SC$dqi <- factor(SC$dqi)
levels(SC$dqi)
# "A"      "A [0]"  "A [4]"  "A [92]" "A [93]" "A e"    "A R"    "P" 
count(SC$dqi)
# x     freq
# 1      A        17169391 ~95% of data
# 2      A [0]    7
# 3      A [4]    298
# 4      A [92]   1343
# 5      A [93]   640
# 6      A e      678
# 7      A R      29872
# 8      P        836314 ~4.6%

SC <- subset(SC, dqi == "A")
SC$SiteID <- as.factor(SC$SiteID)
levels(SC$SiteID)
# 72 sites

# The smallest resolution of data we want is daily, so create daily data from the 15-minute data
# Take daily means, compare to USGS dv data
SC$Date <- as.Date(SC$DateTime)
SC_daily <- SC %>%
  group_by(SiteID ,Date) %>%
  summarise_at(.vars = "Specific.Conductance", .funs = c("mean"=mean))
SC_daily$mean <- round(SC_daily$mean, digits = 0)
  
colnames(SC_daily)[3] <- "Specific.Conductance"
# bring in dv SC data to compare
SCdv <- readRDS("USGS_SC_dv_dqi.rds")
common_sites <- intersect(SC_daily$SiteID, SCdv$SiteID) # 53 sites overlap 

SC_daily$Site_Date <- paste0(SC_daily$SiteID, "_", SC_daily$Date)
SCdv$DateTime <- date(SCdv$DateTime)
SCdv$Site_Date <- paste0(SCdv$SiteID, "_", SCdv$Date)
common_site_date <- intersect(SC_daily$Site_Date, SCdv$Site_Date) # 133878 site-dates overlap
# if you take a site_date from the new SC_daily data and compare it to the same site_date from the 
# USGS daily data, the values are the same. now, we just need to make sure we aren't using duplicates and subset the rest

# find the exact differences so we know what we are adding by making daily data from unit value data
# we will put these in separate dataframes and append them to the dataframe of overlapping data
diff1 <- setdiff(SC_daily$Site_Date, SCdv$Site_Date) #51541 in SC_daily that are not in SCdv
diff2 <- setdiff(SCdv$Site_Date, SC_daily$Site_Date) #232380 in SCdv that are not in SC_daily

SCuv_d <- SC_daily[which(SC_daily$Site_Date %in% diff1),] # create dataframe for the daily data we are adding by aggregating unit value data
# without adding duplicate data


#check
factor(SCuv_d$Site_Date)
# 51541 levels == 51541 site-dates/observations so we are good
SC_check <- unique(SCuv_d)
rm(SC_check) 

# get some info
sapply(SCuv_d, class)
SCuv_d$SiteID <- factor(SCuv_d$SiteID)
levels(SCuv_d$SiteID) # 54,541 sites

#output new unit value data (that has been turned into daily data)
getwd()
saveRDS(SCuv_d, "USGS_SC_uv_daily_dqi.rds")


