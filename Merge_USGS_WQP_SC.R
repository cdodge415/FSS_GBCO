library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
########################### Merge all daily and point USGS data and WQP data 
# Format and merge daily USGS SC data and point USGS SC data ####
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
# daily data
USGS_SCd <- readRDS("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil/USGS_SC_all_daily_data.rds")
# point data
USGS_SCp <- readRDS("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil/USGS_SC_qw_dqiAR.rds")

colnames(USGS_SCp)
# subset columns
USGS_SCp <- select(USGS_SCp, c("site_no", "sample_dt", "sample_tm", "result_va"))

# see if we have duplicated data
USGS_SCp$SiteDate <- paste(USGS_SCp$site_no,USGS_SCp$sample_dt, sep = " ")
class(USGS_SCp$SiteDate)
USGS_SCp$SiteDate <- as.factor(USGS_SCp$SiteDate)
levels(USGS_SCp$SiteDate) # 55,196
unique(USGS_SCp$SiteDate) # 55,196
USGS_SCp_sub <- USGS_SCp
USGS_SCp_sub <- USGS_SCp_sub[duplicated(USGS_SCp_sub[5]),] # look at these and confirm that there are days with multiple measurements
# confirmed: we have days of point sampling where multiple samples were taken, so average by date to get daily data
rm(USGS_SCp_sub)

USGS_SCpd <- USGS_SCp %>%
  group_by(site_no, sample_dt, SiteDate) %>%
  summarise_at(.vars = "result_va", .funs = c("mean"=mean))
USGS_SCpd$mean <- round(USGS_SCpd$mean, digits = 0) # create a dataframe of data made from point data with no more than 1 value per day
rm(USGS_SCp)

# combine daily data and point data
# format the two dfs to be the same first
colnames(USGS_SCpd)
colnames(USGS_SCd)
colnames(USGS_SCpd) <- c("SiteID", "Date", "SiteDate", "SpC")
USGS_SCd <- select(USGS_SCd, -c("Site_Date"))
USGS_SCd$SiteDate <- paste(USGS_SCd$SiteID, USGS_SCd$Date, sep = " ")
USGS_SCd <- select(USGS_SCd, c("SiteID", "Date", "SiteDate", "SpC"))
sapply(USGS_SCd, class)
sapply(USGS_SCpd, class)
USGS_SCpd$SiteID <- as.factor(USGS_SCpd$SiteID)
USGS_SCd$SiteDate <- as.factor(USGS_SCd$SiteDate)
class(USGS_SCd)
class(USGS_SCpd)
USGS_SCpd <- as.data.frame(USGS_SCpd)
# now combine
SC <- rbind(USGS_SCd, USGS_SCpd)
rm(USGS_SCd, USGS_SCpd)
class(SC$SiteID)
SC$SiteID <- factor(SC$SiteID)
levels(SC$SiteID)
# now see what kind of site date overlaps we have between the point data and daily data
# see if we have duplicated data
levels(SC$SiteDate) # 358,098
unique(SC$SiteDate) # 358, 098
SC_sub <- SC
SC_sub <- SC %>%
  group_by(SiteID, Date, SiteDate) %>%
  summarise_at(.vars = "SpC", .funs = c("mean"=mean))
SC_sub$mean <- round(SC_sub$mean, digits = 0)
SC <- SC_sub
rm(SC_sub)
SC$SiteID <- paste0("USGS-", SC$SiteID)
colnames(SC)[4] <- "SpC"
SC$SiteDate <- paste(SC$SiteID, SC$Date, sep = " ")
USGS <- SC
rm(SC)
class(USGS$SiteID)
USGS$SiteID <- factor(USGS$SiteID)
levels(USGS$SiteID) # there are 179 sites with SC just from USGS daily and point measurements
# SC is now all of the USGS data we will use
# Now onto the WQP data

# Format WQP SC data and merge it with USGS SC data ####
setwd("/Users/laurenbolotin/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted TS")
WQ <- read.csv("WQ_SC_TS_final_sites.csv")

# let's just grab SC data for now
colnames(WQ)
WQ <- select(WQ, c("SiteID", "DateTime", "Specific.conductance"))
WQ <- WQ[complete.cases(WQ),] # get rid of NA values for SC
class(WQ$SiteID)
WQ$SiteID <- factor(WQ$SiteID)
levels(WQ$SiteID) # 25,623 sites for WQP SC data

colnames(WQ)[3] <- c("SpC")
WQ$SpC <- round(WQ$SpC, digits = 0)

# get this data into the same format as the USGS data
WQ$Date <- date(WQ$DateTime)
WQ$SiteDateTime <- paste(WQ$SiteID, WQ$DateTime, sep = " ")

WQ$SiteDateTime <- as.factor(WQ$SiteDateTime)
levels(WQ$SiteDateTime) # 674,210 (out of a 674,969 row df)
unique(WQ$SiteDateTime) # 674,210
duplicated(WQ$SiteDateTime)
# I need to understand this better
WQ_sub <- WQ
WQ_sub <- WQ_sub[duplicated(WQ_sub[5]),] # 759 obs. of duplicated data
# you can check by looking at WQ_sub, copying a SiteDateTime, and searching it in WQ
rm(WQ_sub)
# Since we have duplicates, average across them
WQ_sub <- WQ %>%
  group_by(SiteID, DateTime, SiteDateTime) %>%
  summarise_at(.vars = "SpC", .funs = c("mean"=mean))
WQ_sub$mean <- round(WQ_sub$mean, digits = 0)
WQ <- WQ_sub
rm(WQ_sub)
class(WQ$SiteID)
WQ$SiteID <- factor(WQ$SiteID)
levels(WQ$SiteID) # Still 57, 222
WQ <- select(WQ, -c("SiteDateTime")) # do we still need this?
WQ$Date <- date(WQ$DateTime)
class(WQ)
WQ <- as.data.frame(WQ)
colnames(WQ)
WQ <- select(WQ, -c("DateTime")) # Get rid of DateTime because we aren't looking at time. We have ensured no more than 1 value for each date
WQ$SiteDate <- paste(WQ$SiteID, WQ$Date)
colnames(USGS)
colnames(WQ)
WQ <- select(WQ, c("SiteID", "Date", "SiteDate", "mean"))
colnames(WQ)[4] <- "SpC"
class(WQ$SiteID)
levels(WQ$SiteID) # Still 25,623 sites from WQP
# Merge USGS and WQP SC data ####
# The USGS and WQP SC data are now in the same format and can be combined
class(USGS)
class(WQ)
USGS <- as.data.frame(USGS)
SC <- rbind(USGS, WQ)

sapply(SC, class)
SC$SiteID <- as.factor(SC$SiteID)
SC$SiteDate <- as.factor(SC$SiteDate)
levels(SC$SiteID) # 25,624
SC$SiteID <- factor(SC$SiteID)
levels(SC$SiteDate) # 726,499 (out of a 814,880 row df)
unique(SC$SiteDate)
#duplicated(WQ$SiteDate)
WQ_sub <- WQ_sub[duplicated(WQ_sub[5]),] # 759 obs. of duplicated data
# you can check by looking at WQ_sub, copying a SiteDateTime, and searching it in WQ
rm(WQ_sub)
# I need to understand this better
SC_sub <- SC
SC_sub <- SC_sub[duplicated(SC_sub[3]),] # 175,900 obs. of duplicated data
# wasn't this supposed to be taken care of already?
SC_sub <- SC %>%
  group_by(SiteID, Date, SiteDate) %>%
  summarise_at(.vars = "SpC", .funs = c("SpC"=mean))
SC_sub$SpC <- round(SC_sub$SpC, digits = 0)
SC_sub_check <- SC_sub[duplicated(SC_sub[3]),] #now this is empty which means we have no duplicates
rm(SC_sub_check)

# Check that this method works
colnames(SC)
colnames(SC_sub)
# comparison <- setdiff(SC, SC_sub) # What is in SC that is not in SC_sub? This is what we want to see. 
# comparison2 <- setdiff(SC_sub, SC) # What is in SC_sub that is not in SC?
rm(comparison, comparison2, USGS, WQ)
raw <- SC
SC <- SC_sub  
rm(SC_sub)

class(SC$SiteID)
SC$SiteID <- factor(SC$SiteID)
levels(SC$SiteID) # Final number of sites from all sources is 57,223


# Output this new, combined data
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/Finalized TS (All Sources)")
saveRDS(SC, "all_SC_data.rds")
