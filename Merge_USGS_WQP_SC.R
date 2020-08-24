library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
########################### Merge all daily and point USGS data and WQP data 
# Format and merge USGS SC uv, dv, and qw data ####
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# daily data
USGS_dv <- readRDS("USGS_SC_dv_dqi.rds")
# point data
USGS_qw <- readRDS("USGS_SC_qw_dqi.rds")
# unit value (daily) data
USGS_uv <- readRDS("USGS_SC_uv_daily_dqi.rds")

# Get all df's into the same format, standardize column names
dfs <- list(USGS_dv, USGS_qw, USGS_uv)
lapply(dfs, colnames)
rm(dfs)
USGS_dv <- select(USGS_dv, c("SiteID", "DateTime", "Specific.Conductance"))
colnames(USGS_dv) <- c("SiteID", "Date", "SpC")
USGS_qw <- select(USGS_qw, c("SiteID", "Date", "mean"))
colnames(USGS_qw) <- c("SiteID", "Date", "SpC")
USGS_uv <- select(USGS_uv, c("SiteID", "Date", "Specific.Conductance"))
colnames(USGS_uv) <- c("SiteID", "Date", "SpC")

USGS_dv$SiteDate <- paste(USGS_dv$SiteID, USGS_dv$Date, sep = " ")
USGS_uv$SiteDate <- paste(USGS_uv$SiteID, USGS_uv$Date, sep = " ")
USGS_qw$SiteDate <- paste(USGS_qw$SiteID, USGS_qw$Date, sep = " ")

sapply(USGS_dv, class)
sapply(USGS_qw, class)
sapply(USGS_uv, class)

# confirm we don't have overlops of site-date's with the daily value data and the unit value data we aggregated into daily data
dv_check <- USGS_dv
uv_check <- USGS_uv
qw_check <- USGS_qw

overlap <- intersect(dv_check$SiteDate, uv_check$SiteDate) # empty (we made sure of this earlier)
overlap1 <- intersect(dv_check$SiteDate, qw_check$SiteDate) # 6420
print(overlap1) # if you copy/paste a site-date into the search bar of the two dfs, you'll see the values are close
overlap2 <- intersect(uv_check$SiteDate, qw_check$SiteDate) # 76
print(overlap2) # if you copy/paste a site-date into the search bar of the two dfs, you'll see the values are close

# average overlaps or say that we used one set of data where overlaps occurred?
# average: (bind, then group_by/summarise_at)
class(dv_check)
class(qw_check)
qw_check <- as.data.frame(qw_check)
dat_qwdv <- rbind(dv_check, qw_check, deparse.level = 1)
dat_qwdv <- dat_qwdv %>%
  group_by(SiteID, Date, SiteDate) %>%
  summarise_at(.vars = "SpC", .funs = c("SpC" = mean))
dat_qwdv$SpC <- round(dat_qwdv$SpC, digits = 0)
# subset for only the site-dates that were overlapping
dat_qwdv <- subset(dat_qwdv, SiteDate %in% overlap1) # 6420

class(uv_check)
uv_check <- as.data.frame(uv_check)
dat_qwuv <- rbind(uv_check, qw_check, deparse.level = 1)
dat_qwuv <- dat_qwuv %>%
  group_by(SiteID, Date, SiteDate) %>%
  summarise_at(.vars = "SpC", .funs = c("SpC" = mean))
dat_qwuv$SpC <- round(dat_qwuv$SpC, digits = 0)
# subset for only the site-dates that were overlapping
dat_qwuv <- subset(dat_qwuv, SiteDate %in% overlap2) # 76

# overlap <- intersect(dat_qwdv$SiteDate, dat_qwuv$SiteDate) # empty
# now remove this data from qw, uv, and dv entirely
USGS_qw1 <- USGS_qw[which(USGS_qw$SiteDate %in% overlap1),] # take the qw data that overlapped with dv
USGS_qw2 <- USGS_qw[which(USGS_qw$SiteDate %in% overlap2),] # take the qw data that overlapped with uv
USGS_qw_rm <- rbind(USGS_qw1, USGS_qw2) # combine into one df that represents what we want to remove from qw
USGS_qw <- USGS_qw[-which(USGS_qw$SiteDate %in% USGS_qw_rm$SiteDate),] # subset

USGS_dv <- USGS_dv[-which(USGS_dv$SiteDate %in% dat_qwdv$SiteDate),]
USGS_uv <- USGS_uv[-which(USGS_uv$SiteDate %in% dat_qwuv$SiteDate),]

USGS <- rbind_list(USGS_qw, USGS_dv, USGS_uv)
duplicated <- dat[duplicated(dat[4]),] # none!
unique(dat$SiteDate)

rm(overlap, overlap1, overlap2, overlap3, uv_check, dv_check, qw_check, USGS_qw1, USGS_qw2, USGS_qw_rm, dat_qwdv, dat_qwu, duplicated)

colnames(USGS)
USGS$SiteID <- paste0("USGS-", USGS$SiteID)
class(USGS$SiteID)
USGS$SiteID <- factor(USGS$SiteID)
levels(USGS$SiteID) # there are 159 sites with SC just from USGS 

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
