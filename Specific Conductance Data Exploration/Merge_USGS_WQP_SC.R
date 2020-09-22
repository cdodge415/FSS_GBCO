library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(data.table)
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

USGS_uv <- as.data.frame(USGS_uv)
USGS_qw <- as.data.frame(USGS_qw)
colnames(USGS_uv)
colnames(USGS_dv)
colnames(USGS_qw)

USGS <- do.call("rbind", list(USGS_qw, USGS_dv, USGS_uv))
duplicated <- USGS[duplicated(USGS[4]),] # none!
unique(USGS$SiteDate)

rm(overlap, overlap1, overlap2, overlap3, uv_check, dv_check, qw_check, USGS_qw1, USGS_qw2, USGS_qw_rm, dat_qwdv, dat_qwuv, duplicated)

# Standardize SiteID format between USGS and WQP data
colnames(USGS)
USGS$SiteID <- paste0("USGS-", USGS$SiteID)
class(USGS$SiteID)
USGS$SiteID <- factor(USGS$SiteID)
levels(USGS$SiteID) # there are 159 sites with SC just from USGS 
USGS <- select(USGS, -c("SiteDate"))
USGS$SiteDate <- paste(USGS$SiteID, USGS$Date, sep = " ") 
saveRDS(USGS, "USGS_SC.rds")
rm(dat_qwuv, USGS_dv, USGS_qw, USGS_uv)


# Now onto the WQP data
# Format WQP SC data and merge it with USGS SC data ####
setwd("/Users/laurenbolotin/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted TS")
WQ <- read.csv("WQ_TS_final_sites.csv")

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
#WQ <- select(WQ, -c("DateTime"))

WQ$SiteDate <- paste(WQ$SiteID, WQ$Date, sep = " ")

WQ$SiteDate <- as.factor(WQ$SiteDate)
levels(WQ$SiteDate) # 424,664 (out of a 457,474 row df)
unique(WQ$SiteDate) # 424,664
duplicated <- WQ[duplicated(WQ[5]),] # 32,810 (reminder that this does not include the first occurrence of each SiteDate)
# Since we have duplicates, average across them to get =< 1 value/day

WQ <- WQ %>%
  group_by(SiteID, Date, SiteDate) %>%
  summarise_at(.vars = "SpC", .funs = c("SpC"=mean))
WQ$SpC <- round(WQ$SpC, digits = 0)

class(WQ)
WQ <- as.data.frame(WQ)
class(WQ$SiteID)
WQ$SiteID <- factor(WQ$SiteID)
levels(WQ$SiteID) # Still 25,623
duplicated <- WQ[duplicated(WQ[3]),] # none!

colnames(USGS)
colnames(WQ)
WQ <- select(WQ, c("SiteID", "Date", "SpC", "SiteDate"))
saveRDS(WQ, "WQP_SC.rds")


# Merge USGS and WQP SC data ####
# The USGS and WQP SC data are now in the same format and can be combined
class(USGS)
class(WQ)
USGS <- as.data.frame(USGS)
USGS$Source <- "USGS"
class(USGS$SiteID)
class(WQ$SiteID)
USGS$SiteID <- factor(USGS$SiteID)
WQ$SiteID <- factor(WQ$SiteID)
levels(USGS$SiteID) # 159
levels(WQ$SiteID) # 25,623
WQ$Source <- "WQP"
SC <- rbind(USGS, WQ)

sapply(SC, class)
SC$SiteDate <- as.factor(SC$SiteDate)
levels(SC$SiteDate) # 867,739 (out of a 899,857 row df)

duplicated <- SC[duplicated(SC[4]),] # 37,136 duplicated site-dates, looks like WQP data that is a duplicate of USGS
# let's just use USGS data in this case, so remove these site dates of WQP data from the combined dataset
SC <- setdiff(SC, duplicated) # takes the data from SC that is not in duplicated

class(SC$SiteID)
SC$SiteID <- factor(SC$SiteID)
levels(SC$SiteID) # Final number of sites from all sources is 25,624

duplicated <- SC[duplicated(SC[4]),] # none!

# Output this new, combined data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(SC, "all_SC_data.rds")
