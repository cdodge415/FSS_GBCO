# USGS Q Data EDA
# Bring in all USGS discharge data for HUC 14, 15, & 16
# Discharge data is daily average discharge

x <- c("tidyverse", "lubridate")
lapply(x, require, character.only = TRUE)
rm(x)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
Q <- readRDS("USGS_disch_data.rds")
colnames(Q)
Q <- select(Q, c("site_no", "Date", 4, 5))
colnames(Q) <- c("SiteID", 'Date',"Q", "dqi")
Q$SiteID <- as.factor(Q$SiteID)

# use SC data to filter for our sites of interest
SC <- readRDS("all_SC_data.rds") 
head(SC)
Q$SiteID <- paste0("USGS-", Q$SiteID)
head(Q)
Q$SiteDate <- paste0(Q$SiteID, " ", Q$Date)
Q <- Q[which(Q$SiteDate %in% SC$SiteDate),]

Q$dqi <- as.factor(Q$dqi)
levels(Q$dqi)
Q <- subset(Q, dqi == "A") # (~94%)

Q$SiteID <- factor(Q$SiteID) # get rid of unused levels
levels(Q$SiteID) # 1,391 sites with Q data and SC data

diff <- setdiff(SC$SiteDate, Q$SiteDate)
diff <- as.data.frame(diff)












Q$Date <- ymd(Q$Date)
Q$Year <- year(Q$Date)
Q$SiteDate <- paste0(Q$SiteID, " ", Q$Date)
Q$SiteDate <- as.factor(Q$SiteDate)

# Filter for site-dates where we have SC Data
Q <- Q[which(Q$SiteDate %in% SC$SiteDate),]
dup <- Q[duplicated(Q$SiteDate),]
Q <- Q[unique(Q$SiteDate),]
rm(dup)

# Add a column for Q in cubic meters per second
colnames(Q)
colnames(Q)[3] <- "Q_cfs"

Q$Q_cms <- Q$Q_cfs * 0.028316846592

# Save data file
saveRDS(Q, "USGS_disch_dqi.rds")
