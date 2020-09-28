# USGS Q Data EDA
# Bring in all USGS discharge data for HUC 14, 15, & 16
# Discharge data is daily average discharge

x <- c("tidyverse", "lubridate")
lapply(x, require, character.only = TRUE)
rm(x)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
Q <- readRDS("USGS_disch_data.rds")
colnames(Q) # we need to keep all of these for now 
colnames(Q)[2] <- "SiteID"
Q$SiteID <- paste0("USGS-", Q$SiteID)

## We have data through column 6 and the two discharge columns with data have the same data
Qsub <- select(Q, c(2:5))
head(Qsub)
colnames(Qsub) <- c("SiteID", "Date", "Q_cfs", "dqi")
Qsub$SiteDate <- paste0(Qsub$SiteID, " ", Qsub$Date)
Qsub$SiteDate <- as.factor(Qsub$SiteDate)

# use SC data to filter for our sites of interest
SC <- readRDS("all_SC_data.rds") 
SCsub <- subset(SC, SC$Source == "USGS")
SCsub$SiteID <- factor(SCsub$SiteID)
levels(SCsub$SiteID) # 159
head(SC)

# Q$Date <- ymd(Q$Date)
# Q$Year <- year(Q$Date)

# Filter for site-dates where we have SC Data
Qsub <- Qsub[which(Qsub$SiteDate %in% SC$SiteDate),]
Qsub2 <- Qsub[unique(Qsub$SiteDate),]

Qsub2 <- Qsub2[complete.cases(Qsub2),]
table(Qsub2$dqi)

Qsub2 <- subset(Qsub2, dqi == "A" | dqi == "A e") # (~94%)

Qsub2$SiteID <- factor(Qsub2$SiteID) # get rid of unused levels
levels(Qsub2$SiteID) # 420
setdiff(SCsub$SiteID, Qsub2$SiteID) # 89
setdiff(Qsub2$SiteID, SCsub$SiteID) # 350

# Add a column for Q in cubic meters per second
colnames(Qsub2)
Qsub2$Q_cms <- Qsub2$Q_cfs * 0.028316846592

# Save data file
saveRDS(Qsub2, "USGS_disch_dqi.rds")
