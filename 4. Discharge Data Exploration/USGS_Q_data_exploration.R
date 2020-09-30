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
head(Q)

# use SC data to filter for our sites of interest
SC <- readRDS("all_SC_data.rds") 
SCsub$SiteID <- factor(SCsub$SiteID)
levels(SCsub$SiteID) # 159
head(SC)
# Filter for site-dates where we have SC Data
Qsub <- Q[which(Q$SiteID %in% SC$SiteID),]
rm(Q)
sapply(Qsub, class)
Qsub$Date <- ymd(Qsub$Date)
head(Qsub)
summary(Qsub)
Qsub <- subset(Qsub, Qsub$Date >= as.POSIXct("1900-01-01")) # 17369431 -> 17357578
Qsub <- subset(Qsub, Qsub$Date <= as.POSIXct("2020-01-01")) # 17357578 -> 17232802

colnames(Qsub)
Qsub <- select(Qsub, -c("agency_cd"))
colnames(Qsub) <- c("SiteID", "Date", "Q1", "Q1_dqi", "Q2", "Q2_dqi", "Q3", "Q3_dqi", "Q4", "Q4_dqi", "Q5", "Q5_dqi")
# Qsub$SiteDate <- paste0(Qsub$SiteID, " ", Qsub$Date)
# Qsub$SiteDate <- as.factor(Qsub$SiteDate)
colnames(SC)
SC <- select(SC, -c("SiteDate"))
SC_Q <- merge(SC, Qsub, by = c("SiteID", "Date"), all.x = TRUE)
summary(SC_Q)

setdiff(SC_Q$Q2, SC_Q$Q3) # these two seem to have a lot in common but Q2 has some data Q3 does not
setdiff(SC_Q$Q3, SC_Q$Q2) # Q3 does not have any data that is not already in Q2, so we can get rid of it
# we can assign 1040 and A to USGS-09482500 1978-08-02 and then delete Q4 and Q4_dqi
# we can get rid of Q5 and Q5_dqi

SC_Q <- select(SC_Q, -c("Q3", "Q3_dqi", "Q5", "Q5_dqi"))
setdiff(SC_Q$Q2, SC_Q$Q1) # these two also had data in common, but there is nothing in Q2 that is not already in Q1, so we can get rid of it
SC_Q <- select(SC_Q, -c("Q2", "Q2_dqi"))
SC_Q$Q1[which(SC_Q$SiteID == "USGS-09482500" & SC_Q$Date == as.POSIXct("1978-08-02"))] <- 1040
SC_Q$Q1_dqi[which(SC_Q$SiteID == "USGS-09482500" & SC_Q$Date == as.POSIXct("1978-08-02"))] <- "A"
SC_Q <- select(SC_Q, -c("Q4", "Q4_dqi"))
SC_Qsub <- SC_Q[complete.cases(SC_Q),]

class(SC_Qsub$Q1_dqi)
SC_Qsub$Q1_dqi <- as.factor(SC_Qsub$Q1_dqi)
levels(SC_Qsub$Q1_dqi)
table(SC_Qsub$Q1_dqi)
SC_Qsub <- SC_Qsub[SC_Qsub$Q1_dqi == "A" | SC_Qsub$Q1_dqi == "A e", ]

colnames(SC_Qsub) <- c("SiteID", "Date", "SpC", "SpC_source", "Q_cfs", "Q_dqi")
SC_Qsub <- select(SC_Qsub, c("SiteID", "Date", "SpC", "Q_cfs", "SpC_source"))


SC_Qsub$SiteID <- factor(SC_Qsub$SiteID) # get rid of unused levels
levels(SC_Qsub$SiteID) # 1,394
setdiff(SC$SiteID, SC_Qsub$SiteID) # 24,230

# Add a column for Q in cubic meters per second
SC_Qsub$Q_cms <- SC_Qsub$Q_cfs * 0.028316846592

# Save data file
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(SC_Qsub, "USGS_disch_dqi.rds")
