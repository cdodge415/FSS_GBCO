# USGS Q Data EDA
# Bring in all USGS discharge data for HUC 14, 15, & 16
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
Q <- readRDS("USGS_disch_data.rds")
colnames(Q)
Q <- select(Q, c("site_no", "Date", 4, 5))
colnames(Q) <- c("SiteID", 'Date',"Q", "dqi")
Q$SiteID <- as.factor(Q$SiteID)

SC <- readRDS("all_SC_data.rds") # use SC data to filter for our sites of interest
Q$SiteID <- paste0("USGS-", Q$SiteID)
Q <- Q[which(Q$SiteID %in% SC$SiteID),]
rm(SC)

Q$dqi <- as.factor(Q$dqi)
levels(Q$dqi) # "A"     "A [4]" "A <"   "A >"   "A e"   "A R"   "P"     "P ***" "P >"   "P Bkw" "P Dis" "P e"   "P Eqp" "P Ice" "P Rat" "P Ssn" "P Tst" "P ZFL"
length(Q$dqi[Q$dqi == "A"]) # I don't know that this is working correctly # (~ 95%)
Q_sub <- subset(Q, dqi == "A") # (~95%)

Q$SiteID <- factor(Q$SiteID) # get rid of unused levels
levels(Q$SiteID) # 1,548 sites with Q data

Q$Date <- ymd(Q$Date)
Q$Year <- year(Q$Date)