# We may no longer need this script....we already merged the data in USGS_Q_exploration

############################################################
###### Merge USGS SC Data into a dataframe with USGS Q Data
############################################################
rm(list=ls())
x <- c("tidyverse", "data.table", "lubridate")
lapply(x, require, character.only = TRUE)
rm(x)

## Bring in data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
SC <- readRDS("all_SC_data.rds")
Q <- readRDS("USGS_disch_dqi.rds")

colnames(SC)
colnames(Q)
Q$SiteDate <- paste0(Q$SiteID, " ", Q$Date)

Q <- select(Q, c("SiteID", "Date", "Q_cfs", "Q_cms","SiteDate"))
Q$Source <- "USGS"

dat <- merge(SC, Q, by = c("SiteID", "Date", "SiteDate"))
colnames(dat)
colnames(dat) <- c("SiteID", "Date", "SiteDate", "SpC", "Source_SpC", "Q_cfs", "Q_cms", "Source_Q")

dat$Spc_Qcms <- dat$SpC / dat$Q_cms
dat$SiteID <- factor(dat$SiteID)

table(dat$SiteID)

ggplot(subset(dat, dat$SiteID == "USGS-09041400"))+
  geom_line(mapping = aes(Date, Spc_Qcms))

getwd()
saveRDS(dat, "all_SC_Q_data.rds")
