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

Q <- select(Q, c("SiteID", "Date", "Q_cfs", "Q_cms","SiteDate"))
Q$Source <- "USGS"

Q_sd <- as.list(Q$SiteDate)
SC_sd <- as.list(SC$SiteDate)

dat <- merge(SC, Q, by = c("SiteID", "Date", "SiteDate"))
