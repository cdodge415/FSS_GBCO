# streamgage matching for SC and Q gages-FSS project
# 10/5/20- Chloe Dodge
# install.packages("nhdplusTools")
library(nhdplusTools)
library(sf)
library(dplyr)
library(tidyverse)
#set working directory
setwd("C:/Users/cdodg/Desktop/Blaszczak_lab/FSS_project/Streamgage_matching")
#read data
all_SC_data <- readRDS("all_SC_data.rds")
disch_query <- readRDS("disch_query.rds")
SC_query <- readRDS("SC_query.rds")
WQP_location_data <- readRDS("WQP_location_data_NAD83.rds")
#create Site_ID dataframe from all_SC_data
Site_ID <- as.list(levels(all_SC_data$SiteID))
all_Sites <- as.data.frame(do.call(cbind, Site_ID))
all_Sites <- as.data.frame(t(all_Sites))
#filter WQP location data and discharge location data with SiteID
WQP_sites <- WQP_location_data %>% filter(SiteID %in% all_Sites[,1])
WQP_sites_final <- subset(WQP_sites,select = c(SiteID, HUCEightDigitCode, Lat_NAD83, Lon_NAD83))
disch_query$SiteID = paste0(disch_query$agency_cd, "-", disch_query$Site_ID)
disch_sites <- disch_query %>% filter(SiteID %in% all_Sites[,1])
disch_sites_final <- subset(disch_sites,select = c(SiteID, Lat, Lon, huc_cd))
colnames(disch_sites_final)
colnames(WQP_sites_final)
all_disch_sites <- disch_sites_final %>% rename(huc = huc_cd)
#all_WQP_sites <- WQP_sites_final %>% rename(huc = HUCEightDigitCode, Lat = Lat_NAD83, Lon = Lon_NAD83)
# the line above was not working so I did this instead:
all_WQP_sites <- WQP_sites_final
colnames(all_WQP_sites) <- c("SiteID", "huc", "Lat", "Lon", "geometry")
class(all_WQP_sites) # currently an sf dataframe, needs to be changed to regular dataframe
all_WQP_sites <- as.data.frame(all_WQP_sites)
all_WQP_sites <- dplyr::select(all_WQP_sites, -c("geometry")) # get rid of this column
# compare the column names for these two dfs:
colnames(all_disch_sites)
colnames(all_WQP_sites)
# reorder the columns for one to match the other:
all_WQP_sites <- dplyr::select(all_WQP_sites, c("SiteID", "Lat", "Lon", "huc"))
#remove duplicates in disch_sites_final
final_disch_sites <- unique(all_disch_sites)
final_WQP_sites <- unique(all_WQP_sites)
# combine these two
all_data <- rbind(final_disch_sites, final_WQP_sites, deparse.level = 1)
# get rid of any sites that are in there twice because they came from BOTH usgs and wqp
all_data <- all_data[!duplicated(all_data$SiteID),]
all_data$SiteID <- factor(all_data$SiteID)
levels(all_data$SiteID) # the number of levels (different SiteIDs) is the same as the number of rows, which means we did what we needed to do!
