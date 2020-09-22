x <- c("sf", "rgdal", "raster", "tidyverse", "nhdplusTools")
lapply(x, require, character.only = TRUE)

## Bring in all necessary data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("all_SC_data.rds")
USGS <- subset(dat, dat$Source == "USGS")
WQP <- subset(dat, dat$Source == "WQP")

setwd("/Volumes/Blaszczak Lab/FSS/WQP Data/WQP Formatted Meta")
WQP_meta <- read.csv("WQP_location_data_NAD83.csv")

setwd("/Volumes/Blaszczak Lab/FSS/NHD/USGS_Streamgages-NHD_Locations_Shape")
NHD <- st_read("USGS_Streamgages-NHD_Locations.shp")

## Format dataframes so they will work for the function we're going to create
NHD$SITE_NO <- paste0("USGS-", NHD$SITE_NO)
colnames(NHD)[3] <- "SiteID"
NHD <- subset(NHD, NHD$SiteID %in% USGS$SiteID)
not_indexed <- setdiff(USGS$SiteID, NHD$SiteID) # we will have to do these separately
USGS <- subset(USGS, USGS$SiteID %in% NHD$SiteID)

# for multiple sites (all USGS sites)
USGS$SiteID <- factor(USGS$SiteID)
USGS_sites <- levels(USGS$SiteID)
USGS_sites <- as.data.frame(USGS_sites)
USGS_sites$COMID <- 1
colnames(USGS_sites)[1] <- "SiteID"

# ## Using USGS SiteID:
# # for 1 site
# nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-10351700")
# discover_nhdplus_id(nldi_feature = nldi_nwis)

## The following does not work for a lot of locations:
# findCOMID <- function(x){
#   nldi_nwis <- list(featureSource = "nwissite", featureID = USGS_sites$SiteID[x])
#   USGS_sites$COMID[x] <<- discover_nhdplus_id(nldi_feature = nldi_nwis)
# 
# }
# 
# rows <- as.list(1:159)
# 
# for (i in (1:length(rows))){
#   findCOMID(i)
# }
# 
# lapply(rows, findCOMID)

## Use this method instead:
# Using coordinates and crs:
# NAD 83 EPSG = 4269

point <- sf::st_sfc(sf::st_point(c(-106.08817291, 40.08348465)), crs = 4326)
discover_nhdplus_id(point)

# need to add USGS- to each site ID in the streamgage location df

findCOMID <- function(x){ # x = USGS SiteID
# x <- 'USGS-09041400'
# rm(x)
  point <- st_sfc(st_point(c((NHD$LON_NHD[which(NHD$SiteID == x)]), (NHD$LAT_NHD[which(NHD$SiteID == x)]))), crs = 4269)
  USGS_sites$COMID[which(USGS_sites$SiteID == x)] <<- discover_nhdplus_id(point)
  
}

findCOMID('USGS-09041400')

lapply(USGS_sites$SiteID, findCOMID)
