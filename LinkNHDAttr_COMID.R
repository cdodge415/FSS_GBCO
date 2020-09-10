x <- c("sf", "rgdal", "raster", "tidyverse", "nhdplusTools")
lapply(x, require, character.only = TRUE)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("all_SC_data.rds")
USGS <- subset(dat, dat$Source == "USGS")
WQP <- subset(dat, dat$Source == "WQP")
setwd("/Volumes/Blaszczak Lab/FSS/WQP Data/WQP Formatted Meta")
WQP_meta <- read.csv("WQP_location_data_NAD83.csv")

## For a USGS site:
# for 1 site
nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-09034500")
discover_nhdplus_id(nldi_feature = nldi_nwis)
# for multiple sites (all USGS sites)
USGS$SiteID <- factor(USGS$SiteID)
USGS_sites <- levels(USGS$SiteID)
USGS_sites <- as.data.frame(USGS_sites)
USGS_sites$COMID <- 1
colnames(USGS_sites)[1] <- "SiteID"


findCOMID <- function(x){
  nldi_nwis <- list(featureSource = "nwissite", featureID = USGS_sites$SiteID[x])
  USGS_sites$COMID[x] <<- discover_nhdplus_id(nldi_feature = nldi_nwis)
  
}

rows <- as.list(1:159)

for (i in (1:length(rows))){
  findCOMID(i)
}

lapply(rows, findCOMID)




# For a non USGS site:
point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
discover_nhdplus_id(point)

findCOMID <- function(x){
  point <- st_sfc(st_point(c(-76.87479, 39.48233)), crs = 4326)
  discover_nhdplus_id(point)
}