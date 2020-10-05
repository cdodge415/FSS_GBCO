setwd("/Volumes/Blaszczak Lab/FSS/Literature")
# bring in subset of Appendix 1 Table 1-6 from the Streamgage Matching USGS doc. made in excel
info <- read.csv("Table1-6_subset.csv")
class(info$State)
levels(info$State)
# subset for potential GBCO sites
info <- subset(info, info$State %in% states)
states <- c("AZ", "CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WY")
info$State <- factor(info$State)
levels(info$State)
# write subset to new csv
setwd("/Volumes/Blaszczak Lab/FSS/Analysis/Streamgage_matching")
write.csv(info, "Streamgage_Matches_subset.csv")

# once sites are FINALIZED we can see which of those sites are already matched here and can either 
# have their matches used, or need new matches

library(sf)
library(nhdplusTools)
points <-  st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1606_HU4_Shape/NHDPoint.shp")
points <- subset(points, points$FCode == 36701)
# 1: geometry = c(-114.529638900375, 39.216577779753, 0) 

# get ComID from geometry
point <- sf::st_sfc(sf::st_point(c(-114.529638900375, 39.216577779753)), crs = 4326)
discover_nhdplus_id(point)

# based on maps.waterdata.usgs.gov and the above coordinates, I think this is USGS-10243700
# get ComID from featureID
nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-10243700")
discover_nhdplus_id(nldi_feature = nldi_nwis)
# both options give the same ComID


# the next closest gage to the one used above based on maps.waterdata.usgs.gov is 10244950
nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-10244950")
discover_nhdplus_id(nldi_feature = nldi_nwis)

# the coordinates of 10244950 according to the Location Map for this site are:
point <- sf::st_sfc(sf::st_point(c(-114.412098, 39.120554)), crs = 4326)
discover_nhdplus_id(point)
# but this produces a different ComID than using the featureID does

#-------------------------------------------------------------------

# another row from our points df
point <- sf::st_sfc(sf::st_point(c(-116.464444400497, 39.769777779936)), crs = 4326)
discover_nhdplus_id(point)
# appears to be this site:
nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-10245960")
discover_nhdplus_id(nldi_feature = nldi_nwis)



# > # Bring in list of sites with continuous SC data
#   > setwd("/Users/laurenbolotin/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta")
# > continuous <- read.csv("WQ_USGS_SCcontinuous_site_list.csv")
# > View(continuous)
# > class(continuous$SiteID)
# [1] "character"
# > continuous$SiteID <- as.factor(continuous$SiteID)
# > sites <- levels(continuous$SiteID)
# > sites <- as.data.frame(sites)
# > View(sites)
# > setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# > saveRDS(sites, "continuous_sites_usgs.rds")


library(nhdplusTools)
library(sf)
library(dplyr)

# start_point <- st_sfc(st_point(c(-89.362239, 43.090266)), crs = 4269)
# start_comid <- discover_nhdplus_id(start_point)
start_comid <- "UTAHDWQ_WQX-4941100"

flowline <- navigate_nldi(list(featureSource = "WQP",
                               featureID = start_comid),
                          mode = "upstreamTributaries",
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = flowline$nhdplus_comid,
                         output_file = subset_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE)


flowline <- subset$NHDFlowline_Network
# catchment <- subset$CatchmentSP
# waterbody <- subset$NHDWaterbody

plot(flowline)
colnames(flowline)
flowline <- select(flowline, c("comid", "lengthkm", "fdate","reachcode", "ftype", "fcode", "streamorde", "gnis_name","levelpathi", "totdasqkm"))

# see if streamgage has same ComID as the Truckee River
nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-10346000")
discover_nhdplus_id(nldi_feature = nldi_nwis)








