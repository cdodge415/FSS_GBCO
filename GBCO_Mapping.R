library(sf)
library(nhdplusTools)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(FedData)
library(usmap)

# GBCO SITE DATA ###
# bring in WQP location data that has all been transformed to NAD 83: 
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta")
WQP <- read.csv("WQP_location_data_NAD83.csv") # This data has already been filtered for SC, flowing water, and no rounded coordinates
# bring in USGS location data (already in NAD 83):
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
USGS <- read.csv("USGS_SCd_site_data.csv")
sapply(USGS, class)
USGS$Site_ID <- as.factor(as.numeric(USGS$Site_ID))

# get both datasets into similar format:
colnames(WQP)
colnames(USGS)
WQP <- dplyr::select(WQP, c("X", "SiteID","Lat_NAD83", "Lon_NAD83"))
WQP$huc_cd <- "" # we can fix this later
colnames(WQP)
colnames(WQP) <- c("SiteID", "station_nm", "Lat", "Lon", "huc_cd")
USGS <- dplyr::select(USGS, -c("X"))
colnames(USGS)[1] <- "SiteID"
USGS$SiteID <- paste0("USGS-", USGS$SiteID)

WQP$DB_ID <- "WQP"
USGS$DB_ID <- "USGS"

# make sure we don't have duplicated data
overlap <- lubridate::intersect(WQP$SiteID, USGS$SiteID)
WQP <- WQP[-which(WQP$SiteID %in% overlap),]
rm(overlap)

# combine data
dat <- rbind(USGS, WQP, deparse.level = 1)
rm(USGS, WQP)


# Set up basemap with blank states
some.states <- c('california', 'nevada', 'utah', 'wyoming', 'colorado', 'arizona', 'new mexico', 'idaho', 'oregon')
some.states.map <- map_data("state", region = some.states)

# Set the theme for our map plots
theme_set(
  theme(panel.background = element_blank(),axis.title = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), axis.text = element_blank(), plot.title = element_text(hjust = 0.75))
)

# Basemap
ggplot(some.states.map)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "grey", color = "white")+
  geom_point(dat, mapping = aes(x = Lon, y = Lat), size = 0.25)

# bring in Watershed Boundary Dataset (WBD) data:
# I downloaded the WBD by 2 digit HUC to my computer
# I am having issues accessing some web services e.g. downloading using nhdplustools to bring in shapefiles
basin14 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1606_HU4_Shape/WBDHU2.shp")
basin15 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1505_HU4_Shape/WBDHU2.shp")
basin16 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1407_HU4_Shape/WBDHU2.shp")

# view metadata:
st_geometry_type(basin14)
st_crs(basin14)
st_bbox(basin14)
basin14
  
## FLOWLINES
# I downloaded the NHDPlus by 4 digit HUC to my computer
# HUC 14
flowlines14_1 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1401_HU4_Shape/NHDFlowline.shp")
class(flowlines14_1)
st_geometry_type(flowlines14_1)
st_crs(flowlines14_1)
st_bbox(flowlines14)
flowlines14_2 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1402_HU4_Shape/NHDFlowline.shp")
flowlines14_3 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1403_HU4_Shape/NHDFlowline.shp")
flowlines14_4<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1404_HU4_Shape/NHDFlowline.shp")
flowlines14_5<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1405_HU4_Shape/NHDFlowline.shp")
flowlines14_6<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1406_HU4_Shape/NHDFlowline.shp")
flowlines14_7<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1407_HU4_Shape/NHDFlowline.shp")
flowlines14_8<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1408_HU4_Shape/NHDFlowline.shp")
class(flowlines14_1)
flowlines14 <- rbind_list(flowlines14_1, flowlines14_2, flowlines14_3, flowlines14_4, flowlines14_5, flowlines14_6, flowlines14_7, flowlines14_8)
class(flowlines14)
flowlines14 <- st_as_sf(flowlines14)
st_crs(flowlines14) <- 4269
st_crs(flowlines14)
rm(flowlines14_1, flowlines14_2, flowlines14_3, flowlines14_4, flowlines14_5, flowlines14_6, flowlines14_7, flowlines14_8)
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD")
st_write(flowlines14, "HUC14_flowlines.shp")

# HUC 15
flowlines15_1 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1501_HU4_Shape/NHDFlowline.shp")
class(flowlines15_1)
st_geometry_type(flowlines15_1)
st_crs(flowlines15_1)
st_bbox(flowlines15_1)
flowlines15_2 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1502_HU4_Shape/NHDFlowline.shp")
flowlines15_3 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1503_HU4_Shape/NHDFlowline.shp")
flowlines15_4<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1504_HU4_Shape/NHDFlowline.shp")
flowlines15_5<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1505_HU4_Shape/NHDFlowline.shp")
flowlines15_6<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1506_HU4_Shape/NHDFlowline.shp")
flowlines15_7<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1507_HU4_Shape/NHDFlowline.shp")
flowlines15_8<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1508_HU4_Shape/NHDFlowline.shp")
# Combine all flowline sf's into one sf
flowlines15 <- rbind_list(flowlines15_1, flowlines15_2, flowlines15_3, flowlines15_4, flowlines15_5, flowlines15_6, flowlines15_7, flowlines15_8)
class(flowlines15)
flowlines15 <- st_as_sf(flowlines15)
# Set coordinate reference system and confirm that it worked:
st_crs(flowlines15) <- 4269
st_crs(flowlines15)
rm(flowlines15_1, flowlines15_2, flowlines15_3, flowlines15_4, flowlines15_5, flowlines15_6, flowlines15_7, flowlines15_8)  
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD")
st_write(flowlines15, "HUC15_flowlines.shp")

# HUC 16
flowlines16_1 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1601_HU4_Shape/NHDFlowline.shp")
class(flowlines16_1)
st_geometry_type(flowlines16_1)
st_crs(flowlines16_1)
st_bbox(flowlines16_1)
flowlines16_2 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1602_HU4_Shape/NHDFlowline.shp")
flowlines16_3 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1603_HU4_Shape/NHDFlowline.shp")
flowlines16_4<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1604_HU4_Shape/NHDFlowline.shp")
flowlines16_5<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1605_HU4_Shape/NHDFlowline.shp")
flowlines16_6<- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/NHD_H_1606_HU4_Shape/NHDFlowline.shp")
# Combine all flowline sf's into one sf
flowlines16 <- rbind_list(flowlines16_1, flowlines16_2, flowlines16_3, flowlines16_4, flowlines16_5, flowlines16_6)
class(flowlines16)
flowlines16 <- st_as_sf(flowlines16)
# Set coordinate reference system and confirm that it worked:
st_crs(flowlines16) <- 4269
st_crs(flowlines16)
rm(flowlines16_1, flowlines16_2, flowlines16_3, flowlines16_4, flowlines16_5, flowlines16_6)  
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD")
st_write(flowlines16, "HUC16_flowlines.shp")

# MAP with all elements: ####
ggplot(some.states.map)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "grey", color = "white")+
  labs(title = "Specific Conductance Sites")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(dat, mapping = aes(x = Lon, y = Lat), size = 0.25, color = "turquoise4", alpha = 0.6)+
  geom_sf(data = basin14, size = 0.25, color = "indianred1", fill = NA) +
  geom_sf(data = basin15, size = 0.25, color = "indianred1", fill = NA) +
  geom_sf(data = basin16, size = 0.25, color = "indianred1", fill = NA)+
  # geom_sf(data = filter(flowlines14, flowlines14$GNIS_Name == "Colorado River"), size = 0.05, color = "cyan")+ # we can filter for specific streams/rivers this way
  # geom_sf(data = flowlines14, size = 0.05, color = "blue")+
  # geom_sf(data = flowlines15, size = 0.05, color = "blue")+
  # geom_sf(data = flowlines16, size = 0.05, color = "blue")+
  coord_sf(crs = 4269)










# FIELD SITES ###
# This is the method I can't get to work....code is probably messy and any other attempts should start from scratch with an online tutorial/vignette
# temp_dir <- "~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta"
# nwissite <- list(featureSource = "nwissite", 
#                  featureID = "USGS-05428500")
# 
# flowline <- navigate_nldi(nwissite, 
#                           mode = "upstreamTributaries", 
#                           data_source = "")
# 
# nhdplus <- subset_nhdplus(comids = flowline$nhdplus_comid,
#                           output_file = file.path(temp_dir, "nhdplus.gpkg"),
#                           nhdplus_data = "download",
#                           overwrite = TRUE, return_data = FALSE)
# 
# flowline <- read_sf(nhdplus, "NHDFlowline_Network")
# 
# upstream_nwis <- navigate_nldi(nwissite,
#                                mode = "upstreamTributaries",
#                                data_source = "nwissite")