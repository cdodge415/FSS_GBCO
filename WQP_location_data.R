library(raster)
library(sp)
library(rgdal)
# This is done based solely on SC data
# Bring in WQP site data
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta")
WQP <- read.csv("WQP_formatted_metadata_WQ.csv", header = T) # 32,722 WQP sites with flowing waters
# Bring in finalized sites of interest for SC
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/Finalized TS (All Sources)")
sites <- readRDS("SCd.RDS")
# subset WQP site data only for the sites used in analysis of SC
dat <- subset(WQP, WQP$SiteID %in% sites$SiteID) # 25616 sites (probably because this is only SC data). We will also end up reducing the sites in the data, because they have not yet been filtered for only flowing waters.
# We're missing 1 USGS site
#dat <- WQP
rm(sites, WQP)

# Check coordinates
# Remove coordinates with 0 or not within bounds
nrow(subset(dat, Latitude > 90 & Latitude < -90)) ## 0
nrow(subset(dat, Longitude > 180 & Longitude < -180)) ## 0

## Double check if coordinates are rounded off to no decimal places
sub <- dat[,c("SiteID","Latitude","Longitude")]
sub$Latitude <- abs(sub$Latitude)
ctest_lat <- sub %>% separate(Latitude, c("Num","Dec"))
ctest_lat[is.na(ctest_lat$Num),];ctest_lat[is.na(ctest_lat$Dec),] 
# Remove any coordinates that are rounded off
dat <- dat[-which(dat$SiteID %in% ctest_lat[is.na(ctest_lat$Dec),]$SiteID),] ## went from 25616 to 25602

ctest_long <- sub %>% separate(Longitude, c("Num","Dec"))
ctest_long[is.na(ctest_long$Num),]; ctest_long[is.na(ctest_long$Dec),] ## 0

############################################
## Fix WQP coordinate issues
###########################################
## Adjust or remove specific sites that are still problematic (see GRDO_check_WQP_coords)
# says Longitude: -17.74332
dat <- dat[-which(dat$SiteID == "NFRIA-North Fork Below Leroux Creek"),]
# 25,601 sites
#######################################
## Check Coord_Units
#######################################
## Check geodetic datum (Coord_Units)
class(dat$Coord_Units)
dat$Coord_Units <- factor(dat$Coord_Units)
levels(as.factor(dat$Coord_Units))
length(dat$Coord_Units[dat$Coord_Units == "WGS84"])
# "NAD27" 3302
# "NAD83" 19535
# "Unknown" 2
# "UNKWN" 2118
# "WGS84" 644

## Converting WQP sites to WGS1984 if unclear based on count above
dat$Coord_Units <- as.character(dat$Coord_Units)
dat$Coord_Units <- as.factor(dat$Coord_Units)
levels(dat$Coord_Units) <- c(levels(dat$Coord_Units), "WGS1984")
dat[which(dat$Coord_Units == "UNKWN"),]$Coord_Units <- "WGS1984"
dat[which(dat$Coord_Units == "Unknown"),]$Coord_Units <- "WGS1984"
dat[which(dat$Coord_Units == "WGS84"),]$Coord_Units <- "WGS1984"
## Leaving NAD27, NAD83, WGS72 for proper conversion to WGS1984

## Subset sites based on CRS of original data and reproject to WGS84:
# NAD83 sites:
dat.NAD83sub.projectWGS84 <- dat[which(dat$Coord_Units=="NAD83"),] %>%
  # convert to spatial object:
  st_as_sf(coords=c("Longitude","Latitude"),crs=4269) %>%
  # project to WGS84: 
  st_transform(.,4326) 

# WGS84 sites:
dat.WGS84sub <- dat[which(dat$Coord_Units=="WGS1984"),] %>%
  # convert to spatial object:
  st_as_sf(coords=c("Longitude","Latitude"),crs=4326) 

# NAD27 sites:
# note that I've noticed differences between machines in st_transform results from NAD27 only. If no shift is observed, try lwgeom::st_transform_proj:
dat.NAD27sub.projectWGS84 <- dat[which(dat$Coord_Units=="NAD27"),] %>%
  # convert to spatial object:
  st_as_sf(coords=c("Longitude","Latitude"),crs=4267) %>%
  # project to WGS84: 
  st_transform(.,4326)

## Merge data subsets back into one dataset (all CRS = WGS84)
dat.merged <- rbind(dat.NAD27sub.projectWGS84,
                            dat.NAD83sub.projectWGS84,
                            dat.WGS84sub) 
# NEXT LINE ONLY FOR WGS84 FINAL PRODUCT, NOT NAD83
dat.merged <- dat.merged %>% mutate("Lat_WGS84" = st_coordinates(.)[,2],
                                    "Lon_WGS84" = st_coordinates(.)[,1])

## Check that crs reflects WGS84 and plot data:                                                     
st_crs(dat.merged)

## Compare number of sites in merged dataset with original data:
length(dat$SiteID) == length(dat.merged$SiteID)

###########
## Export
###########
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta")
write.csv(dat.merged, "WQP_location_data_WGS84.csv")

# Now that we have everything in WGS84, we actually want to put it all in NAD83 to match USGS data
dat.WGS84.projectNAD83 <- dat.merged %>%
  # convert to spatial object:
  st_as_sf(coords=c("Longitude","Latitude"),crs=4326) %>%
  # project to WGS84: 
  st_transform(.,4269) 

dat.WGS84.projectNAD83 <- dat.WGS84.projectNAD83 %>% mutate("Lat_NAD83" = st_coordinates(.)[,2],
                                    "Lon_NAD83" = st_coordinates(.)[,1])

setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta")
write.csv(dat.WGS84.projectNAD83, "WQP_location_data_NAD83.csv")
