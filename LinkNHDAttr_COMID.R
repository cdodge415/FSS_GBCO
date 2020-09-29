x <- c("sf", "rgdal", "raster", "tidyverse", "nhdplusTools")
lapply(x, require, character.only = TRUE)
rm(x)

## Bring in all necessary data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("all_SC_data.rds")
USGS <- subset(dat, dat$Source == "USGS")
WQP <- subset(dat, dat$Source == "WQP")

setwd("/Volumes/Blaszczak Lab/FSS/WQP Data/WQP Formatted Meta")
WQP_meta <- read.csv("WQP_location_data_NAD83.csv")

setwd("/Volumes/Blaszczak Lab/FSS/NHD/USGS_Streamgages-NHD_Locations_Shape")
NHD <- st_read("USGS_Streamgages-NHD_Locations.shp")

setwd("/Users/laurenbolotin/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
USGS_metadata <- readRDS("GBCO_SC_sites.rds")

## Format dataframes so they will work for the function we're going to create
NHD$SITE_NO <- paste0("USGS-", NHD$SITE_NO)
colnames(NHD)[3] <- "SiteID"
NHD <- subset(NHD, NHD$SiteID %in% USGS$SiteID)
not_indexed <- setdiff(USGS$SiteID, NHD$SiteID) # we will have to do these separately
USGS <- subset(USGS, USGS$SiteID %in% NHD$SiteID)

USGS_metadata$Site_ID <- ifelse(USGS_metadata$Site_ID < 10000000, paste0("0", USGS_metadata$Site_ID), paste0(USGS_metadata$Site_ID))
USGS_metadata$Site_ID <- paste0("USGS-", USGS_metadata$Site_ID)
colnames(USGS_metadata)[1] <- "SiteID"

# for multiple sites (all USGS sites)
USGS$SiteID <- factor(USGS$SiteID)
USGS_sites <- levels(USGS$SiteID)
USGS_sites <- as.data.frame(USGS_sites)
USGS_sites$COMID <- 1
colnames(USGS_sites)[1] <- "SiteID"

## USGS Sites:
# Using coordinates and crs:
# NAD 83 EPSG = 4269

point <- sf::st_sfc(sf::st_point(c(-106.08817291, 40.08348465)), crs = 4326)
discover_nhdplus_id(point)

findCOMID <- function(x){ # x = USGS SiteID
# x <- 'USGS-09041400'
# rm(x)
  point <- st_sfc(st_point(c((NHD$LON_NHD[which(NHD$SiteID == x)]), (NHD$LAT_NHD[which(NHD$SiteID == x)]))), crs = 4269)
  USGS_sites$COMID[which(USGS_sites$SiteID == x)] <<- discover_nhdplus_id(point)
  
}

findCOMID('USGS-09041400')

lapply(USGS_sites$SiteID, findCOMID)

not_indexed <- as.data.frame(not_indexed)
not_indexed$COMID <- ""
colnames(not_indexed)[1] <- "SiteID"

findCOMID <- function(x){
  tryCatch((nldi_nwis <- list(featureSource = "nwissite", featureID = not_indexed$SiteID[x])), error = function(e) NULL)
  tryCatch((not_indexed$COMID[x] <<- discover_nhdplus_id(nldi_feature = nldi_nwis)), error = function(e) NULL)

}

not_indexed_list <- as.list(not_indexed$SiteID)
# not_indexed_list <- seq(12, 15)

lapply(not_indexed_list, findCOMID)

newly_indexed <- subset(not_indexed, not_indexed$COMID != "")
not_indexed <- setdiff(not_indexed$SiteID, newly_indexed$SiteID)
USGS_sites <- rbind(USGS_sites, newly_indexed)

USGS_metadata <- subset(USGS_metadata, USGS_metadata$SiteID %in% not_indexed)
USGS_metadata <- unique(USGS_metadata)

not_indexed <- as.data.frame(not_indexed)
not_indexed$COMID <- ""
colnames(not_indexed)[1] <- "SiteID"

findCOMID <- function(x){ # x = USGS SiteID
  # x <- 'USGS-09041400'
  # rm(x)
  point <- st_sfc(st_point(c((USGS_metadata$Lon[which(USGS_metadata$SiteID == x)]), (USGS_metadata$Lat[which(USGS_metadata$SiteID == x)]))), crs = 4269)
  not_indexed$COMID[which(not_indexed$SiteID == x)] <<- discover_nhdplus_id(point)
  
}

lapply(not_indexed$SiteID, findCOMID)

USGS_sites <- rbind(USGS_sites, not_indexed)
 # we're back at 159 sites

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(USGS_sites, "USGS_SC_ComID.rds")

rm(newly_indexed, not_indexed, not_indexed_list, USGS_metadata, USGS)

## WQP Sites:
head(WQP_meta)
WQP_meta <- select(WQP_meta, c("X", "Lon_NAD83", "Lat_NAD83"))
colnames(WQP_meta)[1] <- "SiteID"

WQP_sites <- WQP_meta$SiteID
WQP_sites <- as.data.frame(WQP_sites)
WQP_sites$COMID <- ""
colnames(WQP_sites)[1] <- "SiteID"

findCOMID <- function(x){ # x = USGS SiteID
  # x <- 'USGS-09041400'
  # rm(x)
  
  tryCatch(point <- st_sfc(st_point(c((WQP_meta$Lon_NAD83[which(WQP_meta$SiteID == x)]), 
              (WQP_meta$Lat_NAD83[which(WQP_meta$SiteID == x)]))), crs = 4269), error = function(e) NULL)
  tryCatch((WQP_sites$COMID[which(WQP_sites$SiteID == x)] <<- discover_nhdplus_id(point)),
           error = function(e) NULL)
  
}

lapply(WQP_sites$SiteID, findCOMID) # < 2 hrs
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(WQP_sites, "WQP_SC_ComID.rds")

na <- WQP_sites[is.na(WQP_sites$COMID),]



## Link data with NLCD data via ComID (Wieczorek & Schwarz 2019)
# metadata: https://www.sciencebase.gov/catalog/file/get/5d66b3b6e4b0c4f70cefb11d?f=__disk__96%2F19%2Fa6%2F9619a658ff38aaa0762d0ed54dd9656d2f52546c&transform=1&allowOpen=true
## CAT = catchment
setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD16_CAT_CONUS")
nhd_attr <- read.csv("NLCD16_CAT_CONUS.csv")
head(nhd_attr)
# USGS
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
usgs_comid <- readRDS("USGS_SC_ComID.rds")
usgs_comid$COMID <- as.factor(as.character(usgs_comid$COMID))
levels(usgs_comid$COMID)

nhd_attr_sub <- subset(nhd_attr, nhd_attr$COMID %in% usgs_comid$COMID)
head(nhd_attr_sub)
usgs_nlcd <- merge(usgs_comid, nhd_attr_sub, by = "COMID")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_nlcd, "USGS_SC_NLCD_cat.rds")
# WQP
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
wqp_comid <- readRDS("WQP_SC_ComID.rds")
wqp_comid$COMID <- as.factor(as.character(wqp_comid$COMID))
levels(wqp_comid$COMID)

nhd_attr_sub <- subset(nhd_attr, nhd_attr$COMID %in% wqp_comid$COMID)
wqp_nlcd <- merge(wqp_comid, nhd_attr_sub, by = "COMID")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(wqp_nlcd, "WQP_SC_NLCD_cat.rds")

## ACC = accumulated upstream percent area of NHDPlus version 2 flowline catchments
setwd("/Volumes/Blaszczak Lab/FSS/NLCD")
nhd_attr <- read.csv("NLCD16_ACC_CONUS.csv")
head(nhd_attr)

# USGS
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
nhd_attr_sub <- subset(nhd_attr, nhd_attr$COMID %in% usgs_comid$COMID)
head(nhd_attr_sub)
usgs_nlcd <- merge(usgs_comid, nhd_attr_sub, by = "COMID")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_nlcd, "USGS_SC_NLCD_acc.rds")
# WQP
nhd_attr_sub <- subset(nhd_attr, nhd_attr$COMID %in% wqp_comid$COMID)
wqp_nlcd <- merge(wqp_comid, nhd_attr_sub, by = "COMID")
saveRDS(wqp_nlcd, "WQP_SC_NLCD_acc.rds")



# Now link Generalized Geology Type (Reed and Bush 2001 via Wieczorek & Schwarz 2019)
# metadata: https://www.sciencebase.gov/catalog/file/get/5703f6b5e4b0328dcb826d06?f=__disk__87%2F59%2Fac%2F8759ac764197b53a525dbad8270a975b35b27b07&transform=1&allowOpen=true
## ACC
# USGS
setwd("/Volumes/Blaszczak Lab/FSS/Geology")
geol <- read.csv("BUSHREED_ACC_CONUS.csv")
geol_sub <- subset(geol, geol$COMID %in% usgs_comid$COMID)
usgs_geol <- merge(usgs_comid, geol_sub, by = "COMID")
colnames(usgs_geol)
colnames(usgs_geol) <- c("COMID", "SiteID", "Gneiss", "Granitic", "Ultramafic", "Quarternary",
                         "Sedimentary", "Volcanic", "Water", "Anorthositic", "Intermediate", "NO_DATA")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_geol, "USGS_SC_Geology_acc.rds")
# WQP
geol_sub <- subset(geol, geol$COMID %in% wqp_comid$COMID)
wqp_geol <- merge(wqp_comid, geol_sub, by = "COMID")
colnames(wqp_geol)
colnames(wqp_geol) <- c("COMID", "SiteID", "Gneiss", "Granitic", "Ultramafic", "Quarternary",
                         "Sedimentary", "Volcanic", "Water", "Anorthositic", "Intermediate", "NO_DATA")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(wqp_geol, "WQP_SC_Geology_acc.rds")
## CAT
# USGS
setwd("/Volumes/Blaszczak Lab/FSS/Geology")
geol <- read.csv("BUSHREED_CAT_CONUS.csv")
geol_sub <- subset(geol, geol$COMID %in% usgs_comid$COMID)
usgs_geol <- merge(usgs_comid, geol_sub, by = "COMID")
colnames(usgs_geol)
colnames(usgs_geol) <- c("COMID", "SiteID", "Gneiss", "Granitic", "Ultramafic", "Quarternary",
                         "Sedimentary", "Volcanic", "Water", "Anorthositic", "Intermediate", "NO_DATA")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_geol, "USGS_SC_Geology_cat.rds")
# WQP
geol_sub <- subset(geol, geol$COMID %in% wqp_comid$COMID)
wqp_geol <- merge(wqp_comid, geol_sub, by = "COMID")
colnames(wqp_geol)
colnames(wqp_geol) <- c("COMID", "SiteID", "Gneiss", "Granitic", "Ultramafic", "Quarternary",
                        "Sedimentary", "Volcanic", "Water", "Anorthositic", "Intermediate", "NO_DATA")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(wqp_geol, "WQP_SC_Geology_cat.rds")
