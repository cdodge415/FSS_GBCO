setwd("/Volumes/Blaszczak Lab/FSS/NHD")
library(FedData)
library(sf)
library(raster)
library(sp)
library(rgdal)
library(fasterize)
library(data.table)
library(dplyr)
library(ggplot2)

##################################################
# Procedure: 
# 1) bring in shapefiles
# 2) transform shapefiles to same crs as NLCD data
# 3) make shapefiles Spatial Polygons Data Frame
# 4) crop raster to shapefile
# 5) mask raster by shapefile
##################################################

# The following lines bring in NLCD data and clip it to the total area of the Great Basin and
# Colorado River Basin watersheds. We only needed to do this once and output a new raster.
# Clip NLCD Data to Study Area ####
# # Read in watershed shapefile
# basin14 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1401_HU4_Shape/WBDHU2.shp")
# basin15 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1501_HU4_Shape/WBDHU2.shp")
# basin16 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1606_HU4_Shape/WBDHU2.shp")
#
# # Read in NLCD raster and learn some details about it
# setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD_2016_Land_Cover_L48_20190424 (1)") # This is the full dataset for the CONUS
# nlcd <- raster("NLCD_2016_Land_Cover_L48_20190424.img")
# nlcd
# GDALinfo("NLCD_2016_Land_Cover_L48_20190424.img")
# View(nlcd@data@attributes[[1]]) # This is how we can actually look at the land use data attribute table
# 
# # Transform watershed to same projection as NLCD data and convert to polygon
# class(basin14)
# #st_crs(basin14) <- " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
# basin14 <- st_transform(basin14, crs = " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
# basin14_sp <- as(basin14, Class = "Spatial")
# # or: #basin14_sp <- sf::as_Spatial(basin14)
# class(basin14_sp)

# basin15 <- st_transform(basin15, crs = " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
# basin15_sp <- as(basin15, Class = "Spatial")
# basin16 <- st_transform(basin16, crs = " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
# basin16_sp <- as(basin16, Class = "Spatial")
# 
# # See if you can combine all basins into 1
# basins <- rbind(basin14, basin15)
# basins <- rbind(basins, basin16)
# basins
# basins_sp <- as(basins, Class = "Spatial")
# 
# # Try for all basins together
# nlcd_c <- crop(nlcd, basins_sp)
# plot(nlcd_c)
# nlcd_m <- mask(nlcd_c, basins_sp)
# plot(nlcd_m) # this takes a while, 10+ mins

# writeRaster(nlcd_m, "GBCO_NLCD_masked_raster.img") # New raster file for NLCD clipped to all 3 HUC 2 basins together
# writeRaster(nlcd_m, "GBCO_NLCD_masked_raster.tif") # Try saving with a different format.
#                                                    # Using .img resulted in the @data@attributes being empty when the file was brought in later
# nlcd2 <- raster("GBCO_NLCD_masked_raster.tif") # Saving as a .tif worked
# rm(nlcd2)

# Clip NLCD Data to HUC8 Watersheds ####
# Create a function to do this for each of the smaller watersheds, I'm thinking HUC 8 since that is the level of HUC offered by our data
setwd("/Volumes/Blaszczak Lab/FSS/NHD")
files <- list.files(path = '/Volumes/Blaszczak Lab/FSS/NHD', pattern = "NHD_H_")
class(files)
files <- as.list(files)
# Just get it to work for 1 HUC8 watershed: 
huc8_1 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[1], "/WBDHU8.shp"))
huc8_1
# class(huc8_1) # "sf" "data.frame"
huc8_1 <- st_transform(huc8_1, crs = " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
huc8_1_sp <- as(huc8_1, Class = "Spatial")

class(huc8_1_sp) # "SpatialPolygonsDataFrame:"attr(,"package") "sp"
huc8_1_split <- split(huc8_1, huc8_1$Name)
list2env(huc8_1_split, envir = .GlobalEnv)

class(Blue) # "sf" "data.frame"
Blue # the projection is still correct from when we transformed it before
Blue <- as(Blue, Class = "Spatial")
# Bring in cropped NLCD data
setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD_2016_Land_Cover_L48_20190424 (1)")
nlcd <- raster("GBCO_NLCD_masked_raster.tif")
GDALinfo("GBCO_NLCD_masked_raster.tif")
attributes(nlcd)
# plot(nlcd)
# After bringing in NLCD data, continue to try to get this to work for 1 HUC8 watershed (Blue)
nlcd_c_Blue <- crop(nlcd, Blue)
nlcd_Blue <- mask(nlcd_c_Blue, Blue)

plot(nlcd_Blue)
class(nlcd_Blue)
writeRaster(nlcd_Blue, "nlcd_Blue_huc14010001.tif")
GDALinfo("nlcd_Blue_huc14010001.tif")
# nlcd_Blue@file@nodatavalue

View(nlcd_Blue@data@attributes[[1]])
huc8df <- nlcd_Blue@data@attributes[[1]]
sapply(huc8df, class)
levels(huc8df$NLCD.Land.Cover.Class)

huc8df <- huc8df[-which(huc8df$NLCD.Land.Cover.Class == ""),]
huc8df <- huc8df[-which(huc8df$NLCD.Land.Cover.Class == "Unclassified"),]
huc8df$NLCD.Land.Cover.Class <- factor(huc8df$NLCD.Land.Cover.Class)
huc8df$PLAND <- (huc8df$COUNT/sum(huc8df$COUNT))*100
sum(huc8df$PLAND) # = 100

ggplot(huc8df)+
  geom_col(mapping = aes(x = NLCD.Land.Cover.Class, y = PLAND))+
  ylim(0,25)+
  coord_flip()

# SUCCESS!!!
# Now, write a function to do what we did above for the 180+ HUC 8 watersheds in our study area
# Once you are able to do this you can delete a lot of the stuff from above that is commented out

setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD_2016_Land_Cover_L48_20190424 (1)")
nlcd_basins <- raster("GBCO_NLCD_masked_raster.tif")

clip_pland <- function(x){
  files <- list.files(path = '/Volumes/Blaszczak Lab/FSS/NHD', pattern = "NHD_H_")
  class(files)
  files <- as.list(files)
  #x <- files[7]
  huc8 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", x, "/WBDHU8.shp"))
  huc8 <- st_transform(huc8, crs = " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  huc8_sp <- as(huc8, Class = "Spatial")
  huc8_split <- split(huc8, huc8$Name)
  #list2env(huc8_1_split, envir = .GlobalEnv)
  huc8_split[[17]] <- as(huc8_split[[17]], Class = "Spatial")
  nlcd_c <- crop(nlcd_m, huc8_split[[17]])
  nlcd_huc8 <- mask(nlcd_c, huc8_split[[17]])
  #plot(nlcd_huc8)
  #class(nlcd_Blue)
  writeRaster(nlcd_huc8, paste0("NLCD_huc_", huc8_split[[17]]$HUC8,".tif"), overwrite = TRUE)
  
  huc8df <- nlcd_huc8@data@attributes[[1]]
  #sapply(huc8df, class)
  #levels(huc8df$NLCD.Land.Cover.Class)
  huc8df <- huc8df[-which(huc8df$NLCD.Land.Cover.Class == ""),]
  huc8df <- huc8df[-which(huc8df$NLCD.Land.Cover.Class == "Unclassified"),]
  huc8df$NLCD.Land.Cover.Class <- factor(huc8df$NLCD.Land.Cover.Class)
  huc8df$PLAND <- (huc8df$COUNT/sum(huc8df$COUNT))*100
  sum(huc8df$PLAND) # = 100
  write.csv(huc8df, paste0("PLAND_huc_", huc8_split[[17]]$HUC8, ".csv"))
  
}

lapply(files, clip_pland) # run with huc8_split[[1]], then do 2, 3, 4, 5. Had to go through files manually for 6, as not all file groups had this many huc8 watersheds
# [[7]] for files[4:16, 18:22]
# [[8]] for files [6, 8:11, 13, 14, 18:22]
# [[9]] for files [8:14, 18:22]
# [[10]] for files [8:11, 13, 18, 20:22]
# [[11]] for files [8:11, 18, 20:22]
# [[12]] for files [8:11, 18, 20, 22]
# [[13]] for files [9, 10, 18, 20, 22]
# [[14]] for files [9, 10, 18, 20, 22]
# [[15]] for files [9, 10, 18, 20]
# [[16]] for files [10, 18]
# [[17]] for files [10]
# [[18]] for files [10]

x <- files[10]
clip_pland(x)
# 214 huc 8 watersheds (verify this)



# Should be able to delete this part VVVVVVVVVVVVVVVVVVVVVV
# Came from before combining all 3 basins to clip and mask
#nlcd_14e <- extract(nlcd, basin14_sp) # takes way too long
# class(nlcd_14)
# plot(nlcd)
# plot(basin14)
# # Try for individual basins
# nlcd_14c <- crop(nlcd, basin14_sp) # only takes a few minutes; I think it crops to the extent of the watershed
# plot(nlcd_14c)
# nlcd_14m <- mask(nlcd_14c, basin14_sp) # also only takes a few minutes
# plot(nlcd_14m)
#
# nlcd_15c <- crop(nlcd, basin15_sp)
# plot(nlcd_15c)
# nlcd_15m <- mask(nlcd_15c, basin15_sp)
# plot(nlcd_15m)
#
# nlcd_16c <- crop(nlcd, basin16_sp)
# plot(nlcd_16c)
# nlcd_16m <- mask(nlcd_16c, basin16_sp)
# plot(nlcd_16m)
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Should also be able to delete this: came before I wrote the function
# VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
# huc8_2 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[2], "/WBDHU8.shp"))
# huc8_3 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[3], "/WBDHU8.shp"))
# huc8_4 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[4], "/WBDHU8.shp"))
# huc8_5 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[5], "/WBDHU8.shp"))
# huc8_6 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[6], "/WBDHU8.shp"))
# huc8_7 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[7], "/WBDHU8.shp"))
# huc8_8 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[8], "/WBDHU8.shp"))
# huc8_9 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[9], "/WBDHU8.shp"))
# huc8_10 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[10], "/WBDHU8.shp"))
# huc8_11 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[11], "/WBDHU8.shp"))
# huc8_12 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[12], "/WBDHU8.shp"))
# huc8_13 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[13], "/WBDHU8.shp"))
# huc8_14 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[14], "/WBDHU8.shp"))
# huc8_15 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[15], "/WBDHU8.shp"))
# huc8_16 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[16], "/WBDHU8.shp"))
# huc8_17 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[17], "/WBDHU8.shp"))
# huc8_18 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[18], "/WBDHU8.shp"))
# huc8_19 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[19], "/WBDHU8.shp"))
# huc8_20 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[20], "/WBDHU8.shp"))
# huc8_21 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[21], "/WBDHU8.shp"))
# huc8_22 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[22], "/WBDHU8.shp"))

# Need to transform all to correct CRS here

# rm(files)
# 
# huc8 <- rbind(huc8_1, huc8_2)
# huc8 <- rbind(huc8, huc8_3)
# huc8 <- rbind(huc8, huc8_4)
# huc8 <- rbind(huc8, huc8_5)
# huc8 <- rbind(huc8, huc8_6)
# huc8 <- rbind(huc8, huc8_7)
# huc8 <- rbind(huc8, huc8_8)
# huc8 <- rbind(huc8, huc8_9)
# huc8 <- rbind(huc8, huc8_10)
# huc8 <- rbind(huc8, huc8_11)
# huc8 <- rbind(huc8, huc8_12)
# huc8 <- rbind(huc8, huc8_13)
# huc8 <- rbind(huc8, huc8_14)
# huc8 <- rbind(huc8, huc8_15)
# huc8 <- rbind(huc8, huc8_16)
# huc8 <- rbind(huc8, huc8_17)
# huc8 <- rbind(huc8, huc8_18)
# huc8 <- rbind(huc8, huc8_19)
# huc8 <- rbind(huc8, huc8_20)
# huc8 <- rbind(huc8, huc8_21)
# huc8 <- rbind(huc8, huc8_22)
