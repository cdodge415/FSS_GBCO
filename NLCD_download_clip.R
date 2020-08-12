setwd("/Volumes/Blaszczak Lab/FSS/NHD")
library(FedData)
library(sf)
library(raster)
library(sp)
library(rgdal)
library(fasterize)
library(data.table)

# # Read in watershed shapefile
# basin14 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1401_HU4_Shape/WBDHU2.shp")
# basin15 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1501_HU4_Shape/WBDHU2.shp")
# basin16 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1606_HU4_Shape/WBDHU2.shp")
# 
# # Read in NLCD raster
# setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD_2016_Land_Cover_L48_20190424 (1)")
# nlcd <- raster("NLCD_2016_Land_Cover_L48_20190424.img")
# 
# # Transform watershed to same projection as NLCD data and convert to polygon
# class(basin14)
# #st_crs(basin14) <- " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
# basin14 <- st_transform(basin14, crs = " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
# basin14_sp <- as(basin14, Class = "Spatial")
# # or: #basin14_sp <- sf::as_Spatial(basin14)
# class(basin14_sp)
# 
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
# #nlcd_14e <- extract(nlcd, basin14_sp) # takes way too long
# # class(nlcd_14)
# # plot(nlcd)
# # plot(basin14)
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
# 
# nlcd_c <- crop(nlcd, basins_sp) 
# plot(nlcd_c)
# nlcd_m <- mask(nlcd_c, basins_sp)
# plot(nlcd_m)
# 
# writeRaster(nlcd_m, "GBCO_NLCD_masked_raster.img")

# Create a function to do this for each of the smaller watersheds, I'm thinking HUC 8 since that is the level of HUC offered by our data
setwd("/Volumes/Blaszczak Lab/FSS/NHD")
files <- list.files(path = '/Volumes/Blaszczak Lab/FSS/NHD', pattern = "NHD_H_")
class(files)
files <- as.list(files)

huc8_1 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[1], "/WBDHU8.shp"))
huc8_2 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[2], "/WBDHU8.shp"))
huc8_3 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[3], "/WBDHU8.shp"))
huc8_4 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[4], "/WBDHU8.shp"))
huc8_5 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[5], "/WBDHU8.shp"))
huc8_6 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[6], "/WBDHU8.shp"))
huc8_7 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[7], "/WBDHU8.shp"))
huc8_8 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[8], "/WBDHU8.shp"))
huc8_9 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[9], "/WBDHU8.shp"))
huc8_10 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[10], "/WBDHU8.shp"))
huc8_11 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[11], "/WBDHU8.shp"))
huc8_12 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[12], "/WBDHU8.shp"))
huc8_13 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[13], "/WBDHU8.shp"))
huc8_14 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[14], "/WBDHU8.shp"))
huc8_15 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[15], "/WBDHU8.shp"))
huc8_16 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[16], "/WBDHU8.shp"))
huc8_17 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[17], "/WBDHU8.shp"))
huc8_18 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[18], "/WBDHU8.shp"))
huc8_19 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[19], "/WBDHU8.shp"))
huc8_20 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[20], "/WBDHU8.shp"))
huc8_21 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[21], "/WBDHU8.shp"))
huc8_22 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", files[22], "/WBDHU8.shp"))

rm(files)

huc8 <- rbind(huc8_1, huc8_2)
huc8 <- rbind(huc8, huc8_3)
huc8 <- rbind(huc8, huc8_4)
huc8 <- rbind(huc8, huc8_5)
huc8 <- rbind(huc8, huc8_6)
huc8 <- rbind(huc8, huc8_7)
huc8 <- rbind(huc8, huc8_8)
huc8 <- rbind(huc8, huc8_9)
huc8 <- rbind(huc8, huc8_10)
huc8 <- rbind(huc8, huc8_11)
huc8 <- rbind(huc8, huc8_12)
huc8 <- rbind(huc8, huc8_13)
huc8 <- rbind(huc8, huc8_14)
huc8 <- rbind(huc8, huc8_15)
huc8 <- rbind(huc8, huc8_16)
huc8 <- rbind(huc8, huc8_17)
huc8 <- rbind(huc8, huc8_18)
huc8 <- rbind(huc8, huc8_19)
huc8 <- rbind(huc8, huc8_20)
huc8 <- rbind(huc8, huc8_21)
huc8 <- rbind(huc8, huc8_22)

class(huc8)
huc8 <- as(huc8, Class = "Spatial")

# rm(huc8_1, huc8_10, huc8_11,huc8_12, huc8_13, huc8_14, huc8_15, huc8_16, huc8_17, huc8_18, huc8_19, huc8_2, huc8_20, huc8_21, huc8_22, huc8_3,huc8_4, huc8_5, huc8_6, huc8_7, huc8_8, huc8_9)

# TRANSFORM/SET CRS WHEN YOU'RE FINISHED

setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD_2016_Land_Cover_L48_20190424 (1)")
nlcd <- raster("GBCO_NLCD_masked_raster.img")
# plot(nlcd)

# huc8
# nlcd
# huc8 <- st_transform(huc8, crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


huc8_1
class(huc8_1)
huc8_1 <- as(huc8_1, Class = "Spatial")
huc8_1 <- transform(huc8_1, crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
huc8_1m <- mask(nlcd, huc8_1[1])






# read_huc8 <- function(x){
#   huc8 <- st_read(paste0("/Volumes/Blaszczak Lab/FSS/NHD/", x, "/WBDHU8.shp"))
# }

# read_huc8(files[2])

# class(huc8)
# huc8_split <- split(huc8, huc8$HUC8)
# 
# list2env(huc8_split,envir=.GlobalEnv)
# 
# hucs <- ls()[1:5]
