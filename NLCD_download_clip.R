setwd("/Volumes/Blaszczak Lab/FSS/NHD")
library(FedData)
library(sf)
library(raster)
library(sp)
library(rgdal)
library(fasterize)

# Read in watershed shapefile
basin14 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1401_HU4_Shape/WBDHU2.shp")
basin15 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1501_HU4_Shape/WBDHU2.shp")
basin16 <- st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1606_HU4_Shape/WBDHU2.shp")

# Read in NLCD raster
setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD_2016_Land_Cover_L48_20190424 (1)")
nlcd <- raster("NLCD_2016_Land_Cover_L48_20190424.img")

# Transform watershed to same projection as NLCD data and convert to polygon
class(basin14)
#st_crs(basin14) <- " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
basin14 <- st_transform(basin14, crs = " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
basin14_sp <- as(basin14, Class = "Spatial")
# or: #basin14_sp <- sf::as_Spatial(basin14)
class(basin14_sp)

basin15 <- st_transform(basin15, crs = " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
basin15_sp <- as(basin15, Class = "Spatial")
basin16 <- st_transform(basin16, crs = " +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
basin16_sp <- as(basin16, Class = "Spatial")

# See if you can combine all basins into 1
basins <- rbind(basin14, basin15)
basins <- rbind(basins, basin16)
basins
basins_sp <- as(basins, Class = "Spatial")

#nlcd_14e <- extract(nlcd, basin14_sp) # takes way too long
# class(nlcd_14)
# plot(nlcd)
# plot(basin14)
nlcd_14c <- crop(nlcd, basin14_sp) # only takes a few minutes; I think it crops to the extent of the watershed
plot(nlcd_14c)
nlcd_14m <- mask(nlcd_14c, basin14_sp) # also only takes a few minutes
plot(nlcd_14m)

nlcd_15c <- crop(nlcd, basin15_sp) 
plot(nlcd_15c)
nlcd_15m <- mask(nlcd_15c, basin15_sp)
plot(nlcd_15m)

nlcd_16c <- crop(nlcd, basin16_sp) 
plot(nlcd_16c)
nlcd_16m <- mask(nlcd_16c, basin16_sp)
plot(nlcd_16m)

nlcd_c <- crop(nlcd, basins_sp) 
plot(nlcd_c)
nlcd_m <- mask(nlcd_c, basins_sp)
plot(nlcd_m)

writeRaster(nlcd_m, "GBCO_NLCD_masked_raster.img")

# Create a function to do this for each of the smaller watersheds, I'm thinking HUC 8 since that is the level of HUC offered by our data
