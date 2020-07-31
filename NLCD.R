library(FedData)
library(sf)
library(raster)
library(sp)
library(rgdal)
library(fasterize)
# NEW: Clip NLCD to watersheds in ArcMap or QGIS, try to calculate PLAND in R

# NOT RUN {
# Extract data for the Village Ecodynamics Project 'VEPIIN' study area:
# http://village.anth.wsu.edu
# vepPolygon <- polygon_from_extent(raster::extent(672800,740000,4102000,4170000), 
#                                   proj4string='+proj=utm +datum=NAD83 +zone=12')
# basin14 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/WBD_14_HU2_Shape/WBDHU2.shp")
# basin15 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/WBD_15_HU2_Shape/WBDHU2.shp")
basin16 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/WBD_16_HU2_Shape/WBDHU2.shp")
# bbox:           xmin: -120.4649 ymin: 35.20661 xmax: -110.5985 ymax: 42.85578

# GBCO_basins <- rbind_list(basin14, basin15, basin16)
# class(GBCO_basins)
# GBCO_basins <- st_as_sf(GBCO_basins)

# Set coordinate reference system and confirm that it worked:
# *Note: it's important to use an equal area projection since we are looking at proportional land cover
# NLCD uses Albers Equal Area
st_crs(basin16) <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs"
st_crs(basin16)
st_transform(basin16, crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs")
#https://epsg.io/102008
class(basin16)


# GBCO_basins_r <- fasterize(GBCO_basins, raster::raster(GBCO_basins))
# class(GBCO_basins_r)
# GBCO_basins_r <- raster(GBCO_basins_r)
# Get the NLCD (USA ONLY)
basin16_r <- fasterize(basin16, raster::raster(basin16))
# class(basin16_r)
basin16_r
poly16 <- polygon_from_extent(raster::extent(-120.4649, -110.5985, 35.20661, 42.85578), proj4string = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
class(poly16)
plot(poly16)
# Returns a raster
NLCD <- get_nlcd(template=poly16, label='GBCO', force.redo = F)
# Plot with raster::plot
plot(NLCD)
# }


#############
vepPolygon <- polygon_from_extent(raster::extent(672800,740000,4102000,4170000), 
                                  proj4string='+proj=utm +datum=NAD83 +zone=12')
class(vepPolygon)

# Get the NLCD (USA ONLY)
# Returns a raster
NLCD <- get_nlcd(template=vepPolygon, label='VEPIIN')

# Plot with raster::plot
plot(NLCD)

###############################################################################################

if (!require(devtools)) install.packages('devtools')
devtools::install_github("giswqs/whiteboxR")
library(whitebox)
# Prints the whitebox-tools help...a listing of available commands
print(wbt_help())

# Prints the whitebox-tools license
print(wbt_license())

# Prints the whitebox-tools version
print(wbt_version())

# Prints the toolbox for a specific tool.
print(wbt_toolbox())

# List all available tools in whitebox-tools
print(wbt_list_tools())

# Lists tools with 'lidar' in tool name or description.
print(wbt_list_tools("lidar"))

# Prints the help for a specific tool.
print(wbt_tool_help("lidar_info"))

# Retrieves the tool parameter descriptions for a specific tool.
print(wbt_tool_parameters("slope"))

# View the source code for a specific tool on the source code repository.
print(wbt_view_code("breach_depressions"))

