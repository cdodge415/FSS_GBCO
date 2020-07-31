library(FedData)
library(sf)
library(raster)
library(sp)
library(rgdal)
library(fasterize)

# NEW: Clip NLCD to watersheds in ArcMap or QGIS, try to calculate PLAND in R

# basin14 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/WBD_14_HU2_Shape/WBDHU2.shp")
# basin15 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/WBD_15_HU2_Shape/WBDHU2.shp")
# basin16 <- st_read("~/Desktop/Blaszczak Lab/GB CO WQ Data/NHD/WBD_16_HU2_Shape/WBDHU2.shp")
## bbox:           xmin: -120.4649 ymin: 35.20661 xmax: -110.5985 ymax: 42.85578

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

# Example from: https://gatesdupont.com/2019/06/16/nlcd-velox-dplyr.html
library(dplyr)
library(rnaturalearth)
library(sf)

state = ne_states(iso_a2 = "US", returnclass = "sf") %>%  # pull admin. bounds. for US
  filter(iso_3166_2 == "US-MA") %>% # select Massachusetts
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80
+towgs84=0,0,0,0,0,0,0 +units=m +no_defs") # nlcd crs

pts = st_sample(state, size = 10, type = "regular") # sample 10 points in polygon

# Plot!
plot(st_transform(pts, 4326), col="red", pch=20)
maps::map("state", add=T)

# Bring in NLCD data
library(FedData)
nlcd = get_nlcd(        # fn for pulling NLCD data
  template = state,     # polygon template for download
  label = "4pland",     # this is just a character string label
  year = 2011,          # specify the year (2016 should be available soon)
  force.redo = F
) 

# Plot!
plot(nlcd)
plot(pts, col="black", pch=20, cex=1.5, add=T)

# Extract (option 1)
library(raster)
ex.mat.r = extract(nlcd, as(pts, 'Spatial'), buffer=1000, df=T) # raster extract

# Extract (option 2, supposedly better for "complicated analysis" with large areas and many points)
library(velox)
library(sp)
nlcd.vx = velox(stack(nlcd))                                  # raster for velox
sp.buff = gBuffer(as(pts, 'Spatial'), width=1000, byid=TRUE)  # spatial buffer, radius in meters
buff.df = SpatialPolygonsDataFrame(
  sp.buff,                                         
  data.frame(id=1:length(sp.buff)),                 # set ids
  FALSE)                                            # df of buffers
ex.mat.vx = nlcd.vx$extract(buff.df, df=T)                    # extract buffers from velox raster
rm(nlcd.vx) # removing the velox raster can free up space

# Calculate PLAND
prop.lc = ex.mat.vx %>%
  setNames(c("ID", "lc_type")) %>%        # rename for ease
  group_by(ID, lc_type) %>%               # group by point (ID) and lc class 
  summarise(n = n()) %>%                  # count the number of occurences of each class
  mutate(pland = n / sum(n)) %>%          # calculate percentage
  ungroup() %>%                           # convert back to original form
  dplyr::select(ID, lc_type, pland) %>%   # keep only these vars
  complete(ID, nesting(lc_type), 
           fill = list(pland = 0)) %>%             # fill in implicit landcover 0s
  spread(lc_type, pland)                  # convert to long format

# Assing land cover class names
nlcd_cover_df = as.data.frame(nlcd@data@attributes[[1]]) %>%      # reference the name attributes
  subset(NLCD.2011.Land.Cover.Class != "") %>%                    # only those that are named
  dplyr::select(ID, NLCD.2011.Land.Cover.Class)                   # keep only the ID and the lc class name
lc.col.nms = data.frame(ID = as.numeric(colnames(prop.lc[-1])))   # select landcover classes
matcher = merge.data.frame(x = lc.col.nms,                        # analogous to VLOOKUP in Excel
                           y = nlcd_cover_df,
                           by.x = "ID",
                           all.x = T)               
colnames(prop.lc) = c("ID", as.character(matcher[,2]))            # assign new names

# Results
print(prop.lc)









#############
# Example from FedData vignette:
# NOT RUN {
# vepPolygon <- polygon_from_extent(raster::extent(672800,740000,4102000,4170000), 
#                                   proj4string='+proj=utm +datum=NAD83 +zone=12')
# class(vepPolygon)
# 
# # Get the NLCD (USA ONLY)
# # Returns a raster
# NLCD <- get_nlcd(template=vepPolygon, label='VEPIIN')
# 
# # Plot with raster::plot
# plot(NLCD)
# 




###############################################################################################
