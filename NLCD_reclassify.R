library(tidyverse)

setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD_2016_Land_Cover_L48_20190424 (1)")
files <- list.files(pattern = "PLAND_huc")
files <- as.list(files)

#nlcd <- lapply(files, read.csv)

nlcd_reclass <- function(x){
setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD_2016_Land_Cover_L48_20190424 (1)")
df <- read.csv(x)
print(df)
df <- df %>%
  mutate(NLCD.Reclass = recode(NLCD.Land.Cover.Class, 
                               "Open Water" = "Undeveloped", 
                               "Perennial Snow/Ice" = "Undeveloped",
                               "Developed, Open Space" = "Developed",
                               "Developed, Low Intensity" = "Developed",
                               "Developed, Medium Intensity" = "Developed",
                               "Developed, High Intensity" = "Developed",
                               "Barren Land" = "Undeveloped",
                               "Deciduous Forest" = "Undeveloped",
                               "Evergreen Forest" = "Undeveloped",
                               "Mixed Forest" = "Undeveloped",
                               "Shrub/Scrub" = "Undeveloped",
                               "Herbaceuous" = "Undeveloped",
                               "Hay/Pasture" = "Agricultural",
                               "Cultivated Crops" = "Agricultural",
                               "Woody Wetlands" = "Undeveloped",
                               "Emergent Herbaceuous Wetlands" = "Undeveloped",
                               .default = NULL))
# Could have also set Urban and Ag and then said: , .default = "Undeveloped" to automatically apply that to the last several land use types
df2 <- select(df, c("NLCD.Land.Cover.Class", "PLAND", "NLCD.Reclass"))
df2 <- df2 %>%
  group_by(NLCD.Reclass) %>%
  summarise_at(.vars = "PLAND", .funs = c("sum" = sum))
setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD_2016_Land_Cover_L48_20190424 (1)/Reclassified NLCD")
write.csv(df2, paste0(x,"_reclass.csv"))
}

lapply(files, nlcd_reclass)

# I wonder if we will eventually care how much of a primarily undeveloped watershed is forested vs shrub/scrub, etc.
# If we need to figure this out, we would make 3 different dfs for each watershed, one for all the Developed, Undeveloped, and Agricultural land uses,
# and then see what percentage each one makes up
# We may also not want 3 such simplified categories of dominant land use type
