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
flowlinesn <-  st_read("/Volumes/Blaszczak Lab/FSS/NHD/NHD_H_1606_HU4_Shape/NHDFlowline.shp")


> # Bring in list of sites with continuous SC data
  > setwd("/Users/laurenbolotin/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta")
> continuous <- read.csv("WQ_USGS_SCcontinuous_site_list.csv")
> View(continuous)
> class(continuous$SiteID)
[1] "character"
> continuous$SiteID <- as.factor(continuous$SiteID)
> sites <- levels(continuous$SiteID)
> sites <- as.data.frame(sites)
> View(sites)
> setwd("/Volumes/Blaszczak Lab/FSS/All Data")
> saveRDS(sites, "continuous_sites_usgs.rds")