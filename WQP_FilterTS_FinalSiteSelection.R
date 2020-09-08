# We have now formatted WQP time series data and metadata
# While formatting the metadata, we filtered sites for those that are flowing waters
# We now need to filter the time series (TS) data to include only these sites
# **So far, we have only done this for specific conductance, and will need to re-do it for flow and other
# water quality parameters

# Set working directory and bring in data:
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted TS")
# before formatting the TS there were 69,735 sites
TS <- read.csv("WQ_TS.csv") # 64,130 after formatting the TS
class(TS$SiteID)
levels(TS$SiteID)

setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta")
meta <- read.csv("WQP_formatted_metadata_WQ.csv") # 32,772 sites 
# Formatted metadata was already filtered for acceptable site types

names(TS)
names(meta)

# Subset TS data accordingly for acceptable site types: 
TS_sub <- subset(TS, TS$SiteID %in% meta$SiteID)

# Output a csv with just the SC data:
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted TS")
write.csv(TS_sub, "WQ_TS_final_sites.csv")
