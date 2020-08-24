# We have now formatted WQP time series data and metadata
# While formatting the metadata, we filtered sites for those that are flowing waters
# We now need to filter the time series (TS) data to include only these sites
# **So far, we have only done this for specific conductance, and will need to re-do it for flow and other
# water quality parameters

# Set working directory and bring in data:
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted TS")
TS <- read.csv("WQ_TS.csv") # 64,130 (see if you can go back and see if this matches the metadata before it is filtered for lotic sites)
class(TS$SiteID)
levels(TS$SiteID)

setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta")
meta <- read.csv("WQP_formatted_metadata_WQ.csv") # 32,772 sites

names(TS)
names(meta)

# Subset data accordingly: 
TS_sub <- subset(TS, TS$SiteID %in% meta$SiteID)

# Output a csv with just the SC data:
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted TS")
write.csv(TS_sub, "WQ_SC_TS_final_sites.csv")
