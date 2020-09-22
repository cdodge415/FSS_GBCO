#downloading flow and spc data using dataretrieval package
#6/1/20
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
#setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil/dataretrieval flow and spc")
library(dataRetrieval)
library(tidyverse)
disch_huc_sites <- readRDS("GBCO_disch_sites.rds") # all lotic disch sites in the GBCO
SC_huc_sites <- readRDS("GBCO_SC_sites.rds") # all lotic SC sites in the GBCO

#parameter codes: 00060-discharge 00095-specific conductance
#pull data using dataretrieval package

#------------------------------
# Download daily value (dv) discharge data ####
#------------------------------
siteNumber <- disch_huc_sites %>% pull(Site_ID) # 3900
parameterCd <- "00060"
startDate <- ""  
endDate <- "" 
# if statCd is not specified, it defaults to 00003 (mean) this is ok
discharge <- readNWISdv(siteNumber, 
                        parameterCd, startDate, endDate)
saveRDS(discharge, "USGS_disch_data.rds")

#------------------------------
# Download water quality (qw) SC data (point measurements) ####
#------------------------------
parameterCd <- c("00095")
# It just so happens that none of the gages in GBCO have the 00094 parameter for SC, so we don't need to download it
siteNumber <- SC_huc_sites$Site_ID[which(SC_huc_sites$data_type_cd == "qw")]
# there are 53 sites with qw SC data
SCqw <- readNWISqw(siteNumber, parameterCd, 
                     startDate, endDate)

saveRDS(SCqw, "USGS_SC_qw_data.rds")

#------------------------------
# Download daily value (dv) SC data ####
#------------------------------
siteNumber <- SC_huc_sites$Site_ID[which(SC_huc_sites$data_type_cd == "dv")]
# there are 386 sites with dv SC data
SCdv <- readNWISdv(siteNumber, parameterCd, 
                     startDate, endDate)
saveRDS(SCdv, "USGS_SC_dv_data.rds")

#------------------------------
# Download unit value (uv) SC data ####
#------------------------------
siteNumber <- SC_huc_sites$Site_ID[which(SC_huc_sites$data_type_cd == "uv")]
# there are 73 sites with uv SC data
SCuv <- readNWISuv(siteNumber, parameterCd, 
                     startDate, endDate)

saveRDS(SCuv, "USGS_SC_uv_data.rds")

# filter for data that is approved for publication
# final_SC_uv <- USGS_SC_uv_data[which(USGS_SC_uv_data$X_00095_00000_cd == "A"),]
# class(final_SC_uv$site_no)
# levels(final_SC_uv$site_no)
# saveRDS(final_SC_uv, "USGS_SC_uv_data_AFP.rds")
