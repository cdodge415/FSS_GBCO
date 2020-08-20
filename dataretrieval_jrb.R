#downloading flow and spc data using dataretrieval package
#6/1/20
#setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
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
disch_huc_sites$Site_ID <- as.numeric(as.character(disch_huc_sites$Site_ID))
disch_huc_sites$Site_ID <- ifelse(disch_huc_sites$Site_ID < 1e7,
                                  yes = paste("0", disch_huc_sites$Site_ID, sep=""),
                                  no = paste(disch_huc_sites$Site_ID))
# disch_huc_sites$Site_ID <- as.character(disch_huc_sites$Site_ID)
siteNumber <- disch_huc_sites %>% pull(Site_ID) # 3900

parameterCd <- "00060"
startDate <- ""  
endDate <- "" 
# if statCd is not specified, it defaults to 00003 (mean) this is ok
discharge <- readNWISdv(siteNumber[1:1000], 
                        parameterCd, startDate, endDate)
discharge2 <- readNWISdv(siteNumber[1001:2000], 
                        parameterCd, startDate, endDate)
discharge3 <- readNWISdv(siteNumber[2001:3000], 
                         parameterCd, startDate, endDate)
discharge4 <- readNWISdv(siteNumber[3001:3600], 
                         parameterCd, startDate, endDate)
siteNumber <- siteNumber[-c(3739)] # had to get rid of SiteID 414500112000000. Would not download, said the SiteID was not an acceptable format.
discharge5 <- readNWISdv(siteNumber[3601:3800],
                         parameterCd,startDate, endDate)
discharge6 <- readNWISdv(siteNumber[3801:3899], parameterCd, startDate, endDate)
discharge7 <- readNWISdv(siteNumbers = "414500112000000", parameterCd, startDate, endDate)


saveRDS(discharge, "USGS_disch_data.rds")
saveRDS(discharge2, "USGS_disch_data2.rds")
saveRDS(discharge3, "USGS_disch_data3.rds")
saveRDS(discharge4, "USGS_disch_data4.rds")
#setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(discharge5, "USGS_disch_data5.rds")
saveRDS(discharge6, "USGS_disch_data6.rds")
saveRDS(discharge7, "USGS_disch_data7.rds")

#------------------------------
# Download water quality (qw) SC data (point measurements) ####
#------------------------------
parameterCd <- c("00095")
# It just so happens that none of the gages in GBCO have the 00094 parameter for SC, so we don't need to download it
SC_huc_sites$Site_ID <- ifelse(SC_huc_sites$Site_ID < 1e7,
                               yes = paste("0", SC_huc_sites$Site_ID, sep=""),
                               no = disch_huc_sites$Site_ID)

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
