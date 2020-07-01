#downloading flow and spc data using dataretrieval package
#6/1/20
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
#setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil/dataretrieval flow and spc")
library(dataRetrieval)
library(tidyverse)
#overlap_sites <- read.csv("disch_SC_sensor.csv") #this is old code
overlap_sites <- readRDS("GBCO_dischSC_sites.rds")
# or 
overlap_huc_sites <- readRDS("GBCO_dischSC_sites.rds")

#parameter codes: 00060-discharge 00095-specific conductance

#####site numbers that overlap
siteNumber <- overlap_sites %>% pull(Site_ID)
#siteNumber <- "10348200"
#pull data using dataretrieval package

#------------------------------
# Download daily value (dv) discharge data ####
#------------------------------
#siteNumber <- c('1031221902','383926107593001','394220106431500')
parameterCd <- "00060"
startDate <- ""  
endDate <- "" 
# if statCd is not specified, it defaults to 00003 (mean)
discharge <- readNWISdv(siteNumber, 
                        parameterCd, startDate, endDate)
write.csv(discharge, "USGS_disch_data.rds")

#---discharge data unit values---
#siteNumber <- "10336645" 
#parameterCd <- "00060" 
#startDate <- "2010-01-01" #form YYYY-MM-DD
#endDate <- "" 

#dischargeUnit <- readNWISuv(siteNumber, parameterCd, 
#                            startDate, endDate)
#dischargeUnit <- renameNWISColumns(dischargeUnit)

#------------------------------
# Download water quality (qw) SC data (point measurements) ####
#------------------------------
#---USGS water quality data---
#siteNumber <- "10336645"
parameterCd <- c("00095")
# It just so happens that none of the gages in GBCO have the 00094 parameter for SC, so we don't need to download it
# you can tell by looking at overlap_huc_sites, which was created with the Chloe_USGS_DataFilter.R scripts

startDate <- ""
endDate <- ""

siteNumber <- overlap_huc_sites$Site_ID[which(overlap_huc_sites$data_type_cd == "qw")]
# there are 117 sites with qw SC data
dfLong <- readNWISqw(siteNumber, parameterCd, 
                     startDate, endDate)

write.csv(dfLong, "USGS_SC_data.rds")

#------------------------------
# Download daily value (dv) SC data ####
#------------------------------
overlap_huc_sites <- readRDS("GBCO_dischSC_sites.rds")

parameterCd <- c("00095")

startDate <- ""
endDate <- ""

siteNumber <- overlap_huc_sites$Site_ID[which(overlap_huc_sites$data_type_cd == "dv")]
dfLong <- readNWISdv(siteNumber, parameterCd, 
                     startDate, endDate)
saveRDS(dfLong, "USGS_SC_dv_data.rds")

#------------------------------
# Download unit value (uv) SC data ####
#------------------------------

siteNumber <- overlap_huc_sites$Site_ID[which(overlap_huc_sites$data_type_cd == "uv")]
# had to download in chunks to get this to run
dfLong <- readNWISuv(siteNumber[1:5], parameterCd, 
                     startDate, endDate)
dfLong1 <- readNWISuv(siteNumber[6:12], parameterCd, 
                      startDate, endDate)
dfLong2 <- readNWISuv(siteNumber[13:20], parameterCd, 
                      startDate, endDate)
dfLong3 <- readNWISuv(siteNumber[20:27], parameterCd, 
                      startDate, endDate)
dfLong4 <- readNWISuv(siteNumber[28:34], parameterCd, 
                      startDate, endDate)
dfLong5 <- readNWISuv(siteNumber[35:42], parameterCd, 
                      startDate, endDate)
dfLong6 <- readNWISuv(siteNumber[43:49], parameterCd, 
                      startDate, endDate)
dfLong7 <- readNWISuv(siteNumber[50:55], parameterCd, 
                      startDate, endDate)
dfLong8 <- readNWISuv(siteNumber[56:62], parameterCd, 
                      startDate, endDate)
dfLong9 <- readNWISuv(siteNumber[63:70], parameterCd, 
                      startDate, endDate)

SC_uv_data <- rbind_list(dfLong,dfLong1, dfLong2, dfLong3, dfLong4, dfLong5, dfLong6, dfLong7, dfLong8, dfLong9)
class(SC_uv_data$site_no)
SC_uv_data$site_no <- as.factor(SC_uv_data$site_no)
levels(SC_uv_data$site_no)
saveRDS(SC_uv_data, "USGS_SC_uv_data.rds")

# filter for data that is approved for publication
final_SC_uv <- USGS_SC_uv_data[which(USGS_SC_uv_data$X_00095_00000_cd == "A"),]
class(final_SC_uv$site_no)
levels(final_SC_uv$site_no)
saveRDS(final_SC_uv, "USGS_SC_uv_data_AFP.rds")