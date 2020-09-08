#downloading flow and spc data using dataretrieval package
#6/1/20
#setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
#setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil/dataretrieval flow and spc")

library(dataRetrieval)
library(tidyverse)
library(data.table)

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

# Downloaded by Joanna: 
saveRDS(discharge, "USGS_disch_data.rds")
saveRDS(discharge2, "USGS_disch_data2.rds")
saveRDS(discharge3, "USGS_disch_data3.rds")
saveRDS(discharge4, "USGS_disch_data4.rds")
# Downloaded by Lauren
#setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(discharge5, "USGS_disch_data5.rds")
saveRDS(discharge6, "USGS_disch_data6.rds")
saveRDS(discharge7, "USGS_disch_data7.rds")

#------------------------------
# Download water quality (qw) SC data (point measurements) ####
#------------------------------
parameterCd <- c("00095")
# It just so happens that none of the gages in GBCO have the 00094 parameter for SC, so we don't need to download this parameter
SC_huc_sites$Site_ID <- ifelse(SC_huc_sites$Site_ID < 1e7,
                               yes = paste("0", SC_huc_sites$Site_ID, sep=""),
                               no = SC_huc_sites$Site_ID)

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
class(siteNumber)
# there are 73 sites with uv SC data
SCuv1 <- readNWISuv(siteNumber[1:5], parameterCd, 
                   startDate, endDate) 
SCuv2 <- readNWISuv(siteNumber[6:26], parameterCd, 
                    startDate, endDate)
SCuv3 <- readNWISuv(siteNumber[27:50], parameterCd, 
                    startDate, endDate)
SCuv4 <- readNWISuv(siteNumber[51:73], parameterCd, 
                    startDate, endDate)

#setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(SCuv1, "USGS_SC_uv1_data.rds")
saveRDS(SCuv2, "USGS_SC_uv2_data.rds")
saveRDS(SCuv3, "USGS_SC_uv3_data.rds")
saveRDS(SCuv4, "USGS_SC_uv4_data.rds")

#----------------------------
# Combine data files #####
#----------------------------
# clear environment after each one to save space
# SC_uv
allSCuv <- list(SCuv1, SCuv2, SCuv3, SCuv4)
allSCuv <- rbindlist(allSCuv)
saveRDS(allSCuv, "USGS_SC_uv_data.rds")
rm(allSCuv, SCuv, SCuv1, SCuv2, SCuv3, SCuv4)
# disch
disch1 <- readRDS("USGS_disch_data.rds")
saveRDS(disch1, "USGS_disch_data1.rds") # resave this with a different name so we can keep all original downloads as well as the combined file we are aobut to make
disch2 <- readRDS("USGS_disch_data2.rds")
disch3 <- readRDS("USGS_disch_data3.rds")
disch4 <- readRDS("USGS_disch_data4.rds")
disch5 <- readRDS("USGS_disch_data5.rds")
disch6 <- readRDS("USGS_disch_data6.rds")
disch7 <- readRDS("USGS_disch_data7.rds")
# dataframes have different columns in some cases
alldisch <- bind_rows(disch1, disch2)
alldisch <- bind_rows(alldisch, disch3)
alldisch <- bind_rows(alldisch, disch4)
alldisch <- bind_rows(alldisch, disch5)
alldisch <- bind_rows(alldisch, disch6)
alldisch <- bind_rows(alldisch, disch7)
saveRDS(alldisch, "USGS_disch_data.rds")
