#===============================================================================
#Example script for filtering data queries
#Created 5/15/2020
#===============================================================================
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
library("here") # WORKING DIRECTORY MUST BE CORRECT BEFORE CALLING THIS PACKAGE AND USING IT
library("rgdal") #For CRS
library("data.table")
library("dplyr") #For filtering

#Source functions for finding overlap
#source(here("R", "functions", "concurrent_overlap.R"))
source(here("concurrent_overlap.R"))

#Read in the data queries
 disch_query <- readRDS(here("disch_query.rds"))
 SC_query <- readRDS(here("SC_query.rds"))

#-------------------------------------------------
#Filter for all lotic NWIS sites with specific conductivity sensors (subdaily or daily)  
#-------------------------------------------------
  SC_lotic_sensor <- SC_query %>%
    filter(site_tp_cd %in% c("ST", "ST-CA", "ST-DCH", "ST-TS", "SP")) %>%
    filter(data_type_cd == "dv" | data_type_cd == "uv") %>%
    filter(!is.na(Lat)) %>%
    distinct(Site_ID, Lat, Lon) # Flowing waters with SC
    
    class(SC_lotic_sensor$Site_ID)
    SC_lotic_sensor$Site_ID <- factor(SC_lotic_sensor$Site_ID)
    levels(SC_lotic_sensor$Site_ID) # 2811
 
  disch_lotic_sensor <- disch_query %>%
   filter(site_tp_cd %in% c("ST", "ST-CA", "ST-DCH", "ST-TS", "SP")) %>%
   filter(data_type_cd == "dv" | data_type_cd == "uv") %>%
   filter(!is.na(Lat)) %>%
   distinct(Site_ID, Lat, Lon) # Flowing waters with Q
  
   class(disch_lotic_sensor$Site_ID)
   disch_lotic_sensor$Site_ID <- factor(disch_lotic_sensor$Site_ID)
   levels(disch_lotic_sensor$Site_ID) # 23425
  
  
  
  # Write 2 csv's: 1 for all lotic sites with SC and 1 for all lotic sites with Q, this is all data we want
  SC_lotic_sensor <- subset(SC_query, SC_query$Site_ID %in% SC_lotic_sensor$Site_ID)
  disch_lotic_sensor <- subset(disch_query, disch_query$Site_ID %in% disch_lotic_sensor$Site_ID)
  
  write.csv(SC_lotic_sensor, "all_SC_lotic_sensor.csv") # as opposed to SC_only which is sites that have SC and not disch
  write.csv(disch_lotic_sensor,"all_disch_lotic_sensor.csv") # as opposed to disch_only which is sites that have disch and not SC
  
  
# Concurrent overlap doesn't necessarily matter for us since we are doing streamgage matching 
#-------------------------------------------------
#Filter for all lotic NWIS sites with specific conductivity and discharge sensors (subdaily or daily)  
#-------------------------------------------------
#   #Since the specific conductivity query contains two different parameter codes ("00094", "00095")
#   #We have to do a little trick to get the overlap function to work by giving them all the same parameter code
#     SC_query2 <- SC_query
#     SC_query2$parm_cd <- "00000"
# 
#   #Find concurrent overlap of data
#     disch_SC_sensor <- overlap_fun(
#       param_queries = list(disch_query, SC_query2), 
#       site_types = c("ST", "ST-CA", "ST-DCH", "ST-TS", "SP"), 
#       data_types = c("uv", "dv"), 
#       min_obs = 1
#     )  # there are 2002 sites with flowing water in the COUNTRY with SC and Discharge sensors  
#     
# write.csv(disch_SC_sensor, "disch_SC_sensor.csv") # Flowing waters with Q and SC


# #See what sites have SC that do not have discharge    
# SC_only <- setdiff(SC_lotic_sensor, disch_SC_sensor) # 811 sites
# unique(SC_query$Site_ID)
# SC_query[duplicated(SC_query$Site_ID),]
# SC_only <- SC_query[which(SC_query$Site_ID %in% SC_only$Site_ID),]
# # SC_only$Site_ID <- as.factor(SC_only$Site_ID)  
# # levels(SC_only$Site_ID) 
# # SC_only$site_tp_cd <- as.factor(SC_only$site_tp_cd)
# # levels(SC_only$site_tp_cd) # already filtered for only flowing waters
# 
# write.csv(SC_only, "SC_sensor_only.csv")
# 
# #See what sites have discharge that do not have SC
# disch_only <- setdiff(disch_lotic_sensor, disch_SC_sensor)
# disch_only <- disch_query[which(disch_query$Site_ID %in% disch_only$Site_ID),]
# 
# write.csv(disch_only, "disch_sensor_only.csv")
