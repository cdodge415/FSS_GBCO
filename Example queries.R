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
  # disch_query <- readRDS(here("output", "disch_query.rds")) 
  # SC_query <- readRDS(here("output", "SC_query.rds"))
 disch_query <- readRDS(here("disch_query.rds")) 
 SC_query <- readRDS(here("SC_query.rds"))

#-------------------------------------------------
#Filter for all lotic NWIS sites with specific conductivity sensors (subdaily or daily)  
#-------------------------------------------------
  SC_lotic_sensor <- SC_query %>%
    filter(site_tp_cd %in% c("ST", "ST-CA", "ST-DCH", "ST-TS", "SP")) %>%
    filter(data_type_cd == "dv" | data_type_cd == "uv") %>%
    filter(!is.na(Lat)) %>%
    distinct(Site_ID, Lat, Lon)
        
#-------------------------------------------------
#Filter for all lotic NWIS sites with specific conductivity and discharge sensors (subdaily or daily)  
#-------------------------------------------------
  #Since the specific conductivity query contains two different parameter codes ("00094", "00095")
  #We have to do a little trick to get the overlap function to work by giving them all the same parameter code
    SC_query2 <- SC_query
    SC_query2$parm_cd <- "00000"

  #Find concurrent overlap of data
    disch_SC_sensor <- overlap_fun(
      param_queries = list(disch_query, SC_query2), 
      site_types = c("ST", "ST-CA", "ST-DCH", "ST-TS", "SP"), 
      data_types = c("uv", "dv"), 
      min_obs = 1
    )
  # there are 2002 sites in the COUNTRY with SC and Discharge sensors  
  
write.csv(disch_SC_sensor, "disch_SC_sensor.csv")



#disch_query$HUC <- substr(disch_query$huc_cd,1,2)
