setwd("C:/Users/cdodg/Desktop/Blaszczak lab/FSS project/Data from Phil")
# Data is for all of US
# Need to filter for GB and CO River basins (HUC 14, 15, 16)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(tidyverse)
library(dplyr)

# Read in data
disch_sensor <- read.csv("all_disch_lotic_sensor.csv")
SC_sensor <- read.csv("all_SC_lotic_sensor.csv")
# disch_SC_sensor <- ("disch_SC_sensor.csv")
# Chloe: we don't really want to filter this, this still has sites of all types, not just lotic
# disch_data <- readRDS("disch_query.rds")
# SC_data <- readRDS("SC_query.rds")

# # Bar plot of hucIDs
# ggplot(disch_data, aes(huc_cd)) +
# geom_bar(fill = "#0073C2FF") +
# theme_pubclean()
# # Confirmed data is for all of US

# Need to filter for HUC 14,15,16 with >=365 days of obs, assuming this gives us at least 1 rough year of data
# Lauren replaced disch_data and SC_data here with disch_sensor and SC_sensor
disch_huc_sites <- disch_sensor %>% 
  filter(str_detect(huc_cd, "^14|^15|^16") & count_nu>=365) %>% 
  select(Site_ID,station_nm,Lat,Lon,huc_cd,parm_cd,begin_date,end_date,count_nu,site_tp_cd,data_type_cd,access_cd)
  disch_huc_sites$Site_ID <- factor(disch_huc_sites$Site_ID)
  levels(disch_huc_sites$Site_ID) # 2654
SC_huc_sites <- SC_sensor %>% 
  filter(str_detect(huc_cd, "^14|^15|^16") & count_nu>=365) %>% 
  select(Site_ID,station_nm,Lat,Lon,huc_cd,parm_cd,begin_date,end_date,count_nu,site_tp_cd,data_type_cd,access_cd)
  SC_huc_sites$Site_ID <- factor(SC_huc_sites$Site_ID)  
  levels(SC_huc_sites$Site_ID) # 193
# disch_SC_huc_sites <- disch_SC_sensor %>% 
#   filter(str_detect(huc_cd, "^14|^15|^16") & count_nu>=365) %>% 
#   select(Site_ID,station_nm,Lat,Lon,huc_cd,parm_cd,begin_date,end_date,count_nu,site_tp_cd,data_type_cd,access_cd)


saveRDS(disch_huc_sites, "GBCO_disch_sites.rds")
saveRDS(SC_huc_sites, "GBCO_SC_sites.rds")

overlap_huc_sites <- intersect(disch_huc_sites$Site_ID, SC_huc_sites$Site_ID)
overlap_huc_sites <- SC_huc_sites[which(SC_huc_sites$Site_ID %in% overlap_huc_sites),]
class(overlap_huc_sites$Site_ID)
overlap_huc_sites$Site_ID <- factor(overlap_huc_sites$Site_ID)
levels(overlap_huc_sites$Site_ID)

saveRDS(overlap_huc_sites, "GBCO_dischSC_sites.rds") # 182

# overlap_huc_sites <- SC_huc_sites[(SC_huc_sites$Site_ID%in% disch_huc_sites$Site_ID), ]
# saveRDS(overlap_huc_sites, "GBCO_dischSC_sites.rds")

#saveRDS(disch_huc_sites, "disch_filtered_sites.rds")
#saveRDS(SC_huc_sites, "SC_filtered_sites.rds")

########