setwd("C:/Users/cdodg/Desktop/Blaszczak lab/FSS project/Data from Phil")
#data is for all of US? need to figure out which observations
#are for GB and CO river basins (HUC 14, 15, 16)
#call packages from library
#library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)
#theme_set(theme_pubr())
library(tidyverse)
library(dplyr)
#read data
disch_data <- readRDS("disch_query.rds")
SC_data <- readRDS("SC_query.rds")
#bar plot of hucIDs
#ggplot(disch_data, aes(huc_cd)) +
#geom_bar(fill = "#0073C2FF") +
#theme_pubclean()
#confirmed data is for all of US
#need to filter for HUC 14,15,16 with >=365 days of obs
disch_huc_sites <- disch_data %>% filter(str_detect(huc_cd, "^14|^15|^16") & count_nu>=365) %>% select(Site_ID,station_nm,Lat,Lon,huc_cd,parm_cd,begin_date,end_date,count_nu,site_tp_cd,data_type_cd,access_cd)
SC_huc_sites <- SC_data %>% filter(str_detect(huc_cd, "^14|^15|^16") & count_nu>=365) %>% select(Site_ID,station_nm,Lat,Lon,huc_cd,parm_cd,begin_date,end_date,count_nu,site_tp_cd,data_type_cd,access_cd)
saveRDS(disch_huc_sites, "GBCO_disch_sites.rds")
saveRDS(SC_huc_sites, "GBCO_SC_sites.rds")

overlap_huc_sites <- SC_huc_sites[(SC_huc_sites$Site_ID%in% disch_huc_sites$Site_ID), ]
saveRDS(overlap_huc_sites, "GBCO_dischSC_sites.rds")
#saveRDS(disch_huc_sites, "disch_filtered_sites.rds")
#saveRDS(SC_huc_sites, "SC_filtered_sites.rds")

########