rm(list = ls())
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
# setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
#---------------------
# Read in SC qw Data ####
#---------------------
SC <- readRDS("USGS_SC_qw_data.rds")
class(SC$dqi_cd)
SC$dqi_cd <- as.factor(SC$dqi_cd)
levels(SC$dqi_cd)
# A = historical data
# R = reviewed and approced
# S = provisional
# water quality data with all three of these dqi's were used in the streamgage matching document
# so we do not need to subset this data at all. we will use all of it. 

colnames(SC)
SC <- select(SC, c("site_no", "sample_dt", "sample_end_dt","sample_tm", "sample_end_tm", "result_va"))
colnames(SC) <- c("SiteID", "Date","End_Date","Time", "End_Time","Specific.conductance")
# SC$DateTime <- ifelse(!is.na(SC$Time), paste0(SC$Date, " ", SC$Time), paste(SC$Date))
sapply(SC, class)
SC$SiteID <- as.factor(SC$SiteID)
levels(SC$SiteID)
# 46 qw sites

# we need to see if there are days with multiple values and if they are, average them to get one value per day of sampling
SC$SiteDate <- paste(SC$SiteID,SC$Date, sep = " ")
class(SC$SiteDate)
SC$SiteDate <- as.factor(SC$SiteDate)
levels(SC$SiteDate) # 38,268
unique(SC$SiteDate) # 38,268
SC_sub <- SC
SC_sub <- SC[duplicated(SC[7]),] # look at these and confirm that there are days with multiple measurements
# confirmed: we have days of point sampling where multiple samples were taken, so average by date to get daily data
rm(SC_sub)
SC <- SC %>%
  group_by(SiteID, Date, SiteDate) %>%
  summarise_at(.vars = "Specific.conductance", .funs = c("mean"=mean))
SC$mean <- round(SC$mean, digits = 0) # create a dataframe of data made from point data with no more than 1 value per day

# write new file with naming convention of other data quality filtered files just to avoid confusion
# even though we ended up keeping all of the data
saveRDS(SC, "USGS_SC_qw_dqi.rds")
# NOTE: right now, the data is just using the first day of a multi-day sample to represent the value of SC
# ask Phil if this is correct

#################################SCRATCH BELOW CODE (8/21/20 after final data download)#############################################
# #---------------------
# # Create year and month column ####
# #---------------------
# sapply(SC, class)
# SC$Date <- ymd(SC$Date)
# SC$Year <- year(SC$Date)
# SC$Month <- month(SC$Date)
# 
# # Split by SiteID and count # of measurements per year
# # SC_split <- split(SC, SC$SiteID)
# # SC_split_ct <- lapply(SC_split,plyr::count,  vars = "Year")
# 
# # Stats by Site-year
# site_year_stats <- SC %>% 
#   group_by(SiteID, Year) %>%
#   summarise_at(.vars = "Specific.conductance", .funs = c("max" = max, "min" = min, "mean" = mean, "sd" = sd))
# site_year_stats$range <- (site_year_stats$max-site_year_stats$min)
# # 4530 site years of SC data from USGS
# 
# # find # of measurements per site-year
# site_year_count <- SC %>% 
#   group_by(SiteID, Year) %>%  
#   dplyr::summarise(n()) 
# #site_year_split <- split(site_year_split, site_year$SiteID)
# # site_year$range <- ifelse(site_year$max == site_year$min, NA, paste(site_year$range))
# 
# # split SC data by SiteID
# SC_site_split <- split(SC, SC$SiteID)
# 
# # Stats by SiteID
# site_stats <- SC %>% 
#   group_by(SiteID) %>%
#   summarise_at(.vars = "Specific.conductance", .funs = c("max" = max, "min" = min, "mean" = mean, "sd" = sd, "n" = n))
# site$range <- (site$max - site$min)
# # 174 sites
# 
# # Day of year
# #dat$Date <- paste0(month(dat$DateTime), "-", day(dat$DateTime))
# SC$doy <- strftime(SC$Date, format = "%j")
# SC$doy <-as.numeric(SC$doy)
# 
# 
# #----------------
# # Site-Year
# #----------------
# # Max SC
# monthly_max <- SC %>%
#   group_by(SiteID, Year) %>%
#   slice(which.max(Specific.conductance))
# monthly_max_count <- plyr::count(monthly_max, vars = "Month")
# class(monthly_max_count$Month)
# monthly_max_count$Month <- month.abb[monthly_max_count$Month]
# monthly_max_count$Month = factor(monthly_max_count$Month, levels = month.abb)
# # 560 site-years had their max SC in March (the majority) 462 in November
# # versus only 156 in June and 157 in May 
# ggplot(monthly_max_count)+
#   geom_col(mapping = aes(x = Month, y = freq))
# # create function to get the mode
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# 
# annual_avg_max <- monthly_max %>%
#   group_by(SiteID) %>%
#   summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=mean, "sd"=sd))
# 
# annual_avg_max_count <- plyr::count(annual_avg_max, vars = "mode")
# class(annual_avg_max_count$mode)
# annual_avg_max_count$mode <- month.abb[annual_avg_max_count$mode]
# annual_avg_max_count$mode = factor(annual_avg_max_count$mode, levels = month.abb)
# ggplot(annual_avg_max_count)+
#   geom_col(mapping = aes(x = mode, y = freq))
# # 33 sites had their average annual max SC in March, 21 in August
# # 3 in May, 6 in June
# 
# # Min SC
# monthly_min <- SC %>%
#   group_by(SiteID, Year) %>%
#   slice(which.min(Specific.conductance))
# monthly_min_count <- plyr::count(monthly_min, vars = "Month")
# class(monthly_min_count$Month)
# monthly_min_count$Month <- month.abb[monthly_min_count$Month]
# monthly_min_count$Month = factor(monthly_min_count$Month, levels = month.abb)
# # 1204 site-years had their min SC in May (the majority) 1123 June
# # versus only 127 in September and 136 in December
# ggplot(monthly_min_count)+
#   geom_col(mapping = aes(x = Month, y = freq))
# 
# annual_avg_min <- monthly_min %>%
#   group_by(SiteID) %>%
#   summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=mean, "sd"=sd))
# 
# annual_avg_min_count <- plyr::count(annual_avg_min, vars = "mode")
# class(annual_avg_min_count$mode)
# annual_avg_min_count$mode <- month.abb[annual_avg_min_count$mode]
# annual_avg_min_count$mode = factor(annual_avg_min_count$mode, levels = month.abb)
# ggplot(annual_avg_min_count)+
#   geom_col(mapping = aes(x = mode, y = freq))
# #need to add missing months somehow
# # 63 sites had their average annual minimum SC in May, 47 in June
# # 1 in December, 3 in October
# # I am suspicious of sites who have the same min/max and/or only have one measurement
# 
# 
# #------------------------
# # Data exploration plots
# #------------------------
# # Subset for sites in CO for an easy comparison of data availability in USGS data vs WQP data #
# subSC <- subset(SC, site_no %in% c("09014050", "09041090", "09041400", "09071750", 
#                                    "09085150", "09095500", "09136100", "09144250",
#                                    "09147500", "09149500", "09152500", "09163500",
#                                    "09169500", "09171100", "09251000", "09358000",
#                                    "09358550", "09359010", "09359020", "09361500",
#                                    "09371492", "09371520", "383926107593001", "394220106431500"))
# # Use 2019 to demonstrate difference between USGS and WQP
# subSC <- subset(SC, year(Date) == "2019")
# # Plot USGS
# USGS_SC_2019 <- ggplot(SCa = subSC, mapping = aes(x = Date, y = result_va, color = site_no))+
#   geom_point()+
#   theme(legend.position = "none")+
#   labs(y = "SC", x = "SCeTime", title = "USGS")
# # Compare USGS plot and WQP plot
# compare <- ggarrange(USGS_SC_2019, WQP_SC_2019, nrow = 2, ncol = 1)
# compare <- annotate_figure(compare, top = "CO Sites")
# print(compare)
# 
# 
# #plot years on top of each other for the same site
# # changed year to factor for multiple colors
# library(viridis)
# SC$Year <- as.numeric(SC$Year)
# # Colorado
# SC$Year <- as.factor(SC$Year)
# SC$SiteID <- paste0(SC$agency_cd, "-", SC$site_no)
# 
# ggplot(subset(SC, SiteID %in% "USGS-09371492"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_smooth(stat = "smooth", position = "identity", na.rm = TRUE, se = FALSE)+
#   geom_point()+
#   # theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# 
# ggplot(subset(SC, SiteID %in% "USGS-09163500"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_smooth(stat = "smooth", position = "identity", na.rm = TRUE, se = FALSE)+
#   geom_point()+
#   #theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# 
# ggplot(subset(SC, SiteID %in% "USGS-394220106431500"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_smooth(stat = "smooth", position = "identity", na.rm = TRUE, se = FALSE)+
#   geom_point()+
#   # theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# 
# 
# ggplot(subset(SC, SiteID %in% "USGS-394220106431500"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_smooth(stat = "smooth", position = "identity", na.rm = TRUE, se = FALSE)+
#   geom_point()+
#   #theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# #also a strong pattern for USGS-09371520, USGS-09041400, USGS-09163500
# 
# # Truckee
# #10348200	10350500	10351600	10351700
# ggplot(subset(SC, SiteID %in% "USGS-10350500"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_line()+
#   geom_point()+
#   #theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# 
# ggplot(subset(SC, SiteID %in% "USGS-10348200"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_line()+
#   geom_point()+
#   #theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# 
# ggplot(subset(SC, SiteID %in% "USGS-10351600"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_line()+
#   geom_point()+
#   #theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# 
# ggplot(subset(SC, SiteID %in% "USGS-10351700"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_line()+
#   geom_point()+
#   #theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# 
# 
# # Walker
# #10301500	10302002  10302025	
# ggplot(subset(SC, SiteID %in% "USGS-10302002"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_smooth(stat = "smooth", position = "identity", na.rm = TRUE, se = FALSE)+
#   geom_point()+
#   #theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# 
# ggplot(subset(SC, SiteID %in% "USGS-10301500"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_smooth(stat = "smooth", position = "identity", na.rm = TRUE, se = FALSE)+
#   geom_point()+
#   #theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# 
# ggplot(subset(SC, SiteID %in% "USGS-10302025"), mapping = aes(x = doy, y = result_va, color = Year))+
#   #geom_smooth(stat = "smooth", position = "identity", na.rm = TRUE, se = FALSE)+
#   geom_point()+
#   #theme(legend.position = "none")+
#   scale_color_viridis(option = "D")
# 

