rm(list = ls())
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
# setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
#---------------------
# Read in SC Data ####
#---------------------
SC <- readRDS("USGS_SC_uv_data.rds")
colnames(SC)
SC <- select(SC, -c("agency_cd"))
colnames(SC) <- c("SiteID", "DateTime", "Specific.Conductance", "dqi", "tz_cd")
sapply(SC, class)
SC$dqi <- as.factor(SC$dqi)
levels(SC$dqi)
# [1] "A"      "A [0]"  "A [4]"  "A [92]" "A [93]" "A e"    "A R"    "P"      "P Dis"  "P Dry" 
# [11] "P Eqp"  "P Ice"  "P Mnt"  "P Ssn" 
# for now, in order to re-run the exploratory data analysis, lets filter for all types of A, AR, and Ae
SC <- subset(SC, dqi == "A" | dqi == "A [0]" | dqi == "A [4]" | dqi == "A [92]" | dqi == "A [93]" |
               dqi == "A e" | dqi == "A R")
SC$SiteID <- as.factor(SC$SiteID)
levels(SC$SiteID)
# 72 sites

# The smallest resolution of data we want is daily, so create it from the 15-minute data
# Take daily means, compare to USGS dv data
SC$Date <- as.Date(SC$DateTime)
SC_daily <- SC %>%
  group_by(SiteID ,Date) %>%
  summarise_at(.vars = "Specific.Conductance", .funs = c("mean"=mean))
SC_daily$mean <- round(SC_daily$mean, digits = 0)
  
colnames(SC_daily)[3] <- "Specific.Conductance"
# bring in dv SC data to compare
SCdv <- readRDS("USGS_SC_dv_dqi.rds")
common_sites <- intersect(SC_daily$SiteID, SCdv$SiteID) # 53 sites overlap 

SC_daily$Site_Date <- paste0(SC_daily$SiteID, "_", SC_daily$Date)
SCdv$DateTime <- date(SCdv$DateTime)
SCdv$Site_Date <- paste0(SCdv$SiteID, "_", SCdv$Date)
common_site_date <- intersect(SC_daily$Site_Date, SCdv$Site_Date) # 133878 site-dates overlap
# if you take a site_date from the new SC_daily data and compare it to the same site_date from the 
# USGS daily data, the values are the same. now, we just need to make sure we aren't using duplicates and subset the rest

# find the exact differences so we know what we are adding by making daily data from unit value data
# we will put these in separate dataframes and append them to the dataframe of overlapping data
diff1 <- setdiff(SC_daily$Site_Date, SCdv$Site_Date) #51541 in SC_daily that are not in SCdv
diff2 <- setdiff(SCdv$Site_Date, SC_daily$Site_Date) #232380 in SCdv that are not in SC_daily

SCuv_d <- SC_daily[which(SC_daily$Site_Date %in% diff1),] # create dataframe for the daily data we are adding by aggregating unit value data
# without adding duplicate data


#check
factor(SCuv_d$Site_Date)
# 51541 levels == 51541 site-dates/observations so we are good
SC_check <- unique(SCuv_d)
rm(SC_check) 

# get some info
sapply(SCuv_d, class)
SCuv_d$SiteID <- factor(SCuv_d$SiteID)
levels(SCuv_d$SiteID) # 54,541 sites

#output new unit value data (that has been turned into daily data)
getwd()
saveRDS(SCuv_d, "USGS_SC_uv_daily_dqi.rds")

##################### SCRATCH THE REST (8/21/20 after doing the final data downloads) #########
# The rest of this script was made before we analyzed the differences and overlaps of the dv and summarized uv data
# #---------------------
# # Create year and month column ####
# #---------------------
# sapply(SCuv, class)
# SCuv$DateTime <- ymd_hms(SCuv$DateTime)
# SCuv$Year <- year(SCuv$DateTime)
# # get rid of any data from 2020
# SCuv <- SCuv[which(SCuv$Year < "2020"),]
# SCuv$Month <- month(SCuv$DateTime)
# 
# # Split by SiteID and count # of measurements per year
# # SCuv_split <- split(SCuv, SCuv$SiteID)
# # SCuv_split_ct <- lapply(SCuv_split,plyr::count,  vars = "Year")
# 
# # Stats by Site-year
# site_year_stats <- SCuv %>% 
#   group_by(SiteID, Year) %>%
#   summarise_at(.vars = "SpC", .funs = c("max" = max, "min" = min, "mean" = mean, "sd" = sd))
# site_year_stats$range <- (site_year_stats$max-site_year_stats$min)
# # 578 site years of uv SCuv data from USGS
# # try box plot?
# 
# # find # of measurements per site-year
# site_year_count <- SCuv %>% 
#   group_by(SiteID, Year) %>%  
#   dplyr::summarise(n()) 
# #site_year_split <- split(site_year_split, site_year$SiteID)
# # site_year$range <- ifelse(site_year$max == site_year$min, NA, paste(site_year$range))
# 
# # split SCuv data by SiteID
# #SCuv_site_split <- split(SCuv, SCuv$SiteID)
# 
# # Stats by SiteID
# site_stats <- SCuv %>% 
#   group_by(SiteID) %>%
#   summarise_at(.vars = "SpC", .funs = c("max" = max, "min" = min, "mean" = mean, "sd" = sd))
# site_stats$range <- (site_stats$max - site_stats$min)
# # 68 sites
# 
# # Day of year
# #dat$Date <- paste0(month(dat$DateTime), "-", day(dat$DateTime))
# # SCuv$doy <- strftime(SCuv$Date, format = "%j")
# # SCuv$doy <-as.numeric(SCuv$doy)
# 
# 
# #----------------
# # Site-Year
# #----------------
# # Max SCuv
# monthly_max <- SCuv %>%
#   group_by(SiteID, Year) %>%
#   slice(which.max(SpC))
# monthly_max_count <- plyr::count(monthly_max, vars = "Month")
# class(monthly_max_count$Month)
# monthly_max_count$Month <- month.abb[monthly_max_count$Month]
# monthly_max_count$Month = factor(monthly_max_count$Month, levels = month.abb)
# # 103 site-years had their max SCuv in Dec (the majority)
# # versus only 12 in June and 13 in May 
# ggplot(monthly_max_count)+
#   geom_col(mapping = aes(x = Month, y = freq))
# # take averages of all years
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
# # 21 sites had their average annual max SCuv in December, 7 in Nov and July
# # 1 in June, 2 in March and May
#   
# # Min SCuv
# monthly_min <- SCuv %>%
#   group_by(SiteID, Year) %>%
#   slice(which.min(SpC))
# monthly_min_count <- plyr::count(monthly_min, vars = "Month")
# class(monthly_min_count$Month)
# monthly_min_count$Month <- month.abb[monthly_min_count$Month]
# monthly_min_count$Month = factor(monthly_min_count$Month, levels = month.abb)
# # 187 site-years had their min SCuv in June (the majority)
# # 117 in May (close to majority)
# # versus only 4 in February and 8 in November
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
# # 30 sites had their average annual minimum SCuv in June, 12 in May
# # 1 in March, 2 in April and November



