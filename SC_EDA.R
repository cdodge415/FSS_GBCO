###################################################################################################
# EDA = Exploratory Data Analysis 
# Most of this EDA was originally drafted using only USGS data. I am applying to all USGS and WQP data
# and adding additional data exploration
###################################################################################################
library(lubridate)
library(dplyr)
library(plyr)
library(ggplot2)
library(zoo)

# Read in specific conductance (SC) data 
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
SC <- readRDS("all_SC_data.rds")
# levels(SC$SiteID) # 25,624 sites
# class(SC)
SC <- as.data.frame(SC)
SC <- select(SC, -c("SiteDate"))

# add/format Date, Month, Year columns, only keep data 1900-2019
SC$Year <- year(SC$Date)
SC$Month <- month(SC$Date)
SC <- subset(SC, SC$Year >= "1900") 
SC <- subset(SC, SC$Year < "2020")
# SC$SiteID <- factor(SC$SiteID)
# levels(SC$SiteID) # 25,617 sites

# ouput final site list for spatial analysis (only need to do this once)
# sites <- SC
# class(sites)
# sites <- as.data.frame(sites)
# sites$Data <- "Specific Conductance"
# sites <- select(sites, c("SiteID", "Data"))
# setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta")
# write.csv(sites, "WQ_USGS_SC_site_list.csv")


# A look at # of measurements per site, per year, the period of record (POR), etc.

# SC_per_month <- plyr::count(SC, vars = "Month")
# There are definitely some large gaps here that may skew analysis of annual max and min's if using mode
# Graphic representation:
# SC$MonthAbb <- month.abb[SC$Month]
# SC$MonthAbb = factor(SC$MonthAbb, levels = month.abb)
# barplot(table(SC$MonthAbb))

# DATA AVAILABILITY/SUBSETTING ####
# see how many measurements there are in a year for each site-year
SC_per_sy <- plyr::count(SC, vars = c("SiteID","Year")) 
SC_per_sy$SiteYear <- paste0(SC_per_sy$SiteID, "_", SC_per_sy$Year)
SC$SiteYear <- paste0(SC$SiteID, "_", SC$Year) # add site-year to the full dataset
# 83,553 site-years

# SC_sub = a subset of data:
# Filter data for >= 12 data points per year. We are ASSUMING that this implies a monthly sampling 
# use subset for identifying timing of max and mins, range, etc. # use the full dataset to look at trends over time
SC_sub <- SC_per_sy[which(SC_per_sy$freq >= 12),] 
SC_sub <- SC[which(SC$SiteYear %in% SC_sub$SiteYear),] 
SC_sub$SiteYear <- factor(SC_sub$SiteYear) # get rid of unused levels by re-factoring
SC_sub$SiteID <- factor(SC_sub$SiteID) 
# levels (SC_sub$SiteYear)
# levels(SC_sub$SiteID) # 8872 site-years, 1603 sites, 552,496 observations

# SC_continuous = site-years with >= 350 measurements
SC_continuous <- SC_per_sy[which(SC_per_sy$freq >= 350),] # 517 site-years
SC_continuous$SiteID <- factor(SC_continuous$SiteID)
levels(SC_continuous$SiteID) # 93 sites
SC_continuous <- SC[which(SC$SiteYear %in% SC_continuous$SiteYear),]
class(SC_continuous$Year)
SC_continuous$Year <- as.factor(SC_continuous$Year)

SC_cont_POR <- SC_continuous %>%
  group_by(SiteID, Year) %>%
  summarise_at(.vars = "SpC", .funs = c("mean" = mean))

SC_cont_POR$PORadd <- "1"
SC_cont_POR$PORadd <- as.numeric(SC_cont_POR$PORadd)
SC_cont_POR <- as.data.frame(SC_cont_POR)
SC_cont_POR <- SC_cont_POR %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "PORadd", .funs = c("Years_cont" = sum))
# 88 sites with continuous data  (what happened to 93?)
library(tidyverse)
SC_cont_POR$Years_cont <- as.factor(SC_cont_POR$Years_cont)
print(paste(levels(SC_cont_POR$Years_cont)))
count(SC_cont_POR$Years_cont)
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 19, 21, 24)
y <- c(26, 12, 7, 6, 1, 5, 5, 4, 3, 6, 1, 2, 3, 2, 2, 1, 1, 1)
x_name <- "n_years"
y_name <- "n_occurrences"
cont <- data.frame(x,y)
names(cont) <- c(x_name,y_name)

ggplot(cont, aes(x=n_years, y = n_occurrences)) +
  labs(title = "Continuous SC Data", x = "# of Years", y = "# of Sites", subtitle = "n = 88 sites")+
  geom_col(alpha=.5, position="identity", colour = NA, fill = "lightseagreen")+
  theme_classic()+
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm"))+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(NA, 0), limits = c(0, 27))+
  scale_x_continuous(expand = c(0, NA), limits = c(0, 25))




# Heat map to look just at years of data available in the raw data and the subset
heatmap <- ggplot(SC, aes(x = Year, y = SiteID)) +
  geom_tile(color = "red", fill = "red")+
  theme(axis.text.y=element_blank())+
  xlim(1900,2020)+
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm"))+
  labs(title = "Data Availability", subtitle = "All SC Data")+
  theme(plot.title = element_text(hjust = 0.5),  plot.subtitle = element_text(hjust = 0.5))
  
sub_heatmap <- ggplot(SC_sub, aes(x = Year, y = SiteID)) +
  geom_tile(color = "red", fill = "red")+
  theme(axis.text.y=element_blank())+
  xlim(1900,2020)+
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm"))+
  labs(title = "Data Availability", subtitle = "SC Data with >= 12 obs. per Year")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# remake SC_continuous so Year works out here
cont_heatmap <- ggplot(SC_continuous, aes(x = Year, y = SiteID)) +
  geom_tile(color = "red", fill = "red")+
  theme(axis.text.y=element_blank())+
  xlim(1900,2020)+
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm"))+
  labs(title = "Data Availability", subtitle = "Continuous SC Data")+
  theme(plot.title = element_text(hjust = 0.5),  plot.subtitle = element_text(hjust = 0.5))

print(heatmap) # SC_heatmap
print(sub_heatmap) # SCsub_heatmap
print(cont_heatmap) # SCcont_heatmap

# #print(subw_heatmap)
# rm(heatmap, sub_heatmap)

# SC_sub_per_month <- plyr::count(SC_sub, vars = "Month")
# We have evened out the the number of measurements in each month a bit
SC_sub$MonthAbb <- month.abb[SC_sub$Month]
SC_sub$MonthAbb = factor(SC_sub$MonthAbb, levels = month.abb)
barplot(table(SC_sub$MonthAbb))
rm(heatmap, sub_heatmap, SC_per_month) # save space/make things move faster

# SUMMARY STATS ####
getmode <- function(v) { # create function to get the mode
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

tm <- function(x){ # create function for 25% trimmed mean
  mean(x, trim = 0.25)
}

iqr <- function(x){ # create function for IQr type 6
  IQR(x, type = 6)
}
# summary statistics by site (all sites)
SS <- SC %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "SpC", .funs = c("min" = min, "max"=max, "mean"=mean, "mode"=getmode, "median"=median, "sd"=sd, "tm"=tm, "IQR"=iqr))
SS$range <- SS$max-SS$min
SS$cv <- SS$sd/SS$mean

# summary statistics by site (subset of sites with at least monthly measurements)
SSsub <- SC_sub %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "SpC", .funs = c("min" = min, "max"=max, "mean"=mean, "mode"=getmode, "median"=median, "sd"=sd, "tm"=tm, "IQR"=iqr))
SSsub$range <- SSsub$max-SSsub$min
SSsub$cv <- SSsub$sd/SSsub$mean

# summary statistics by site-year (subset)
SSsubSY <- SC_sub %>%
  group_by(SiteID, Year) %>%
  summarise_at(.vars = "SpC", .funs = c("min" = min, "max"=max, "mean"=mean, "mode"=getmode, "median"=median, "sd"=sd, "tm"=tm, "IQR"=iqr))
SSsubSY$range <- SSsubSY$max-SSsubSY$min
SSsubSY$cv <- SSsubSY$sd/SSsubSY$mean

# VARIABILITY ####
# cv ####
# by site
ggplot(SSsub, aes(x=cv, fill=NULL)) +
  labs(title = "Coefficient of Variation by SiteID", x = "CV", y = "# of Sites")+
  geom_histogram(binwidth=.1, alpha=.5, position="identity", colour = "black", fill = "lightseagreen")+
  theme_classic()+
  xlim(0,2)+
  geom_vline(aes(xintercept=mean(cv, na.rm=T)), color="red", linetype="dashed", size=1)+
  scale_y_continuous(expand = c(NA, 0), limits = c(0, 400))+
  scale_x_continuous(expand = c(0, NA), limits = c(0, 2))+
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm"))+
  theme(plot.title = element_text(hjust = 0.5))
# by site-year
ggplot(SSsubSY, aes(x=cv, fill=NULL)) +
  labs(title = "Coefficient of Variation by Site-Year", x = "CV", y = "# of Site-Years")+
  geom_histogram(binwidth=.1, alpha=.5, position="identity", colour = "black", fill = "lightseagreen")+
  theme_classic()+
  xlim(0,2)+
  geom_vline(aes(xintercept=mean(cv, na.rm=T)), color="red", linetype="dashed", size=1)+
  scale_y_continuous(expand = c(NA, 0), limits = c(0, 2300))+
  scale_x_continuous(expand = c(0, NA), limits = c(0, 2))+
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm"))+
  theme(plot.title = element_text(hjust = 0.5))

# TIMING/ANNUAL TRENDS ####
# Mode ####
# MAX SC_sub timing
# find out what month of the year the maximum SC_sub occurs for each site-year
monthly_max <- SC_sub %>% 
  group_by(SiteID, Year) %>%
  slice(which.max(SpC))
monthly_max_count <- plyr::count(monthly_max, vars = "Month") # count how many site-years have their max SC_sub in each month
monthly_max_count$Month <- month.abb[monthly_max_count$Month]
monthly_max_count$Month = factor(monthly_max_count$Month, levels = month.abb)
ggplot(monthly_max_count)+
  geom_col(mapping = aes(x = Month, y = freq))

annual_avg_max <- monthly_max %>% # take averages of month of peak SC_sub for all years for each site
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_max_count <- plyr::count(annual_avg_max, vars = "mode") # choose which function you want to use and apply it to the following lines as well (i.e. mode, mean)
# count how many sites had their peak SC_sub in each month on "average" (mean) or "most years" (mode) (specify which function was used)
annual_avg_max_count$mode <- month.abb[annual_avg_max_count$mode]
annual_avg_max_count$mode = factor(annual_avg_max_count$mode, levels = month.abb)
ggplot(annual_avg_max_count)+
  geom_col(mapping = aes(x = mode, y = freq))+theme_classic()

# MIN SC_sub
# follow procedure for minimum SC_sub
monthly_min <- SC_sub %>%
  group_by(SiteID, Year) %>%
  slice(which.min(SpC))
monthly_min_count <- plyr::count(monthly_min, vars = "Month")
monthly_min_count$Month <- month.abb[monthly_min_count$Month]
monthly_min_count$Month = factor(monthly_min_count$Month, levels = month.abb)
ggplot(monthly_min_count)+
  geom_col(mapping = aes(x = Month, y = freq))

annual_avg_min <- monthly_min %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_min_count <- plyr::count(annual_avg_min, vars = "mode")
annual_avg_min_count$mode <- month.abb[annual_avg_min_count$mode]
annual_avg_min_count$mode = factor(annual_avg_min_count$mode, levels = month.abb)
ggplot(annual_avg_min_count)+
  geom_col(mapping = aes(x = mode, y = freq))+theme_classic()

# Mean ####
# Max SC_sub
# take averages of all years
annual_avg_max <- monthly_max %>% # take averages of month of peak SC_sub for all years for each site
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_max$mean <- round(annual_avg_max$mean)
annual_avg_max_count <- plyr::count(annual_avg_max, vars = "mean") # choose which function you want to use and apply it to the following lines as well
annual_avg_max_count$mean <- month.abb[annual_avg_max_count$mean]
annual_avg_max_count$mean = factor(annual_avg_max_count$mean, levels = month.abb)
ggplot(annual_avg_max_count)+
  geom_col(mapping = aes(x = mean, y = freq))

# Min SC_sub
annual_avg_min <- monthly_min %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_min$mean <- round(annual_avg_min$mean)
annual_avg_min_count <- plyr::count(annual_avg_min, vars = "mean")
annual_avg_min_count$mean <- month.abb[annual_avg_min_count$mean]
annual_avg_min_count$mean = factor(annual_avg_min_count$mean, levels = month.abb)
ggplot(annual_avg_min_count)+
  geom_col(mapping = aes(x = mean, y = freq))

# Rolling Mean ####
# Perhaps by looking at a 7-day average and then finding max and mins, we can dampen noisy data
# This is probably only applicable to the actual daily resolution data (~365 obs. per year), so create a new subset of data
# Mode (of rolling mean) ####
library(zoo)
SC_sub_rollmean <- SC_per_sy[which(SC_per_sy$freq >= 365),] 
SC_sub_rollmean <- SC[which(SC$SiteYear %in% SC_sub_rollmean$SiteYear),] 
SC_sub_rollmean$SiteYear <- factor(SC_sub_rollmean$SiteYear) # get rid of unused levels by re-factoring
SC_sub_rollmean$SiteID <- factor(SC_sub_rollmean$SiteID) # get rid of unused levels by re-factoring
SC_sub_rollmean$Rollmean <- rollmean(SC_sub_rollmean$SpC, 7, fill = NA, align = c("center")) # 7 == 7-day rolling mean (to potentially smooth out any odd 1-day outliers)
# Max SC_sub
monthly_max <- SC_sub_rollmean %>% 
  group_by(SiteID, Year) %>%
  slice(which.max(Rollmean))
monthly_max_count <- plyr::count(monthly_max, vars = "Month") 
monthly_max_count$Month <- month.abb[monthly_max_count$Month]
monthly_max_count$Month = factor(monthly_max_count$Month, levels = month.abb)
ggplot(monthly_max_count)+
  geom_col(mapping = aes(x = Month, y = freq))

annual_avg_max <- monthly_max %>% 
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_max_count <- plyr::count(annual_avg_max, vars = "mode") 
annual_avg_max_count$mode <- month.abb[annual_avg_max_count$mode]
annual_avg_max_count$mode = factor(annual_avg_max_count$mode, levels = month.abb)
ggplot(annual_avg_max_count)+
  geom_col(mapping = aes(x = mode, y = freq))

# Min SC_sub
monthly_min <- SC_sub_rollmean %>%
  group_by(SiteID, Year) %>%
  slice(which.min(Rollmean))
monthly_min_count <- plyr::count(monthly_min, vars = "Month")
monthly_min_count$Month <- month.abb[monthly_min_count$Month]
monthly_min_count$Month = factor(monthly_min_count$Month, levels = month.abb)
ggplot(monthly_min_count)+
  geom_col(mapping = aes(x = Month, y = freq))

annual_avg_min <- monthly_min %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_min_count <- plyr::count(annual_avg_min, vars = "mode")
annual_avg_min_count$mode <- month.abb[annual_avg_min_count$mode]
annual_avg_min_count$mode = factor(annual_avg_min_count$mode, levels = month.abb)
ggplot(annual_avg_min_count)+
  geom_col(mapping = aes(x = mode, y = freq))

# Mean (of rolling mean) #### 
# Max SC_sub
annual_avg_max <- monthly_max %>% # take averages of month of peak SC_sub for all years for each site
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_max$mean <- round(annual_avg_max$mean)
annual_avg_max_count <- plyr::count(annual_avg_max, vars = "mean") 
annual_avg_max_count$mean <- month.abb[annual_avg_max_count$mean]
annual_avg_max_count$mean = factor(annual_avg_max_count$mean, levels = month.abb)
ggplot(annual_avg_max_count)+
  geom_col(mapping = aes(x = mean, y = freq))

# Min SC_sub
annual_avg_min <- monthly_min %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_min$mean <- round(annual_avg_min$mean)
annual_avg_min_count <- plyr::count(annual_avg_min, vars = "mean")
annual_avg_min_count$mean <- month.abb[annual_avg_min_count$mean]
annual_avg_min_count$mean = factor(annual_avg_min_count$mean, levels = month.abb)
ggplot(annual_avg_min_count)+
  geom_col(mapping = aes(x = mean, y = freq))



# MAPPING ####
# bring in site metadata
library(viridis)
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
site_data <- readRDS("GBCO_dischSC_sites.rds")
colnames(site_data)
site_data <- select(site_data, c("Site_ID","station_nm", "Lat", "Lon", "huc_cd"))
site_data <- site_data[which(site_data$Site_ID %in% SC$SiteID),]
site_data <- unique(site_data)
site_data <- merge(site_data, lm_slope, by.x = "Site_ID", by.y = "names")

some.states <- c('california', 'nevada', 'utah', 'wyoming', 'colorado', 'arizona', 'new mexico')
some.states.map <- map_data("state", region = some.states)

# to make visualization with colors easier for now:
site_data <- site_data[-which(site_data$Site_ID == "09179200"),]
site_data <- site_data[-which(site_data$Site_ID == "10172630"),]
site_data <- site_data[-which(site_data$Site_ID == "09153290"),]
#

ggplot(some.states.map)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "white", color = "black")+
  geom_point(site_data, mapping = aes(x = Lon, y = Lat, color = slopes))+
  scale_x_continuous(limits = c(-125,-101))+
  theme(panel.background = element_blank(),axis.title = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), axis.text = element_blank(), plot.title = element_text(hjust = 0.75))+
  labs(title = "Slope of USGS Daily SC Linear Models", color = "ΔSC/Time")+
  scale_color_viridis(option = "A")
#scale_color_gradientn(colours = rainbow(2), trans = "reverse")

# PLOT TS ####
# (time_plot <- ggplot(SC_sub, aes(x = Date, y = SpC, color = SiteID)) +
#    geom_line() +
#    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
#    theme_classic()+
#    theme(legend.position = "none"))+
#   facet_wrap(~ SiteID)

# Great example TS:
ggplot(subset(SC, SiteID == "10343500"), aes(x = Date, y = SpC))+
  geom_line()+
  geom_smooth(method = "lm")+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_classic()+
  theme(legend.position = "none")
# Interesting way to plot max and mins
# install.packages("ggpmisc")  
# library(ggpmisc)
# 
# ggplot(data = subset(SC_sub, SC_sub$SiteID == "09230300" & SC_sub$Year == "2008"))+
#   geom_line(mapping = aes(x = Date, y = SpC))+
#   stat_peaks(mapping = aes(x = Date, y = SpC),color = "green")+
#   stat_valleys(mapping = aes(x = Date, y = SpC),color = "red")


# SAVE DATA FILES ####
saveRDS(SCd, "SCd.RDS")
saveRDS(SCd_sub, "SCd_sub.RDS")
saveRDS(SCd_sub2, "SCd_sub_sym.RDS")
