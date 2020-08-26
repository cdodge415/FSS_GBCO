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
# levels (SC_sub$SiteYear) # 8872 site-years
# levels(SC_sub$SiteID) # 1603 sites
# 593,590 observations

# Filter data for >= 52 data points per year
# We are ASSUMING that this implies a weekly sampling
# SC_sub_w <- SC_per_sy[which(SC_per_sy$freq >= 52),] 
# SC_sub_w <- SC[which(SC$SiteYear %in% SC_sub_w$SiteYear),] 
# SC_sub_w$SiteYear <- factor(SC_sub_w$SiteYear) # get rid of unused levels by re-factoring
# SC_sub_w$SiteID <- factor(SC_sub_w$SiteID) # get rid of unused levels by re-factoring

# Heat map to look just at years of data available in the raw data and the subset
heatmap <- ggplot(SC, aes(x = Year, y = SiteID)) + 
  geom_tile(color = "red")+
  theme(axis.text.y=element_blank())+
  xlim(1900,2020)
sub_heatmap <- ggplot(SC_sub, aes(x = Year, y = SiteID)) + 
  geom_tile(color = "red")+
  theme(axis.text.y=element_blank())+
  xlim(1900,2020)
#subw_heatmap <- ggplot(SC_sub_w, aes(x = Year, y = SiteID)) + 
  # geom_tile(color = "red")+
  # theme(axis.text.y=element_blank())+
  # xlim(1900,2020)
print(heatmap)
print(sub_heatmap)
#print(subw_heatmap)
rm(heatmap, sub_heatmap)

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
