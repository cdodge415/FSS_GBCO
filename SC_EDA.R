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
rm(heatmap, sub_heatmap, SC_per_month, SC_per_sy) # save space/make things move faster

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

SS <- SC %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "SpC", .funs = c("min" = min, "max"=max, "mean"=mean, "mode"=getmode, "median"=median, "sd"=sd, "tm"=tm, "IQR"=iqr))
SS$range <- SS$max-SS$min
SS$cv <- SS$sd/SS$mean

SSsub <- SC_sub %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "SpC", .funs = c("min" = min, "max"=max, "mean"=mean, "mode"=getmode, "median"=median, "sd"=sd, "tm"=tm, "IQR"=iqr))
SSsub$range <- SSsub$max-SSsub$min
SSsub$cv <- SSsub$sd/SSsub$mean


ggplot(SSsub, aes(x = cv))+
  geom_density(fill = "gray")+
  theme_classic()

# TIMING/ANNUAL TRENDS ####
# MODE ####
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
  geom_col(mapping = aes(x = mode, y = freq))
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
  geom_col(mapping = aes(x = mode, y = freq))

# MEAN ####
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

# ROLLING MEAN ####
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


# VARIABILITY by MONTH ####
# SC_sub ####
# by month # NOTE: I'm not sure it makes much sense to look at variability this way...
# SC_sub_monthly <- SC_sub %>%
#   group_by(Month) %>% 
#   summarise_at(.vars = "SpC", .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean))
# SC_sub_monthly$range <- SC_sub_monthly$max-SC_sub_monthly$min
# SC_sub_monthly$cv <- SC_sub_monthly$sd/SC_sub_monthly$mean
# SC_sub_monthly$Month <- month.abb[SC_sub_monthly$Month]
# SC_sub_monthly$Month = factor(SC_sub_monthly$Month, levels = month.abb)
# ggplot(SC_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = cv))
# ggplot(SC_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = range))

# by site-month
# subset further to only include months with >= 4 obs. per month 
# We are ASSUMING this implies a weekly sampling
SC_per_sym <- plyr::count(SC, vars = c("SiteID", "Year", "Month")) 
SC_per_sym$SiteYearMonth <- paste0(SC_per_sym$SiteID, "_", SC_per_sym$Year, "_", SC_per_sym$Month)
# add site-year-month to the full dataset
SC$SiteYearMonth <- paste(SC$SiteID, SC$Year, SC$Month, sep = "_")

# SC_sub2 = a subset of data:
# try filtering data for >= 4 data points per site-year-month (so in a specific month of any given year, must have >= 4 obs.)
# We are ASSUMING that this implies a monthly sampling ####
SC_sub2 <- SC_per_sym[which(SC_per_sym$freq >= 4),] 
SC_sub2 <- SC[which(SC$SiteYearMonth %in% SC_sub2$SiteYearMonth),] 
SC_sub2$SiteYearMonth <- as.factor(SC_sub2$SiteYearMonth)
SC_sub2$SiteYearMonth <- factor(SC_sub2$SiteYearMonth) # get rid of unused levels by re-factoring
SC_sub2$SiteID <- factor(SC_sub2$SiteID) # get rid of unused levels by re-factoring
# levels(SC_sub2$SiteYearMonth) # 21,987 site-year-months
# levels(SC_sub2$SiteID) # 928 sites
# 356,316 observations
SC_sub2_monthly <- SC_sub2 %>%
  group_by(SiteID, Year, Month) %>% 
  summarise_at(.vars = "SpC", .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean, "IQR"=iqr, "tm"=tm))
SC_sub2_monthly$range <- SC_sub2_monthly$max-SC_sub2_monthly$min
SC_sub2_monthly$cv <- SC_sub2_monthly$sd/SC_sub2_monthly$mean
SC_sub2_monthly$Month <- month.abb[SC_sub2_monthly$Month]
SC_sub2_monthly$Month = factor(SC_sub2_monthly$Month, levels = month.abb)
ggplot(SC_sub2_monthly)+
  geom_col(mapping = aes(x = Month, y = cv))
ggplot(SC_sub2_monthly)+
  geom_col(mapping = aes(x = Month, y = range))
# this is better because each month in time at each site is given = weight
# summarize cv by month
SC_sub2_monthly <- SC_sub2_monthly[complete.cases(SC_sub2_monthly), ]
SC_sub2_cv <- SC_sub2_monthly %>%
  group_by(Month) %>%
  summarise_at(.vars = "cv", .funs = c("min" = min, "max" = max, "mean" = mean))
SC_sub2_cv$mean <- round(SC_sub2_cv$mean, digits = 2)
ggplot(SC_sub2_cv)+
  geom_col(mapping = aes(x = Month, y = mean))

SC_sub2_range <- SC_sub2_monthly %>%
  group_by(Month) %>%
  summarise_at(.vars = "range", .funs = c("min" = min, "max" = max, "mean" = mean, "median" = median))

SC_sub2_iqr <- SC_sub2_monthly %>%
  group_by(Month) %>%
  summarise_at(.vars = "IQR", .funs = c("min" = min, "max" = max, "mean" = mean, "median" = median))

# by site
SC_sub_monthly <- SC_sub %>%
  group_by(SiteID) %>% 
  summarise_at(.vars = c("SpC"), .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean))
SC_sub_monthly$range <- SC_sub_monthly$max-SC_sub_monthly$min
SC_sub_monthly$cv <- SC_sub_monthly$sd/SC_sub_monthly$mean
#SC_sub_monthly$Month <- month.abb[SC_sub_monthly$Month]
#SC_sub_monthly$Month = factor(SC_sub_monthly$Month, levels = month.abb)
cv <-
  ggplot(SC_sub_monthly, aes(x = cv))
cv + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(cv)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

# SC ####
# all sites by month
# SC_sub_monthly <- SC_sub %>%
#   group_by(Month) %>% 
#   summarise_at(.vars = "SpC", .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean))
# SC_sub_monthly$range <- SC_sub_monthly$max-SC_sub_monthly$min
# SC_sub_monthly$cv <- SC_sub_monthly$sd/SC_sub_monthly$mean
# SC_sub_monthly$Month <- month.abb[SC_sub_monthly$Month]
# SC_sub_monthly$Month = factor(SC_sub_monthly$Month, levels = month.abb)
# ggplot(SC_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = cv))
# ggplot(SC_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = range))

# by site-month
# SC_sub_monthly <- SC_sub %>%
#   group_by(SiteID, Month) %>% 
#   summarise_at(.vars = "SpC", .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean))
# SC_sub_monthly$range <- SC_sub_monthly$max-SC_sub_monthly$min
# SC_sub_monthly$cv <- SC_sub_monthly$sd/SC_sub_monthly$mean
# SC_sub_monthly$Month <- month.abb[SC_sub_monthly$Month]
# SC_sub_monthly$Month = factor(SC_sub_monthly$Month, levels = month.abb)
# ggplot(SC_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = cv))
# ggplot(SC_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = range))
# 
# # by site
# SC_sub_monthly <- SC_sub %>%
#   group_by(SiteID) %>% 
#   summarise_at(.vars = c("SpC"), .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean))
# SC_sub_monthly$range <- SC_sub_monthly$max-SC_sub_monthly$min
# SC_sub_monthly$cv <- SC_sub_monthly$sd/SC_sub_monthly$mean
## SC_sub_monthly$Month <- month.abb[SC_sub_monthly$Month]
## SC_sub_monthly$Month = factor(SC_sub_monthly$Month, levels = month.abb)



# LINEAR MODEL ####
# https://www.theanalysisfactor.com/linear-models-r-plotting-regression-lines/

# SC ####
SC$SiteID <- factor(SC$SiteID) # double check there are no implicit NAs in SiteID to avoid errors with splitting
SC_split <- split(SC, SC$SiteID)
SC_split <- SC_split[sapply(SC_split, function(x) dim(x)[1]) > 0]
# calculate the fit models per data frame
fits <- lapply( SC_split, function(x) {
  lm( formula = SpC ~ Date, data = x )
} )

# understand what we get from the model
#https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R#:~:text=The%20R%2Dsquared%20(R2,%2F%20target%20variable%20(dist).&text=That%20why%20we%20get%20a%20relatively%20strong%20R2.
#summary(fits[["09149500"]])

# extract the slope from all models
slopes <- sapply( fits, function(x) x$coefficients )
slopes <- slopes[2,]

# print one of the results to see it
slopes[1]
print(slopes)

# create a df with the SiteID's and slopes of linear models
names <- names(SC_split)
lm_slope <- cbind(names, slopes)
lm_slope <- as.data.frame(lm_slope)
class(lm_slope$slopes)
lm_slope$slopes <- as.numeric(as.character(lm_slope$slopes))
lm_slope$slopes <- round(lm_slope$slopes, digits = 3)
# NOTE: see how having more recent data shifts the trends to more commonly be increasing

# Plot slopes to see what kind of change is occurring over time, and in how many sites
sub_slope <- ggplot(lm_slope, aes(x = slopes))+
  xlim(-5,5)
# sub_slope + geom_density() +
#   geom_vline(aes(xintercept = mean(slopes)), 
#              linetype = "dashed", size = 0.6)
sub_slope + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = median(slopes)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

# extract the R squared and add it to the df with the slopes and Site ID's
rsquared <- sapply(fits, function(x) summary(x)$r.squared )
lm_slope$rsquared <- rsquared
lm_slope$rsquared <- round(lm_slope$rsquared, 4)
# I think it's to be expected that the R2 for a time series of something with annual fluctations would be bad
# so how else do we measure if the lm is a good estimate of the long term trend?


# SC sub ####
SC_sub_split <- split(SC_sub, SC_sub$SiteID)
SC_sub_split <- SC_sub_split[sapply(SC_sub_split, function(x) dim(x)[1]) > 0] # get rid of any empty dfs

# calculate the fit models per data frame
fits <- lapply( SC_sub_split, function(x) {
  lm( formula = SpC ~ Date, data = x )
} )

# extract the slope from all models
slopes <- sapply( fits, function(x) x$coefficients )
slopes <- slopes[2,]

# print one of the results to see it
slopes[1]
print(slopes)

# create a df with the SiteID's and slopes of the linear models
names <- names(SC_sub_split)
lm_slope <- cbind(names, slopes)
lm_slope <- as.data.frame(lm_slope)
lm_slope$slopes <- as.numeric(as.character(lm_slope$slopes))
lm_slope$slopes <- round(lm_slope$slopes, digits = 3)

# Plot slopes to see what kind of change is occurring over time, and in how many sites
sub_slope <- ggplot(lm_slope, aes(x = slopes))+
  xlim(-5,5)

# sub_slope + geom_density() +
#   geom_vline(aes(xintercept = mean(slopes)), 
#              linetype = "dashed", size = 0.6)
sub_slope + geom_density(aes(y = ..count..), fill = "lightgray")+
  geom_vline(aes(xintercept = median(slopes)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")
# NOTE: refer to sub_heatmap to see what data is available in this subset and how lack of 
# more recent continuous daily data could be affecting what we see here in the linear models


# Lastly, see how yearly averages are changing over time
# ANNUAL AVERAGE TRENDS ####
# SC_sub
annual_avg <- SC%>% # quartiles might also be good to look at
  group_by(SiteID, Year) %>% 
  summarise_at(.vars = "SpC", .funs = c("mean"=mean, "median"=mean))
# linear model of annual averages
SC_annual_avg_split <- split(annual_avg, annual_avg$SiteID)

# calculate the fit models per data frame
fits <- lapply( SC_annual_avg_split, function(x) {
  lm( formula = median ~ Year, data = x )
} )

# extract the slope from all models
slopes <- sapply( fits, function(x) x$coefficients )
slopes <- slopes[2,]

# print one of the results to see it
slopes[1]
print(slopes)

# create a df with the SiteID's and slopes of linear models
names <- names(SC_annual_avg_split)
lm_slope <- cbind(names, slopes)
lm_slope <- as.data.frame(lm_slope)
class(lm_slope$slopes)
lm_slope$slopes <- as.numeric(as.character(lm_slope$slopes))
lm_slope$slopes <- round(lm_slope$slopes, digits = 3)

# Plot slopes to see what kind of change is occurring over time, and in how many sites
sub_slope <- ggplot(lm_slope, aes(x = slopes))+
  xlim(-100,100)

sub_slope + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = median(slopes)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

# Look at all the slopes on one plot
ggplot(data = annual_avg)+
  geom_smooth(mapping = aes(x = Year, y = mean, color = SiteID), method = lm, se = F, size = 0.25)+
  theme(legend.position = "none")+
  ylim(0, 5000)#+
xlim(2015,2020)



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
