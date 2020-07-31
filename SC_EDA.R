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
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/Finalized TS (All Sources)")
SCd <- readRDS("all_SC_data.rds")
# class(SCd$SiteID)
# levels(SCd$SiteID)# 25,624 sites
unique(SCd$SiteID)
SCd <- select(SCd, -c("SiteDate"))

# add/format Date, Month, Year columns
SCd$Date <- ymd(SCd$Date)
SCd$Year <- year(SCd$Date)
SCd$Month <- month(SCd$Date)
SCd <- subset(SCd, SCd$Year >= "1900") # Data = 1900-2019 ####
SCd <- subset(SCd, SCd$Year < "2020")
# SCd$SiteID <- factor(SCd$SiteID)
# levels(SCd$SiteID) # 25,617 sites

# ouput final site list for spatial analysis
# sites <- SCd
# class(sites)
# sites <- as.data.frame(sites)
# sites$Data <- "Specific Conductance"
# sites <- select(sites, c("SiteID", "Data"))
# setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted Meta")
# write.csv(sites, "WQ_USGS_SC_site_list.csv")


# A look at # of measurements per site, per year, the period of record (POR), etc.
# understand how many raw measurements there are in each month to look for a bias in sampling timing that could affect mean, median, mode, etc.

# ALL DAILY DATA: 
# SCd_per_month <- plyr::count(SCd, vars = "Month")
# print(SCd_per_month)
# Month  freq
# 1      1 48620
# 2      2 46209
# 3      3 58047
# 4      4 63947
# 5      5 72984
# 6      6 71147
# 7      7 67122
# 8      8 70420
# 9      9 63246
# 10    10 62070
# 11    11 54466
# 12    12 47468

# There are definitely some large gaps here that may skew analysis of annual max and min's if using mode
# Graphic representation:
SCd$MonthAbb <- month.abb[SCd$Month]
SCd$MonthAbb = factor(SCd$MonthAbb, levels = month.abb)
barplot(table(SCd$MonthAbb))

# DATA AVAILABILITY/SUBSETTING ####
# see how many measurements there are in a year for each site-year
SCd_per_sy <- plyr::count(SCd, vars = c("SiteID","Year")) 
SCd_per_sy$SiteYear <- paste0(SCd_per_sy$SiteID, "_", SCd_per_sy$Year)
SCd$SiteYear <- paste0(SCd$SiteID, "_", SCd$Year) # add site-year to the full dataset
# 83,650 site-years

# SCd_sub = a subset of data:
# Filter data for >= 12 data points per year
# We are ASSUMING that this implies a monthly sampling 
# use this subset for things like identifying timing of max and mins, range, etc.
# use the full dataset to look at trends over time
SCd_sub <- SCd_per_sy[which(SCd_per_sy$freq >= 12),] 
SCd_sub <- SCd[which(SCd$SiteYear %in% SCd_sub$SiteYear),] 
SCd_sub$SiteYear <- factor(SCd_sub$SiteYear) # get rid of unused levels by re-factoring
SCd_sub$SiteID <- factor(SCd_sub$SiteID) # get rid of unused levels by re-factoring
# levels (SCd_sub$SiteYear) # 8395 site-years
# levels(SCd_sub$SiteID) # 607 sites
# 452,156 observations

# Filter data for >= 52 data points per year
# We are ASSUMING that this implies a weekly sampling
# SCd_sub_w <- SCd_per_sy[which(SCd_per_sy$freq >= 52),] 
# SCd_sub_w <- SCd[which(SCd$SiteYear %in% SCd_sub_w$SiteYear),] 
# SCd_sub_w$SiteYear <- factor(SCd_sub_w$SiteYear) # get rid of unused levels by re-factoring
# SCd_sub_w$SiteID <- factor(SCd_sub_w$SiteID) # get rid of unused levels by re-factoring

# Heat map to look just at years of data available in the raw data and the subset
heatmap <- ggplot(SCd, aes(x = Year, y = SiteID)) + 
  geom_tile(color = "red")+
  theme(axis.text.y=element_blank())+
  xlim(1900,2020)
sub_heatmap <- ggplot(SCd_sub, aes(x = Year, y = SiteID)) + 
  geom_tile(color = "red")+
  theme(axis.text.y=element_blank())+
  xlim(1900,2020)
#subw_heatmap <- ggplot(SCd_sub_w, aes(x = Year, y = SiteID)) + 
  # geom_tile(color = "red")+
  # theme(axis.text.y=element_blank())+
  # xlim(1900,2020)
print(heatmap)
print(sub_heatmap)
#print(subw_heatmap)
rm(heatmap, sub_heatmap)

# SCd_sub_per_month <- plyr::count(SCd_sub, vars = "Month")
# print(SCd_sub_per_month)
# Month  freq
# 1      1 32782
# 2      2 31078
# 3      3 36777
# 4      4 39884
# 5      5 43900
# 6      6 41475
# 7      7 40624
# 8      8 40839
# 9      9 38854
# 10    10 37816
# 11    11 34614
# 12    12 33513
# You can tell we have evened out the the number of measurements in each month a bit
SCd_sub$MonthAbb <- month.abb[SCd_sub$Month]
SCd_sub$MonthAbb = factor(SCd_sub$MonthAbb, levels = month.abb)
barplot(table(SCd_sub$MonthAbb))
rm(heatmap, sub_heatmap, SCd_per_month) # save space/make things move faster

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

SS <- SCd %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "SpC", .funs = c("min" = min, "max"=max, "mean"=mean, "mode"=getmode, "median"=median, "sd"=sd, "tm"=tm, "IQR"=iqr))
SS$range <- SS$max-SS$min
SS$cv <- SS$sd/SS$mean

# TIMING/ANNUAL TRENDS ####
# MODE ####
# MAX SCd_sub timing
# find out what month of the year the maximum SCd_sub occurs for each site-year
monthly_max <- SCd_sub %>% 
  group_by(SiteID, Year) %>%
  slice(which.max(SpC))
monthly_max_count <- plyr::count(monthly_max, vars = "Month") # count how many site-years have their max SCd_sub in each month
monthly_max_count$Month <- month.abb[monthly_max_count$Month]
monthly_max_count$Month = factor(monthly_max_count$Month, levels = month.abb)
ggplot(monthly_max_count)+
  geom_col(mapping = aes(x = Month, y = freq))

annual_avg_max <- monthly_max %>% # take averages of month of peak SCd_sub for all years for each site
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_max_count <- plyr::count(annual_avg_max, vars = "mode") # choose which function you want to use and apply it to the following lines as well (i.e. mode, mean)
# count how many sites had their peak SCd_sub in each month on "average" (mean) or "most years" (mode) (specify which function was used)
annual_avg_max_count$mode <- month.abb[annual_avg_max_count$mode]
annual_avg_max_count$mode = factor(annual_avg_max_count$mode, levels = month.abb)
ggplot(annual_avg_max_count)+
  geom_col(mapping = aes(x = mode, y = freq))
# MIN SCd_sub
# follow procedure for minimum SCd_sub
monthly_min <- SCd_sub %>%
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
# Max SCd_sub
# take averages of all years
annual_avg_max <- monthly_max %>% # take averages of month of peak SCd_sub for all years for each site
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_max$mean <- round(annual_avg_max$mean)
annual_avg_max_count <- plyr::count(annual_avg_max, vars = "mean") # choose which function you want to use and apply it to the following lines as well
annual_avg_max_count$mean <- month.abb[annual_avg_max_count$mean]
annual_avg_max_count$mean = factor(annual_avg_max_count$mean, levels = month.abb)
ggplot(annual_avg_max_count)+
  geom_col(mapping = aes(x = mean, y = freq))

# Min SCd_sub
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
SCd_sub_rollmean <- SCd_per_sy[which(SCd_per_sy$freq >= 365),] 
SCd_sub_rollmean <- SCd[which(SCd$SiteYear %in% SCd_sub_rollmean$SiteYear),] 
SCd_sub_rollmean$SiteYear <- factor(SCd_sub_rollmean$SiteYear) # get rid of unused levels by re-factoring
SCd_sub_rollmean$SiteID <- factor(SCd_sub_rollmean$SiteID) # get rid of unused levels by re-factoring
SCd_sub_rollmean$Rollmean <- rollmean(SCd_sub_rollmean$SpC, 7, fill = NA, align = c("center")) # 7 == 7-day rolling mean (to potentially smooth out any odd 1-day outliers)
# Max SCd_sub
monthly_max <- SCd_sub_rollmean %>% 
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

# Min SCd_sub
monthly_min <- SCd_sub_rollmean %>%
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
# Max SCd_sub
annual_avg_max <- monthly_max %>% # take averages of month of peak SCd_sub for all years for each site
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=median, "sd"=sd))
annual_avg_max$mean <- round(annual_avg_max$mean)
annual_avg_max_count <- plyr::count(annual_avg_max, vars = "mean") 
annual_avg_max_count$mean <- month.abb[annual_avg_max_count$mean]
annual_avg_max_count$mean = factor(annual_avg_max_count$mean, levels = month.abb)
ggplot(annual_avg_max_count)+
  geom_col(mapping = aes(x = mean, y = freq))

# Min SCd_sub
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
# SCd_sub ####
# by month # NOTE: I'm not sure it makes much sense to look at variability this way...
# SCd_sub_monthly <- SCd_sub %>%
#   group_by(Month) %>% 
#   summarise_at(.vars = "SpC", .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean))
# SCd_sub_monthly$range <- SCd_sub_monthly$max-SCd_sub_monthly$min
# SCd_sub_monthly$cv <- SCd_sub_monthly$sd/SCd_sub_monthly$mean
# SCd_sub_monthly$Month <- month.abb[SCd_sub_monthly$Month]
# SCd_sub_monthly$Month = factor(SCd_sub_monthly$Month, levels = month.abb)
# ggplot(SCd_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = cv))
# ggplot(SCd_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = range))

# by site-month
# subset further to only include months with >= 4 obs. per month 
# We are ASSUMING this implies a weekly sampling
SCd_per_sym <- plyr::count(SCd, vars = c("SiteID", "Year", "Month")) 
SCd_per_sym$SiteYearMonth <- paste0(SCd_per_sym$SiteID, "_", SCd_per_sym$Year, "_", SCd_per_sym$Month)
# add site-year-month to the full dataset
SCd$SiteYearMonth <- paste(SCd$SiteID, SCd$Year, SCd$Month, sep = "_")

# SCd_sub2 = a subset of data:
# try filtering data for >= 4 data points per site-year-month (so in a specific month of any given year, must have >= 4 obs.)
# We are ASSUMING that this implies a monthly sampling ####
SCd_sub2 <- SCd_per_sym[which(SCd_per_sym$freq >= 4),] 
SCd_sub2 <- SCd[which(SCd$SiteYearMonth %in% SCd_sub2$SiteYearMonth),] 
SCd_sub2$SiteYearMonth <- as.factor(SCd_sub2$SiteYearMonth)
SCd_sub2$SiteYearMonth <- factor(SCd_sub2$SiteYearMonth) # get rid of unused levels by re-factoring
SCd_sub2$SiteID <- factor(SCd_sub2$SiteID) # get rid of unused levels by re-factoring
# levels(SCd_sub2$SiteYearMonth) # 21,987 site-year-months
# levels(SCd_sub2$SiteID) # 928 sites
# 356,316 observations
SCd_sub2_monthly <- SCd_sub2 %>%
  group_by(SiteID, Year, Month) %>% 
  summarise_at(.vars = "SpC", .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean, "IQR"=iqr, "tm"=tm))
SCd_sub2_monthly$range <- SCd_sub2_monthly$max-SCd_sub2_monthly$min
SCd_sub2_monthly$cv <- SCd_sub2_monthly$sd/SCd_sub2_monthly$mean
SCd_sub2_monthly$Month <- month.abb[SCd_sub2_monthly$Month]
SCd_sub2_monthly$Month = factor(SCd_sub2_monthly$Month, levels = month.abb)
ggplot(SCd_sub2_monthly)+
  geom_col(mapping = aes(x = Month, y = cv))
ggplot(SCd_sub2_monthly)+
  geom_col(mapping = aes(x = Month, y = range))
# this is better because each month in time at each site is given = weight
# summarize cv by month
SCd_sub2_monthly <- SCd_sub2_monthly[complete.cases(SCd_sub2_monthly), ]
SCd_sub2_cv <- SCd_sub2_monthly %>%
  group_by(Month) %>%
  summarise_at(.vars = "cv", .funs = c("min" = min, "max" = max, "mean" = mean))
SCd_sub2_cv$mean <- round(SCd_sub2_cv$mean, digits = 2)
ggplot(SCd_sub2_cv)+
  geom_col(mapping = aes(x = Month, y = mean))

SCd_sub2_range <- SCd_sub2_monthly %>%
  group_by(Month) %>%
  summarise_at(.vars = "range", .funs = c("min" = min, "max" = max, "mean" = mean, "median" = median))

SCd_sub2_iqr <- SCd_sub2_monthly %>%
  group_by(Month) %>%
  summarise_at(.vars = "IQR", .funs = c("min" = min, "max" = max, "mean" = mean, "median" = median))

# by site
SCd_sub_monthly <- SCd_sub %>%
  group_by(SiteID) %>% 
  summarise_at(.vars = c("SpC"), .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean))
SCd_sub_monthly$range <- SCd_sub_monthly$max-SCd_sub_monthly$min
SCd_sub_monthly$cv <- SCd_sub_monthly$sd/SCd_sub_monthly$mean
#SCd_sub_monthly$Month <- month.abb[SCd_sub_monthly$Month]
#SCd_sub_monthly$Month = factor(SCd_sub_monthly$Month, levels = month.abb)
cv <-
  ggplot(SCd_sub_monthly, aes(x = cv))
cv + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(cv)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

# SCd ####
# all sites by month
# SCd_sub_monthly <- SCd_sub %>%
#   group_by(Month) %>% 
#   summarise_at(.vars = "SpC", .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean))
# SCd_sub_monthly$range <- SCd_sub_monthly$max-SCd_sub_monthly$min
# SCd_sub_monthly$cv <- SCd_sub_monthly$sd/SCd_sub_monthly$mean
# SCd_sub_monthly$Month <- month.abb[SCd_sub_monthly$Month]
# SCd_sub_monthly$Month = factor(SCd_sub_monthly$Month, levels = month.abb)
# ggplot(SCd_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = cv))
# ggplot(SCd_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = range))

# by site-month
# SCd_sub_monthly <- SCd_sub %>%
#   group_by(SiteID, Month) %>% 
#   summarise_at(.vars = "SpC", .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean))
# SCd_sub_monthly$range <- SCd_sub_monthly$max-SCd_sub_monthly$min
# SCd_sub_monthly$cv <- SCd_sub_monthly$sd/SCd_sub_monthly$mean
# SCd_sub_monthly$Month <- month.abb[SCd_sub_monthly$Month]
# SCd_sub_monthly$Month = factor(SCd_sub_monthly$Month, levels = month.abb)
# ggplot(SCd_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = cv))
# ggplot(SCd_sub_monthly)+
#   geom_col(mapping = aes(x = Month, y = range))
# 
# # by site
# SCd_sub_monthly <- SCd_sub %>%
#   group_by(SiteID) %>% 
#   summarise_at(.vars = c("SpC"), .funs = c("min" = min, "max" = max, "sd" = sd, "mean" = mean))
# SCd_sub_monthly$range <- SCd_sub_monthly$max-SCd_sub_monthly$min
# SCd_sub_monthly$cv <- SCd_sub_monthly$sd/SCd_sub_monthly$mean
## SCd_sub_monthly$Month <- month.abb[SCd_sub_monthly$Month]
## SCd_sub_monthly$Month = factor(SCd_sub_monthly$Month, levels = month.abb)



# LINEAR MODEL ####
# https://www.theanalysisfactor.com/linear-models-r-plotting-regression-lines/

# SCd ####
SCd$SiteID <- factor(SCd$SiteID) # double check there are no implicit NAs in SiteID to avoid errors with splitting
SCd_split <- split(SCd, SCd$SiteID)
SCd_split <- SCd_split[sapply(SCd_split, function(x) dim(x)[1]) > 0]
# calculate the fit models per data frame
fits <- lapply( SCd_split, function(x) {
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
names <- names(SCd_split)
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


# SCd sub ####
SCd_sub_split <- split(SCd_sub, SCd_sub$SiteID)
SCd_sub_split <- SCd_sub_split[sapply(SCd_sub_split, function(x) dim(x)[1]) > 0] # get rid of any empty dfs

# calculate the fit models per data frame
fits <- lapply( SCd_sub_split, function(x) {
  lm( formula = SpC ~ Date, data = x )
} )

# extract the slope from all models
slopes <- sapply( fits, function(x) x$coefficients )
slopes <- slopes[2,]

# print one of the results to see it
slopes[1]
print(slopes)

# create a df with the SiteID's and slopes of the linear models
names <- names(SCd_sub_split)
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
# SCd_sub
annual_avg <- SCd%>% # quartiles might also be good to look at
  group_by(SiteID, Year) %>% 
  summarise_at(.vars = "SpC", .funs = c("mean"=mean, "median"=mean))
# linear model of annual averages
SCd_annual_avg_split <- split(annual_avg, annual_avg$SiteID)

# calculate the fit models per data frame
fits <- lapply( SCd_annual_avg_split, function(x) {
  lm( formula = median ~ Year, data = x )
} )

# extract the slope from all models
slopes <- sapply( fits, function(x) x$coefficients )
slopes <- slopes[2,]

# print one of the results to see it
slopes[1]
print(slopes)

# create a df with the SiteID's and slopes of linear models
names <- names(SCd_annual_avg_split)
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
site_data <- site_data[which(site_data$Site_ID %in% SCd$SiteID),]
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
# (time_plot <- ggplot(SCd_sub, aes(x = Date, y = SpC, color = SiteID)) +
#    geom_line() +
#    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
#    theme_classic()+
#    theme(legend.position = "none"))+
#   facet_wrap(~ SiteID)

# Great example TS:
ggplot(subset(SCd, SiteID == "10343500"), aes(x = Date, y = SpC))+
  geom_line()+
  geom_smooth(method = "lm")+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_classic()+
  theme(legend.position = "none")
# Interesting way to plot max and mins
# install.packages("ggpmisc")  
# library(ggpmisc)
# 
# ggplot(data = subset(SCd_sub, SCd_sub$SiteID == "09230300" & SCd_sub$Year == "2008"))+
#   geom_line(mapping = aes(x = Date, y = SpC))+
#   stat_peaks(mapping = aes(x = Date, y = SpC),color = "green")+
#   stat_valleys(mapping = aes(x = Date, y = SpC),color = "red")


# SAVE DATA FILES ####
saveRDS(SCd, "SCd.RDS")
saveRDS(SCd_sub, "SCd_sub.RDS")
saveRDS(SCd_sub2, "SCd_sub_sym.RDS")
