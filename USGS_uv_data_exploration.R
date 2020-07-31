rm(list = ls())
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
#---------------------
# Read in SC Data ####
#---------------------
SCuv <- readRDS("USGS_SC_uv_data_AFP.rds") # AFP = data "approved for publication"

colnames(SCuv)
SCuv <- select(SCuv, c("site_no", "dateTime", "X_00095_00000", "X_00095_00000_cd"))
colnames(SCuv) <- c("SiteID", "DateTime", "SpC", "dqi")
colnames(SCuv)[3] <- "SpC"
# SCuv$DateTime <- ifelse(!is.na(SCuv$Time), paste0(SCuv$Date, " ", SCuv$Time), paste(SCuv$Date))
sapply(SCuv, class)
SCuv$SiteID <- as.factor(SCuv$SiteID)
levels(SCuv$SiteID)
# 70 sites
# Take daily means, compare to USGS dv data
SCuv$Date <- as.Date(SCuv$DateTime)
SCuv_daily <- SCuv %>%
  group_by(SiteID ,Date) %>%
  summarise_at(.vars = "SpC", .funs = c("mean"=mean))
SCuv_daily$mean <- round(SCuv_daily$mean, digits = 0)
  
colnames(SCuv_daily)[3] <- "SpC"
# at this point, in order to compare, I have run up to line 40 or "USGS_SC_dv_data_exploration.R"
# as a result, I have SCdv in my global environment
common_sites <- intersect(SCuv_daily$SiteID, SCdv$SiteID) # 65 sites overlap 

SCuv_daily$Site_Date <- paste0(SCuv_daily$SiteID, "_", SCuv_daily$Date)
common_site_date <- intersect(SCuv_daily$Site_Date, SCdv$Site_Date) # 164992 site-dates overlap

# find the exact differences
# we will put these in separate dataframes and append them to the dataframe of overlapping data
diff1 <- setdiff(SCuv_daily$Site_Date, SCdv$Site_Date) #6803 in SCuv_daily that are not in SCdv
diff2 <- setdiff(SCdv$Site_Date, SCuv_daily$Site_Date) #284660 in SCdv that are not in SCuv_daily

SC_all_daily <- merge(SCuv_daily, SCdv) # dataframe of overlapping data
diff1_df <- SCuv_daily[which(SCuv_daily$Site_Date %in% diff1),] # dataframe for diff1
diff2_df <- SCdv[which(SCdv$Site_Date %in% diff2),] # dataframe for diff2

diff1_df <- as.data.frame(diff1_df)
SC_all_daily <- rbind(SC_all_daily, diff1_df, deparse.level = 1) 
SC_all_daily <- rbind(SC_all_daily, diff2_df, deparse.level = 1)

#check
factor(SC_all_daily$Site_Date)
# 311653 levels == 311653 site-dates so we are good
SC_check <- unique(SC_all_daily)
rm(SC_check) 

#output new daily data
getwd()
saveRDS(SC_all_daily, "USGS_SC_all_daily_data.rds")
# The rest of this script was made before we analyzed the differences and overlaps of the dv and summarized uv data
#---------------------
# Create year and month column ####
#---------------------
sapply(SCuv, class)
SCuv$DateTime <- ymd_hms(SCuv$DateTime)
SCuv$Year <- year(SCuv$DateTime)
# get rid of any data from 2020
SCuv <- SCuv[which(SCuv$Year < "2020"),]
SCuv$Month <- month(SCuv$DateTime)

# Split by SiteID and count # of measurements per year
# SCuv_split <- split(SCuv, SCuv$SiteID)
# SCuv_split_ct <- lapply(SCuv_split,plyr::count,  vars = "Year")

# Stats by Site-year
site_year_stats <- SCuv %>% 
  group_by(SiteID, Year) %>%
  summarise_at(.vars = "SpC", .funs = c("max" = max, "min" = min, "mean" = mean, "sd" = sd))
site_year_stats$range <- (site_year_stats$max-site_year_stats$min)
# 578 site years of uv SCuv data from USGS
# try box plot?

# find # of measurements per site-year
site_year_count <- SCuv %>% 
  group_by(SiteID, Year) %>%  
  dplyr::summarise(n()) 
#site_year_split <- split(site_year_split, site_year$SiteID)
# site_year$range <- ifelse(site_year$max == site_year$min, NA, paste(site_year$range))

# split SCuv data by SiteID
#SCuv_site_split <- split(SCuv, SCuv$SiteID)

# Stats by SiteID
site_stats <- SCuv %>% 
  group_by(SiteID) %>%
  summarise_at(.vars = "SpC", .funs = c("max" = max, "min" = min, "mean" = mean, "sd" = sd))
site_stats$range <- (site_stats$max - site_stats$min)
# 68 sites

# Day of year
#dat$Date <- paste0(month(dat$DateTime), "-", day(dat$DateTime))
# SCuv$doy <- strftime(SCuv$Date, format = "%j")
# SCuv$doy <-as.numeric(SCuv$doy)


#----------------
# Site-Year
#----------------
# Max SCuv
monthly_max <- SCuv %>%
  group_by(SiteID, Year) %>%
  slice(which.max(SpC))
monthly_max_count <- plyr::count(monthly_max, vars = "Month")
class(monthly_max_count$Month)
monthly_max_count$Month <- month.abb[monthly_max_count$Month]
monthly_max_count$Month = factor(monthly_max_count$Month, levels = month.abb)
# 103 site-years had their max SCuv in Dec (the majority)
# versus only 12 in June and 13 in May 
ggplot(monthly_max_count)+
  geom_col(mapping = aes(x = Month, y = freq))
# take averages of all years
# create function to get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

annual_avg_max <- monthly_max %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=mean, "sd"=sd))

annual_avg_max_count <- plyr::count(annual_avg_max, vars = "mode")
class(annual_avg_max_count$mode)
annual_avg_max_count$mode <- month.abb[annual_avg_max_count$mode]
annual_avg_max_count$mode = factor(annual_avg_max_count$mode, levels = month.abb)
ggplot(annual_avg_max_count)+
  geom_col(mapping = aes(x = mode, y = freq))
# 21 sites had their average annual max SCuv in December, 7 in Nov and July
# 1 in June, 2 in March and May
  
# Min SCuv
monthly_min <- SCuv %>%
  group_by(SiteID, Year) %>%
  slice(which.min(SpC))
monthly_min_count <- plyr::count(monthly_min, vars = "Month")
class(monthly_min_count$Month)
monthly_min_count$Month <- month.abb[monthly_min_count$Month]
monthly_min_count$Month = factor(monthly_min_count$Month, levels = month.abb)
# 187 site-years had their min SCuv in June (the majority)
# 117 in May (close to majority)
# versus only 4 in February and 8 in November
ggplot(monthly_min_count)+
  geom_col(mapping = aes(x = Month, y = freq))

annual_avg_min <- monthly_min %>%
  group_by(SiteID) %>%
  summarise_at(.vars = "Month", .funs = c("mean"=mean, "mode"=getmode, "median"=mean, "sd"=sd))

annual_avg_min_count <- plyr::count(annual_avg_min, vars = "mode")
class(annual_avg_min_count$mode)
annual_avg_min_count$mode <- month.abb[annual_avg_min_count$mode]
annual_avg_min_count$mode = factor(annual_avg_min_count$mode, levels = month.abb)
ggplot(annual_avg_min_count)+
  geom_col(mapping = aes(x = mode, y = freq))
#need to add missing months somehow
# 30 sites had their average annual minimum SCuv in June, 12 in May
# 1 in March, 2 in April and November



