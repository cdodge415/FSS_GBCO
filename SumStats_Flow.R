##############################################
## Code for Processing WQP Timeseries Data ##
##############################################
## Data should be in the format of the provided template
## Column names include:
## Required - SiteID, DateTime, UTC_TimeZone, DO_mgL, DO_psat, WaterTempC
## Optional - Discharge, Discharge Units, Does stage = channel depth?,
##            Depth, Depth Units, Stage above arbitrary datum, Stage Units, 
##            Baro Pressure, Baro Units, Salinity PSU

## List of parameters this code extracts from Required Data:
## (1) Time: Start Date, End Date, Timestep, Number of data points
## (2) For insertparameters: Mean, SD, Quantiles (0 (Min), 0.05, 0.1, 0.25, 0.5 (Median),
##                 0.75, 0.9, 0.95, 1 (Max)), 
## (3) Probability of insertparameters

## Salinity: Summary Stats


## Load packages
lapply(c("plyr","dplyr","ggplot2","cowplot","lubridate",
         "tidyverse","readxl","rMR","data.table"), require, character.only=T)
###########################
## Import Multiple Files ##
###########################

## (1) Set working directory to formatted TS files
getwd() # use bottom right panel to find folder & set directory
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted TS")
list.files(pattern = "csv")

#SumStat_Processing <- function(num){

formatted_file <- list.files(pattern=".csv")[1]#[num]

dat <- fread(formatted_file, header=T)
## Check
head(dat)
sapply(dat, class)

## Turn into list by splitting by site
dat.list <- split(dat, dat$SiteID)

###################################
## Format & QA/QC Required Data ###
###################################

## First adjust time and time zone
# Creates a function that separates date and time and adds midnight stamp 
# if there is no time already associated with the data
format_time <- function(xdat){
  xdat <- as.data.frame(xdat)
  
  xdat <- xdat %>% separate(DateTime, c("Date", "Time"),sep = " ")
  xdat$Time[is.na(xdat$Time) == TRUE] <- paste("12:00:00")
  xdat$DateTime <- lubridate::ymd_hms(paste(xdat$Date, xdat$Time))
  
  return(xdat)
  
}

## Apply this function to each element of dat.list
dat.list <- lapply(dat.list, function(x) format_time(x)) #, "%Y-%m-%d %H:%M:%S"))


#####################
## Summary Metrics ##
#####################
## Recombine dat.list
df <- ldply(dat.list, data.frame)
sapply(df, class)
#df$WaterTempC <- as.numeric(as.character(df$WaterTempC))
tail(df)

## Get rid of unreasonable values


## TIME
## Want: Start Date, End Date, Timestep, Number of data points for each parameter
names(df)
time_df <- na.omit(df[,c(".id","DateTime","Flow")]) %>%
  group_by(.id) %>%
  arrange(DateTime)%>%
  summarise(Start_time = head(DateTime, n=1),
            End_time = tail(DateTime, n=1),
            tdiff_sec = as.numeric(DateTime[2]) - as.numeric(DateTime[1]),
            n_time = length(DateTime))
# add column for sampling frequency
#time_df$frequency <- time_df$tdiff_sec/time_df$n_time
## Want: Mean, SD, Quantiles (0 (Min), 0.05, 0.1, 0.25, 0.5 (Median),0.75, 0.9, 0.95, 1 (Max))
sapply(df, class)
ss_df <- df %>%
  group_by(.id) %>%
  summarise_at(.vars= c("Flow"),
               .funs = c("mean"=mean, "sd"=sd, "min"=min, "max"=max), na.rm=T)
## dplyr is weird about quantiles so had to separate

quant_df <- df %>%
  group_by(.id) %>% 
  do(data.frame(t(quantile(.$Flow,
                           probs = c(0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95),
                           na.rm = T))))

## Frequency that DO < 1, 2, 3, 4, 5
# Hypox_F <- function(dat){
#   x <- na.omit(dat[,c("SiteID","DateTime","DO_mgL")])
#   f_sub1 <- nrow(x[which(x$DO_mgL < 1),])/nrow(x)
#   f_sub2 <- nrow(x[which(x$DO_mgL < 2),])/nrow(x)
#   f_sub3 <- nrow(x[which(x$DO_mgL < 3),])/nrow(x)
#   f_sub4 <- nrow(x[which(x$DO_mgL < 4),])/nrow(x)
#   f_sub5 <- nrow(x[which(x$DO_mgL < 5),])/nrow(x)
#   f_vec <- as.data.frame(t(as.matrix(c(f_sub1, f_sub2, f_sub3, f_sub4, f_sub5))))
#   colnames(f_vec) <- c("Hyp_pr_sub1","Hyp_pr_sub2","Hyp_pr_sub3","Hyp_pr_sub4","Hyp_pr_sub5")
#   return(f_vec)
# }
# 
# Hypox_F_df <- ldply(lapply(dat.list, function(x) Hypox_F(x)), data.frame)
# 
## Merge all together
stats_merged <- list(time_df, ss_df, quant_df
                     ) %>% 
  reduce(left_join, by = ".id")
colnames(stats_merged)[colnames(stats_merged)== ".id"] <- "SiteID"

## Export
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP SumStats")
write.csv(stats_merged, "SumStats_flow.csv")

