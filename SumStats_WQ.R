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

formatted_file <- list.files(pattern=".csv")[2]#[num]

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
rm(dat.list)
sapply(df, class)
#df$WaterTempC <- as.numeric(as.character(df$WaterTempC))
tail(df)

# use df to put measurements into bins by month to figure out how many measurements were taken in each
sub <- df[,c(".id", "Date", "Time", "HUC", "Alkalinity..total", "Calcium", "Magnesium", "pH", "Potassium", "Salinity", "Sodium", "Specific.conductance", "DateTime")]
sub$Month <- substr(sub$Date, 6,7)
sub$Month[which(sub$Month == "01")] <- "Jan"
sub$Month[which(sub$Month == "02")] <- "Feb"
sub$Month[which(sub$Month == "03")] <- "Mar"
sub$Month[which(sub$Month == "04")] <- "Apr"
sub$Month[which(sub$Month == "05")] <- "May"
sub$Month[which(sub$Month == "06")] <- "Jun"
sub$Month[which(sub$Month == "07")] <- "Jul"
sub$Month[which(sub$Month == "08")] <- "Aug"
sub$Month[which(sub$Month == "09")] <- "Sep"
sub$Month[which(sub$Month == "10")] <- "Oct"
sub$Month[which(sub$Month == "11")] <- "Nov"
sub$Month[which(sub$Month == "12")] <- "Dec"

setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/Figures")
write.csv(sub, "Monthly_WQ.csv")

## TIME
## Want: Start Date, End Date, Timestep, Number of data points for each parameter
names(df)
time_df_alk <- na.omit(df[,c(".id","DateTime","Alkalinity..total")]) %>%
  group_by(.id) %>%
  arrange(DateTime)%>%
  summarise(Start_time = head(DateTime, n=1),
            End_time = tail(DateTime, n=1),
            tdiff_sec = as.numeric(DateTime[2]) - as.numeric(DateTime[1]),
            n_time = length(DateTime))
colnames(time_df_alk)[2:5] <- paste("alk", colnames(time_df_alk)[2:5], sep = "_")

time_df_ca <- na.omit(df[,c(".id","DateTime","Calcium")]) %>%
  group_by(.id) %>%
  arrange(DateTime)%>%
  summarise(Start_time = head(DateTime, n=1),
            End_time = tail(DateTime, n=1),
            tdiff_sec = as.numeric(DateTime[2]) - as.numeric(DateTime[1]),
            n_time = length(DateTime))
colnames(time_df_ca)[2:5] <- paste("ca", colnames(time_df_ca)[2:5], sep = "_")

time_df_mg <- na.omit(df[,c(".id","DateTime","Magnesium")]) %>%
  group_by(.id) %>%
  arrange(DateTime)%>%
  summarise(Start_time = head(DateTime, n=1),
            End_time = tail(DateTime, n=1),
            tdiff_sec = as.numeric(DateTime[2]) - as.numeric(DateTime[1]),
            n_time = length(DateTime))
colnames(time_df_mg)[2:5] <- paste("mg", colnames(time_df_mg)[2:5], sep = "_")

time_df_pH <- na.omit(df[,c(".id","DateTime","pH")]) %>%
  group_by(.id) %>%
  arrange(DateTime)%>%
  summarise(Start_time = head(DateTime, n=1),
            End_time = tail(DateTime, n=1),
            tdiff_sec = as.numeric(DateTime[2]) - as.numeric(DateTime[1]),
            n_time = length(DateTime))
colnames(time_df_pH)[2:5] <- paste("pH", colnames(time_df_pH)[2:5], sep = "_")

time_df_K <- na.omit(df[,c(".id","DateTime","Potassium")]) %>%
  group_by(.id) %>%
  arrange(DateTime)%>%
  summarise(Start_time = head(DateTime, n=1),
            End_time = tail(DateTime, n=1),
            tdiff_sec = as.numeric(DateTime[2]) - as.numeric(DateTime[1]),
            n_time = length(DateTime))
colnames(time_df_K)[2:5] <- paste("K", colnames(time_df_K)[2:5], sep = "_")

time_df_sal <- na.omit(df[,c(".id","DateTime","Salinity")]) %>%
  group_by(.id) %>%
  arrange(DateTime)%>%
  summarise(Start_time = head(DateTime, n=1),
            End_time = tail(DateTime, n=1),
            tdiff_sec = as.numeric(DateTime[2]) - as.numeric(DateTime[1]),
            n_time = length(DateTime))
colnames(time_df_sal)[2:5] <- paste("sal", colnames(time_df_sal)[2:5], sep = "_")

time_df_na <- na.omit(df[,c(".id","DateTime","Sodium")]) %>%
  group_by(.id) %>%
  arrange(DateTime)%>%
  summarise(Start_time = head(DateTime, n=1),
            End_time = tail(DateTime, n=1),
            tdiff_sec = as.numeric(DateTime[2]) - as.numeric(DateTime[1]),
            n_time = length(DateTime))
colnames(time_df_na)[2:5] <- paste("na", colnames(time_df_na)[2:5], sep = "_")

time_df_cond <- na.omit(df[,c(".id","DateTime","Specific.conductance")]) %>%
  group_by(.id) %>%
  arrange(DateTime)%>%
  summarise(Start_time = head(DateTime, n=1),
            End_time = tail(DateTime, n=1),
            tdiff_sec = as.numeric(DateTime[2]) - as.numeric(DateTime[1]),
            n_time = length(DateTime))
colnames(time_df_cond)[2:5] <- paste("cond", colnames(time_df_cond)[2:5], sep = "_")

## Want: Mean, SD, Quantiles (0 (Min), 0.05, 0.1, 0.25, 0.5 (Median),0.75, 0.9, 0.95, 1 (Max))
sapply(df, class)


ss_df <- df %>%
  group_by(.id) %>%
  summarise_at(.vars= c("Alkalinity..total","Calcium" , "Magnesium" ,"pH" ,"Potassium" ,"Salinity", "Sodium" ,"Specific.conductance"),
               .funs = c("mean"=mean, "sd"=sd, "min"=min, "max"=max), na.rm=T)
# We still get Inf and -Inf for Min and Max functions
ss_df[sapply(ss_df, is.infinite)] <- NA
# replace NaNs with NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

ss_df[is.nan(ss_df)] <- NA


## dplyr is weird about quantiles so had to separate

quant_alk_df <- df %>%
  group_by(.id) %>% 
  do(data.frame(t(quantile(.$Alkalinity..total,
                           probs = c(0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95),
                           na.rm = T))))
colnames(quant_alk_df)[2:8] <- paste("alk", colnames(quant_alk_df)[2:8], sep = "_")
class(quant_alk_df$.id)

quant_ca_df <- df %>%
  group_by(.id) %>% 
  do(data.frame(t(quantile(.$Calcium,
                           probs = c(0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95),
                           na.rm = T))))
colnames(quant_ca_df)[2:8] <- paste("ca", colnames(quant_ca_df)[2:8], sep = "_")

quant_cond_df <- df %>%
  group_by(.id) %>% 
  do(data.frame(t(quantile(.$Specific.conductance,
                           probs = c(0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95),
                           na.rm = T))))
colnames(quant_cond_df)[2:8] <- paste("cond", colnames(quant_cond_df)[2:8], sep = "_")

quant_K_df <- df %>%
  group_by(.id) %>% 
  do(data.frame(t(quantile(.$Potassium,
                           probs = c(0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95),
                           na.rm = T))))
colnames(quant_K_df)[2:8] <- paste("K", colnames(quant_K_df)[2:8], sep = "_")

quant_mg_df <- df %>%
  group_by(.id) %>% 
  do(data.frame(t(quantile(.$Magnesium,
                           probs = c(0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95),
                           na.rm = T))))
colnames(quant_mg_df)[2:8] <- paste("mg", colnames(quant_mg_df)[2:8], sep = "_")

quant_na_df <- df %>%
  group_by(.id) %>% 
  do(data.frame(t(quantile(.$Sodium,
                           probs = c(0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95),
                           na.rm = T))))
colnames(quant_na_df)[2:8] <- paste("na", colnames(quant_na_df)[2:8], sep = "_")

quant_pH_df <- df %>%
  group_by(.id) %>% 
  do(data.frame(t(quantile(.$pH,
                           probs = c(0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95),
                           na.rm = T))))
colnames(quant_pH_df)[2:8] <- paste("pH", colnames(quant_pH_df)[2:8], sep = "_")

quant_sal_df <- df %>%
  group_by(.id) %>% 
  do(data.frame(t(quantile(.$Salinity,
                           probs = c(0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95),
                           na.rm = T))))
colnames(quant_sal_df)[2:8] <- paste("sal", colnames(quant_sal_df)[2:8], sep = "_")

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

## Merge all together
ls()
# we are here
stats_merged <- list(ss_df,time_df_alk,time_df_ca,time_df_cond,time_df_K,
                     time_df_mg,time_df_na,time_df_pH,time_df_sal, quant_alk_df,
                     quant_ca_df,quant_cond_df,quant_K_df,quant_mg_df,      
                     quant_na_df,quant_pH_df,quant_sal_df
) %>% 
  reduce(left_join, by = ".id")
colnames(stats_merged)[colnames(stats_merged)== ".id"] <- "SiteID"

## Export
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP SumStats")

write.csv(stats_merged, "SumStats_WQ.csv")

