##### Format Raw Time Series (TS) Data from the Water Quality Data Portal (WQP) ####
## Call packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse", "readxl", "data.table"), require, character.only=T)
# Set wd to Raw TS folder and look at flow data
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Raw TS/Flow")
list.files(pattern=".csv")

## Choose files and read in
# Here we read in both flow files and merge them together
state_file <- list.files(pattern=".csv")[1]
HUC14 <- fread(state_file, header=T)
HUC14$HUC <- "14" 
state_file <- list.files(pattern=".csv")[2]
HUC15 <- fread(state_file, header=T)
HUC15$HUC <- "15"
state_file <- list.files(pattern=".csv")[3]
HUC16 <- fread(state_file, header=T)
HUC16$HUC <- "16"
flow <- rbind(HUC14, HUC15, deparse.level = 1)
flow <- rbind(flow, HUC16, deparse.level = 1)

## Subset columns
names(flow)
sub <- flow[,c("ActivityStartDate","ActivityStartTime/Time", "ActivityStartTime/TimeZoneCode",
            "MonitoringLocationIdentifier",
            "ResultMeasureValue", "ResultMeasure/MeasureUnitCode", "HUC")]
rm(HUC14, HUC15, HUC16, flow) # To save space and speed things up

colnames(sub) <- c("Date","Time","TimeZone",
                   "SiteID","Value","Units", "HUC")

## Adjust to create DateTime column 
sub$Date <- as.POSIXct(as.character(sub$Date), format="%Y-%m-%d")
sub$Time2 <- as.POSIXct(paste(as.character(sub$Date), as.character(sub$Time)), format="%Y-%m-%d %H:%M:%S")
sub$DateTime <- ifelse(is.na(sub$Time2)==FALSE, yes=paste(sub$Time2), no=paste(sub$Date))

# add noon time stamp to all DateTimes without a time
format_time <- function(xdat){
  xdat <- as.data.frame(xdat)
  
  xdat <- xdat %>% separate(DateTime, c("Date", "Time"),sep = " ")
  xdat$Time[is.na(xdat$Time) == TRUE] <- paste("12:00:00")
  xdat$DateTime <- lubridate::ymd_hms(paste(xdat$Date, xdat$Time))
  return(xdat)
}

sub <- format_time(sub)
head(sub,20)
sapply(sub, class)
sub$Value <- as.numeric(sub$Value)

## Recast so parameters all have separate columns
## Fix units
levels(as.factor(sub$Units))
sub$Units[sub$Units == ""] <- NA
sub[which(sub$Units == "cfm"),]$Value <- sub[which(sub$Units == "cfm"),]$Value/(35.3147*60)
sub$Units[which(sub$Units == "cfm")] <- "m3/sec"
sub[which(sub$Units == "cfs"),]$Value <- sub[which(sub$Units == "cfs"),]$Value/35.3147
sub$Units[which(sub$Units == "cfs")] <- "m3/sec"
sub[which(sub$Units == "cm3/sec"),]$Value <- sub[which(sub$Units == "cm3/sec"),]$Value/1000000
sub$Units[which(sub$Units == "cm3/sec")] <- "m3/sec"
sub <- sub[-which(sub$Units == "ft/sec"),]
sub[which(sub$Units == "ft3/sec"),]$Value <- sub[which(sub$Units == "ft3/sec"),]$Value/35.3147
sub$Units[which(sub$Units == "ft3/sec")] <- "m3/sec"
sub[which(sub$Units == "ft3/s"),]$Value <- sub[which(sub$Units == "ft3/s"),]$Value/35.3147
sub$Units[which(sub$Units == "ft3/s")] <- "m3/sec"
sub[which(sub$Units == "g/min"),]$Value <- sub[which(sub$Units == "g/min"),]$Value/(60*264.172)
sub$Units[which(sub$Units == "g/min")] <- "m3/sec"
sub[which(sub$Units == "gal/day"),]$Value <- sub[which(sub$Units == "gal/day"),]$Value/(86400*264.172)
sub$Units[which(sub$Units == "gal/day")] <- "m3/sec"
sub[which(sub$Units == "gal/hr"),]$Value <- sub[which(sub$Units == "gal/hr"),]$Value/(3600*264.172)
sub$Units[which(sub$Units == "gal/hr")] <- "m3/sec"
sub[which(sub$Units == "gal/min"),]$Value <- sub[which(sub$Units == "gal/min"),]$Value/(60*264.172)
sub$Units[which(sub$Units == "gal/min")] <- "m3/sec"
sub[which(sub$Units == "gal/sec"),]$Value <- sub[which(sub$Units == "gal/sec"),]$Value/(264.172)
sub$Units[which(sub$Units == "gal/sec")] <- "m3/sec"
sub[which(sub$Units == "gpm"),]$Value <- sub[which(sub$Units == "gpm"),]$Value/(60*264.172)
sub$Units[which(sub$Units == "gpm")] <- "m3/sec"
sub[which(sub$Units == "l/min"),]$Value <- sub[which(sub$Units == "l/min"),]$Value/(60*1000)
sub$Units[which(sub$Units == "l/min")] <- "m3/sec"
sub[which(sub$Units == "l/sec"),]$Value <- sub[which(sub$Units == "l/sec"),]$Value/(1000)
sub$Units[which(sub$Units == "l/sec")] <- "m3/sec"
sub$Units[sub$Units == "mg/day"] <- NA
sub$Units[sub$Units == "mg/l"] <- NA
sub[which(sub$Units == "mgd"),]$Value <- sub[which(sub$Units == "mgd"),]$Value*0.043813
sub$Units[which(sub$Units == "mgd")] <- "m3/sec"
sub$Units[sub$Units == "None"] <- NA
sub$Units <- factor(sub$Units)
levels(sub$Units)
# Now all flow is in units of m3/sec
names(sub)
# subset columns
sub <- sub[,c("SiteID","DateTime","Value", "TimeZone", "HUC")]
colnames(sub) <- c("SiteID", "DateTime", "Flow", "TimeZone", "HUC") 

# average across duplicate DateTime/Site IDs
sub$DateTimeID <- paste(sub$DateTime,sub$SiteID)
sapply(sub, class)
dat_sum <- sub %>% group_by(as.factor(sub$DateTimeID))%>%
  summarise_at(.vars = c("Flow"), .funs=mean, na.rm=T)
colnames(dat_sum) <- c("DateTimeID","Flow")

## Rejoin with other info
info <- sub
info$DateTimeID <- as.factor(info$DateTimeID)

dat_rejoin <- left_join(dat_sum, info[,c("DateTime","TimeZone","SiteID","DateTimeID", "HUC")], by="DateTimeID", all.x=T)
dat_rejoin <- unique(dat_rejoin)

## Adjust TimeZone to relative to UTC
dat <- dat_rejoin
dat$UTC_TimeZone <- dat$TimeZone
dat$UTC_TimeZone <- as.character(dat$UTC_TimeZone)

dat$UTC_TimeZone[dat$UTC_TimeZone == "HST"] <- -10
dat$UTC_TimeZone[dat$UTC_TimeZone == "HAST"] <- -10
dat$UTC_TimeZone[dat$UTC_TimeZone == "HDT"] <- -9
dat$UTC_TimeZone[dat$UTC_TimeZone == "PST"] <- -8
dat$UTC_TimeZone[dat$UTC_TimeZone == "PDT"] <- -7
dat$UTC_TimeZone[dat$UTC_TimeZone == "MST"] <- -7
dat$UTC_TimeZone[dat$UTC_TimeZone == "MDT"] <- -6
dat$UTC_TimeZone[dat$UTC_TimeZone == "CST"] <- -6
dat$UTC_TimeZone[dat$UTC_TimeZone == "CDT"] <- -5
dat$UTC_TimeZone[dat$UTC_TimeZone == "EST"] <- -5
dat$UTC_TimeZone[dat$UTC_TimeZone == "EDT"] <- -4
dat$UTC_TimeZone[dat$UTC_TimeZone == "AST"] <- -4

## Final adjustments to match template
#df <- dat[,c("SiteID","DateTime","UTC_TimeZone","DO_mgL","DO_psat","WaterTempC")]
df <- dat[,c("SiteID","DateTime","UTC_TimeZone","Flow", "HUC")]
sapply(df, class)
# replace NaNs with NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

df[is.nan(df)] <- NA

## Write CSV
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted TS")
write.csv(df, "flow_TS.csv")
