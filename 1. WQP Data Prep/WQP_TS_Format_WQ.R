##### Format Raw Time Series (TS) Data from the Water Quality Data Portal (WQP) ####

## Call packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse", "readxl", "data.table"), require, character.only=T)
## Set wd to Raw TS Folder
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Raw TS")
# Look at ion data
setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Raw TS/WQ")
list.files(pattern=".csv")

## Choose one file and read it in
state_file <- list.files(pattern=".csv")[1]
HUC14 <- fread(state_file, header=T)
HUC14$HUC <- "14"
state_file <- list.files(pattern=".csv")[2]
HUC15 <- fread(state_file, header=T)
HUC15$HUC <- "15"
state_file <- list.files(pattern= ".csv")[3]
HUC16 <- fread(state_file, header = T)
HUC16$HUC <- "16"
WQ <- rbind(HUC14, HUC15, deparse.level = 1)
WQ <- rbind(WQ, HUC16, deparse.level = 1)
rm(HUC14, HUC15, HUC16) # To save space and speed things up

## Subset columns
names(WQ)
sub <- WQ[,c("ActivityStartDate","CharacteristicName","ActivityStartTime/Time", "ActivityStartTime/TimeZoneCode",
            "MonitoringLocationIdentifier",
            "ResultMeasureValue", "ResultMeasure/MeasureUnitCode", "HUC")]

# rm(WQ) 
names(sub)
colnames(sub) <- c("Date","Parameter","Time","TimeZone",
                   "SiteID","Value","Units", "HUC")
head(sub)
## Add noon time stamp to all DateTimes without a time (function for doing this below results in errors since rows with no time stamp are "", not necessarily NA)
time_check <- subset(sub, sub$SiteID == "USGS-333656112113701" & sub$Date == "1966-04-27")
class(time_check$Time)
time_check$Time <- as.factor(time_check$Time)
levels(time_check$Time)
# "" not NA

sub$Time <- ifelse(sub$Time == "", yes = paste("12:00:00"), no = paste(sub$Time))
time_check <- subset(sub, sub$SiteID == "USGS-333656112113701" & sub$Date == "1966-04-27")
rm(time_check)

## Adjust to create DateTime column 
sub$Date <- as.POSIXct(as.character(sub$Date), format="%Y-%m-%d")
sub$Time2 <- as.POSIXct(paste(as.character(sub$Date), as.character(sub$Time)), format="%Y-%m-%d %H:%M:%S")
sub$DateTime <- ifelse(is.na(sub$Time2)==FALSE, yes=paste(sub$Time2), no=paste(sub$Date))

# replaced this process with line 37
# add noon time stamp to all DateTimes without a time
# format_time <- function(xdat){
#   xdat <- as.data.frame(xdat)
#   
#   xdat <- xdat %>% separate(DateTime, c("Date", "Time"),sep = " ")
#   xdat$Time[is.na(xdat$Time) == TRUE] <- paste("12:00:00")
#   xdat$DateTime <- lubridate::ymd_hms(paste(xdat$Date, xdat$Time))
#   return(xdat)
# }

# sub <- format_time(sub)
# Warning message:
#   Expected 2 pieces. Missing pieces filled with `NA` in 446971 rows [136, 142, 143, 144, 148, 157, 158, 159, 160, 161, 175, 176, 179, 184, 188, 189, 191, 208, 215, 225, ...]

sub <- select(sub, -c("Time2", "Time"))

head(sub,20)
sapply(sub, class)

#split by parameter
sub$Value <- as.numeric(as.character(sub$Value))
sub_split <- split(sub, sub$Parameter)
head(sub_split, 20)

## Alkalinity
sub_split$`Alkalinity, total`$Units <- factor(sub_split$`Alkalinity, total`$Units)
# identify what units there are
levels(sub_split$`Alkalinity, total`$Units)
# identify how many of each type of unit exists in the data
length(sub_split$`Alkalinity, total`$Units[sub_split$`Alkalinity, total`$Units == "uS/cm"])
# get rid of rare units & blank units
sub_split$`Alkalinity, total` <- sub_split$`Alkalinity, total`[-which(sub_split$`Alkalinity, total`$Units == ""),]
sub_split$`Alkalinity, total` <- sub_split$`Alkalinity, total`[-which(sub_split$`Alkalinity, total`$Units == "ueq/L"),]
sub_split$`Alkalinity, total` <- sub_split$`Alkalinity, total`[-which(sub_split$`Alkalinity, total`$Units == "meq/L"),]
sub_split$`Alkalinity, total` <- sub_split$`Alkalinity, total`[-which(sub_split$`Alkalinity, total`$Units == "uS/cm"),]
#Convert all values not in ppm to ppm
#converted mg/l CaCO3 directly to mg/l
#ppm = mg/l
sub_split$`Alkalinity, total`$Units[which(sub_split$`Alkalinity, total`$Units == "mg/l")] <- "ppm"
sub_split$`Alkalinity, total`$Units[which(sub_split$`Alkalinity, total`$Units == "mg/l CaCO3")] <- "ppm"
sub_split$`Alkalinity, total`[which(sub_split$`Alkalinity, total`$Units == "ug/l"),]$Value <- sub_split$`Alkalinity, total`[which(sub_split$`Alkalinity, total`$Units == "ug/l"),]$Value/1000
sub_split$`Alkalinity, total`$Units[which(sub_split$`Alkalinity, total`$Units == "ug/l")] <- "ppm"
#get rid of unused levels
sub_split$`Alkalinity, total`$Units <- factor(sub_split$`Alkalinity, total`$Units)
#check that only the desired level is left
levels(sub_split$`Alkalinity, total`$Units)

## Calcium
sub_split$Calcium$Units <- factor(sub_split$Calcium$Units)
levels(sub_split$Calcium$Units)
length(sub_split$Calcium$Units[sub_split$Calcium$Units == "ug/l"])
sub_split$Calcium <- sub_split$Calcium [-which(sub_split$Calcium$Units == ""),]
sub_split$Calcium <- sub_split$Calcium [-which(sub_split$Calcium$Units == "%"),]
sub_split$Calcium <- sub_split$Calcium [-which(sub_split$Calcium$Units == "% recovery"),]
sub_split$Calcium <- sub_split$Calcium [-which(sub_split$Calcium$Units == "lb/day"),]
sub_split$Calcium <- sub_split$Calcium [-which(sub_split$Calcium$Units == "ueq/L"),]
sub_split$Calcium <- sub_split$Calcium [-which(sub_split$Calcium$Units == "ug/g"),]
sub_split$Calcium <- sub_split$Calcium [-which(sub_split$Calcium$Units == "mg/kg"),]
sub_split$Calcium[which(sub_split$Calcium$Units == "ug/l"),]$Value <- sub_split$Calcium[which(sub_split$Calcium$Units == "ug/l"),]$Value/1000
sub_split$Calcium$Units[which(sub_split$Calcium$Units == "ug/l")] <- "ppm"
sub_split$Calcium$Units[which(sub_split$Calcium$Units == "mg/l")] <- "ppm"
sub_split$Calcium$Units[which(sub_split$Calcium$Units == "mg/l CaCO3")] <- "ppm"
sub_split$Calcium$Units <- factor(sub_split$Calcium$Units)
levels(sub_split$Calcium$Units)

##Magnesium
sub_split$Magnesium$Units <- factor(sub_split$Magnesium$Units)
levels(sub_split$Magnesium$Units)
length(sub_split$Magnesium$Units[sub_split$Magnesium$Units == "ug/g"])
sub_split$Magnesium <- sub_split$Magnesium[-which(sub_split$Magnesium$Units == ""),]
sub_split$Magnesium <- sub_split$Magnesium[-which(sub_split$Magnesium$Units == "%"),]
sub_split$Magnesium <- sub_split$Magnesium[-which(sub_split$Magnesium$Units == "% recovery"),]
sub_split$Magnesium <- sub_split$Magnesium[-which(sub_split$Magnesium$Units == "lb/day"),]
sub_split$Magnesium <- sub_split$Magnesium[-which(sub_split$Magnesium$Units == "ueq/L"),]
sub_split$Magnesium <- sub_split$Magnesium[-which(sub_split$Magnesium$Units == "ug/g"),]
sub_split$Magnesium <- sub_split$Magnesium[-which(sub_split$Magnesium$Units == "mg/kg"),]
sub_split$Magnesium$Units[which(sub_split$Magnesium$Units == "mg/l")] <- "ppm"
sub_split$Magnesium$Units[which(sub_split$Magnesium$Units == "mg/l CaCO3")] <- "ppm"
sub_split$Magnesium[which(sub_split$Magnesium$Units == "ug/l"),]$Value <- sub_split$Magnesium[which(sub_split$Magnesium$Units == "ug/l"),]$Value/1000
sub_split$Magnesium$Units[which(sub_split$Magnesium$Units == "ug/l")] <- "ppm"
sub_split$Magnesium$Units <- factor(sub_split$Magnesium$Units)
levels(sub_split$Magnesium$Units)

##pH
sub_split$pH$Units <- factor(sub_split$pH$Units)
levels(sub_split$pH$Units)
length(sub_split$pH$Units[sub_split$pH$Units == ""])
#change all unites to "None"
#first make sure they are all normal values by subsetting only those 0-14
sub_split$pH <- subset(sub_split$pH, Value >= 0 & Value <= 14)
#subset removed "", 24 values from "None", 
sub_split$pH <- sub_split$pH[-which(sub_split$pH$Units == "mg/l"),]
sub_split$pH <- sub_split$pH[-which(sub_split$pH$Units == "#/100 gal"),]
sub_split$pH <- sub_split$pH[-which(sub_split$pH$Units == "Mole/l"),]
sub_split$pH <- sub_split$pH[-which(sub_split$pH$Units == "ug/l"),]
sub_split$pH <- sub_split$pH[-which(sub_split$pH$Units == ""),]
sub_split$pH <- sub_split$pH[-which(sub_split$pH$Units == "mg/kg"),]
sub_split$pH <- sub_split$pH[-which(sub_split$pH$Units == "deg C"),]
sub_split$pH$Units[which(sub_split$pH$Units == "units/cm")] <- "None"
sub_split$pH$Units[which(sub_split$pH$Units == "std units")] <- "None"
sub_split$pH$Units[which(sub_split$pH$Units == "count")] <- "None"
sub_split$pH$Units <- factor(sub_split$pH$Units)
levels(sub_split$pH$Units)

##Potassium
sub_split$Potassium$Units <- factor(sub_split$Potassium$Units)
levels(sub_split$Potassium$Units)
length(sub_split$Potassium$Units[sub_split$Potassium$Units == "ug/l"])
sub_split$Potassium <- sub_split$Potassium[-which(sub_split$Potassium$Units == ""),]
sub_split$Potassium <- sub_split$Potassium[-which(sub_split$Potassium$Units == "%"),]
sub_split$Potassium <- sub_split$Potassium[-which(sub_split$Potassium$Units == "% recovery"),]
sub_split$Potassium <- sub_split$Potassium[-which(sub_split$Potassium$Units == "deg C"),]
sub_split$Potassium <- sub_split$Potassium[-which(sub_split$Potassium$Units == "mg/kg"),]
sub_split$Potassium <- sub_split$Potassium[-which(sub_split$Potassium$Units == "ml"),]
sub_split$Potassium <- sub_split$Potassium[-which(sub_split$Potassium$Units == "None"),]
sub_split$Potassium <- sub_split$Potassium[-which(sub_split$Potassium$Units == "tons/day"),]
sub_split$Potassium <- sub_split$Potassium[-which(sub_split$Potassium$Units == "ueq/L"),]
sub_split$Potassium[which(sub_split$Potassium$Units == "ug/l"),]$Value <- sub_split$Potassium[which(sub_split$Potassium$Units == "ug/l"),]$Value/1000
sub_split$Potassium[which(sub_split$Potassium$Units == "ug/l"),]$Units <- "ppm"
sub_split$Potassium[which(sub_split$Potassium$Units == "mg/l"),]$Units <- "ppm"
sub_split$Potassium$Units <- factor(sub_split$Potassium$Units)
levels(sub_split$Potassium$Units)

##Salinity
sub_split$Salinity$Units <- factor(sub_split$Salinity$Units)
levels(sub_split$Salinity$Units)
length(sub_split$Salinity$Units[sub_split$Salinity$Units == "PSU"])
sub_split$Salinity <- sub_split$Salinity[-which(sub_split$Salinity$Units == ""),]
sub_split$Salinity <- sub_split$Salinity[-which(sub_split$Salinity$Units == "mg/mL @25C"),]
sub_split$Salinity <- sub_split$Salinity[-which(sub_split$Salinity$Units == "PSU"),]
sub_split$Salinity <- sub_split$Salinity[-which(sub_split$Salinity$Units == "PSS"),]
sub_split$Salinity <- sub_split$Salinity[-which(sub_split$Salinity$Units == "%"),]
sub_split$Salinity[which(sub_split$Salinity$Units == "g/l"),]$Value <- sub_split$Salinity[which(sub_split$Salinity$Units == "g/l"),]$Value*1000
sub_split$Salinity$Units[which(sub_split$Salinity$Units == "g/l")] <- "ppm"
sub_split$Salinity[which(sub_split$Salinity$Units == "ppt"),]$Value <- sub_split$Salinity[which(sub_split$Salinity$Units == "ppt"),]$Value*0.000001
sub_split$Salinity$Units[which(sub_split$Salinity$Units == "ppt")] <- "ppm"
sub_split$Salinity[which(sub_split$Salinity$Units == "ppth"),]$Value <- sub_split$Salinity[which(sub_split$Salinity$Units == "ppth"),]$Value/0.001
sub_split$Salinity$Units[which(sub_split$Salinity$Units == "ppth")] <- "ppm"
sub_split$Salinity$Units[which(sub_split$Salinity$Units == "mg/l")] <- "ppm"
sub_split$Salinity$Units <- factor(sub_split$Salinity$Units)
levels(sub_split$Salinity$Units)

##Sodium
sub_split$Sodium$Units <- factor(sub_split$Sodium$Units)
levels(sub_split$Sodium$Units)
length(sub_split$Sodium$Units[sub_split$Sodium$Units == "ueq/L"])
sub_split$Sodium <- sub_split$Sodium[-which(sub_split$Sodium$Units == ""),]
sub_split$Sodium <- sub_split$Sodium[-which(sub_split$Sodium$Units == "None"),]
sub_split$Sodium <- sub_split$Sodium[-which(sub_split$Sodium$Units == "%"),]
sub_split$Sodium <- sub_split$Sodium[-which(sub_split$Sodium$Units == "% recovery"),]
sub_split$Sodium <- sub_split$Sodium[-which(sub_split$Sodium$Units == "mg/kg"),]
sub_split$Sodium <- sub_split$Sodium[-which(sub_split$Sodium$Units == "ueq/L"),]
sub_split$Sodium$Units[which(sub_split$Sodium$Units == "mg/l")] <- "ppm"
sub_split$Sodium[which(sub_split$Sodium$Units == "ug/l"),]$Value <- sub_split$Sodium[which(sub_split$Sodium$Units == "ug/l"),]$Value/1000
sub_split$Sodium[which(sub_split$Sodium$Units == "ug/l"),]$Units <- "ppm"
sub_split$Sodium$Units <- factor (sub_split$Sodium$Units)
levels(sub_split$Sodium$Units)

##Specific Conductance
sub_split$`Specific conductance`$Units <- factor(sub_split$`Specific conductance`$Units)
levels(sub_split$`Specific conductance`$Units)
length(sub_split$`Specific conductance`$Units[sub_split$`Specific conductance`$Units == "uS/cm @25C"])
sub_split$`Specific conductance` <- sub_split$`Specific conductance`[-which(sub_split$`Specific conductance`$Units == ""),]
sub_split$`Specific conductance` <- sub_split$`Specific conductance`[-which(sub_split$`Specific conductance`$Units == "None"),]
sub_split$`Specific conductance` <- sub_split$`Specific conductance`[-which(sub_split$`Specific conductance`$Units == "ppm"),]
sub_split$`Specific conductance` <- sub_split$`Specific conductance`[-which(sub_split$`Specific conductance`$Units == "ug/l"),]
sub_split$`Specific conductance` <- sub_split$`Specific conductance`[-which(sub_split$`Specific conductance`$Units == "mg/l"),]
sub_split$`Specific conductance`[which(sub_split$`Specific conductance`$Units == "mS/cm"),]$Value <- sub_split$`Specific conductance`[which(sub_split$`Specific conductance`$Units =="mS/cm"),]$Value/0.001
sub_split$`Specific conductance`$Units[which(sub_split$`Specific conductance`$Units == "mS/cm")] <- "uS/cm"
sub_split$`Specific conductance`$Units[which(sub_split$`Specific conductance`$Units == "umho/cm")] <- "uS/cm"
sub_split$`Specific conductance`[which(sub_split$`Specific conductance`$Units == "mho/cm"),]$Value <- sub_split$`Specific conductance`[which(sub_split$`Specific conductance`$Units =="mho/cm"),]$Value/1000000
sub_split$`Specific conductance`$Units[which(sub_split$`Specific conductance`$Units == "mho/cm")] <- "uS/cm"
sub_split$`Specific conductance`$Units[which(sub_split$`Specific conductance`$Units == "uS/cm     ")] <- "uS/cm"
sub_split$`Specific conductance`$Units[which(sub_split$`Specific conductance`$Units == "uS/cm @25C")] <- "uS/cm"
sub_split$`Specific conductance`$Units[which(sub_split$`Specific conductance`$Units == "umho")] <- "uS/cm"
sub_split$`Specific conductance`$Units <- factor(sub_split$`Specific conductance`$Units)
levels(sub_split$`Specific conductance`$Units)

#recombine sub_split data 
merged <- ldply(sub_split, data.frame)
#subset columns
sub_merged <- merged[,c("Parameter","SiteID","DateTime","Value", "TimeZone", "HUC")]
sb_melt <- melt(sub_merged, id.vars = c("Parameter", "SiteID","DateTime", "TimeZone", "HUC"))
head(sb_melt)
sb_melt <- sb_melt[,-c(6)]
sapply(sb_melt, class)

#rm(sub, sub_split, sub_merged, state_file)

#check for duplicates
test <- sb_melt[duplicated(sb_melt),]
rm(test)
#remove duplicates
sb_melt <- sb_melt[!duplicated(sb_melt),]
#go from long to wide format data
sb <- dcast(unique(sb_melt), SiteID + DateTime + TimeZone + HUC ~ Parameter, value.var = "value", fun.aggregate = mean)
sapply(sb, class)

# replace NaNs with NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

sb[is.nan(sb)] <- NA

dat <- sb
#rm(sb)

# we shouldnt need the next lines because of how we used reshape2 rather then joining. We already have means.

## Average across duplicate DateTimes
#dat$DateTimeID <- paste(dat$DateTime,dat$SiteID)
# sapply(dat, class)
# names(dat)
# dat_sum <- dat %>% group_by(as.factor(dat$DateTimeID))%>%
#   summarise_at(.vars = c( "Alkalinity, total", "Calcium", "Magnesium", "pH", "Potassium",           
#                           "Salinity", "Sodium", "Specific conductance"), .funs=mean, na.rm=T)
# names(dat_sum)

## Rejoin with other info
# info <- sub_split$`mg/l`
# info$DateTimeID <- paste(info$DateTime,info$SiteID)
# info$DateTimeID <- as.factor(info$DateTimeID)
# 
# dat_rejoin <- left_join(dat_sum, info[,c("DateTime","TimeZone","SiteID","DateTimeID")], by="DateTimeID", all.x=T)
# dat_rejoin <- unique(dat_rejoin)
#dat <- dat_rejoin

## Adjust TimeZone to relative to UTC
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
df <- dat[,c("SiteID","DateTime","UTC_TimeZone","HUC","Alkalinity, total", "Calcium", "Magnesium", "pH", "Potassium",           
             "Salinity", "Sodium", "Specific conductance")]

setwd("~/Desktop/Blaszczak Lab/GB CO WQ Data/WQP Formatted TS")
## Write CSV
write.csv(df, "WQ_TS.csv")




