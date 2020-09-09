#################### Lauren Bolotin - bolotinljb@gmail.com ######################
# Visualize what multiple years of data at one sight looks like plotted on top of each other
# Create 1 time series of specific conductance for each site of interest
#################################################################################
x <- c("tidyverse", "data.table", "lubridate")
lapply(x, require, character.only = TRUE)
rm(x)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("all_SC_data.rds")
sapply(dat, class)
dat$Date <- as.POSIXct(as.character(dat$Date), format = "%Y-%m-%d")
dat$Year <- year(dat$Date) 
dat$Year <- as.factor(dat$Year)
dat$doy <- strftime(dat$Date, format = "%j")
dat$doy <- as.numeric(as.character(dat$doy))

count(dat$SiteID) # choose some sites that have a lot of data

ggplot(subset(dat, dat$SiteID == "USGS-09041400"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")

ggplot(subset(dat, dat$SiteID == "USGS-09071750"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")

ggplot(subset(dat, dat$SiteID == "USGS-09429490"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")

ggplot(subset(dat, dat$SiteID == "USGS-10350500"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")

ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")

dev.off()

avg <- dat
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("mean" = mean))

check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
check_avg <- mean(check$SpC)
rm(check, check_avg)

ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")+
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean), color = "black")

ggplot(subset(dat, dat$SiteID == "USGS-09429490"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")+
  geom_line(subset(avg, avg$SiteID == "USGS-09429490"), mapping = aes(x = doy, y = mean), color = "black")

ggplot(subset(dat, dat$SiteID == "USGS-10350500"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")+
  geom_line(subset(avg, avg$SiteID == "USGS-10350500"), mapping = aes(x = doy, y = mean), color = "black")

count(dat$SiteID)
# obviously if you choose a site with less data, things can look pretty funky
ggplot(subset(dat, dat$SiteID == "USGS-09416000"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")+
  geom_line(subset(avg, avg$SiteID == "USGS-09416000"), mapping = aes(x = doy, y = mean), color = "black")
