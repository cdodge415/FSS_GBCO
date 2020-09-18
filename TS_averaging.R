#################### Lauren Bolotin - bolotinljb@gmail.com ######################
# Visualize what multiple years of data at one sight looks like plotted on top of each other
# Create "averaged" (or quantile) time series of specific conductance for each site of interest
#################################################################################
rm(list=ls())
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

filter_dat <- dat # we will use this later

count(dat$SiteID) # choose some sites that have a lot of data

## Example Plots #####
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
######

## Example Plots of Mean Time Series #####
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
#####

## Example Plots of Upper Quartile Time Series ####
quant75 <- function(x){
  x <- quantile(x, .75)
}

up_quart <- dat
up_quart <- up_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("upper_quart" = quant75))

# check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
# check_quant <- quantile(check$SpC, .75)
# # all good, check one more time
# 
# check <- subset(dat, dat$SiteID == "USGS-09306500" & dat$doy == 121)
# check_quant <- quantile(check$SpC, .75)
# # all good

ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "black")

## Example Plots of Median (Middle Quartile) Time Series ####
med <- dat
med <- med %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("median" = median))

# check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
# check_med <- median(check$SpC)
# rm(check, check_med)

ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")

## Example Plots of Upper Quartile Time Series ####
quant25 <- function(x){
  x <- quantile(x, .25)
}

low_quart <- dat
low_quart <- low_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("lower_quart" = quant25))

# check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
# check_quant <- quantile(check$SpC, .25)
# # all good, check one more time
# 
# check <- subset(dat, dat$SiteID == "USGS-09306500" & dat$doy == 121)
# check_quant <- quantile(check$SpC, .25)
# # all good

ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  theme(legend.position = "none")+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")

## Plot all data with all quantiles overlain ####
# Just quantiles
ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  labs(y = "SpC")+
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "gray41")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "grey66")

# All data + quantiles + mean
ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "black")+
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean), color = "red")

# Iterate through all sites to make the plot of all data + quantiles + mean

# Code for creating PDFs of plots in R: 
# pdf("rplot.pdf") 
# # 2. Create a plot
# plot(x = my_data$wt, y = my_data$mpg,
#      pch = 16, frame = FALSE,
#      xlab = "wt", ylab = "mpg", col = "#2E9FDF")
# # Close the pdf file
# dev.off()

setwd("/Volumes/Blaszczak Lab/FSS/Figures/SingleTSPlots")
# x <- "USGS-09034500"


sites <- levels(dat$SiteID) # make this a subset of sites with pretty continuous data

plotSpC <- function(x){
  pdf(paste0(x, "_singleTS.pdf"))
  p <- ggplot(subset(dat, dat$SiteID == x))+
    theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
    geom_line(subset(low_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = lower_quart), color = "black")+
    geom_line(subset(med, avg$SiteID == x), mapping = aes(x = doy, y = median), color = "black")+
    geom_line(subset(up_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = upper_quart), color = "black")+
    geom_line(subset(avg, avg$SiteID == x), mapping = aes(x = doy, y = mean), color = "red")
  print(p)
  dev.off()
}

# plotSpC(x)

lapply(sites, plotSpC)


## Filter for site-years with enough data to do dynamic time warping (using df we created earler "filter_dat")
# we want to tally how many sites have >350 obs. in each year
# then we will only use years with >90% of sites 

colnames(filter_dat)
