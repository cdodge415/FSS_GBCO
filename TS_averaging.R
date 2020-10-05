#################### Lauren Bolotin - bolotinljb@gmail.com ######################
# Visualize what multiple years of data at one sight looks like plotted on top of each other
# Create "averaged" (or quantile) time series of specific conductance for each site of interest
#################################################################################
rm(list=ls())
x <- c("tidyverse", "data.table", "lubridate")
lapply(x, require, character.only = TRUE)
rm(x)
## Set the theme for our ggplots
theme_set(theme(legend.position = "none",panel.background = element_blank(), 
                axis.line = element_line(colour = "black")))

## Bring in data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("continuous_SC_Q_data.rds")
sapply(dat, class)
dat$Date <- as.POSIXct(as.character(dat$Date), format = "%Y-%m-%d")
dat$Year <- year(dat$Date) 
dat$Year <- as.factor(dat$Year)
dat$doy <- strftime(dat$Date, format = "%j")
dat$doy <- as.numeric(as.character(dat$doy))

## NOT FLOW CORRECTED ####################
## Example Plots of Mean Time Series #####
avg <- dat
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("mean" = mean))

# check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
# check_avg <- mean(check$SpC)
# rm(check, check_avg) # all good

ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean), color = "black")
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
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")

## Example Plots of Lower Quartile Time Series ####
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
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")

## Plot all data with all quantiles overlain ####
# Just quantiles
ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  labs(y = "SpC")+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "gray41")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "grey66")

# All data + quantiles + mean
ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "black")+
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean), color = "red")

# Iterate through all sites to make the plot of all data + quantiles + mean
### Code for creating PDFs of plots in R: 
## For one site:
# pdf("rplot.pdf") 
# # 2. Create a plot
# plot(x = my_data$wt, y = my_data$mpg,
#      pch = 16, frame = FALSE,
#      xlab = "wt", ylab = "mpg", col = "#2E9FDF")
# # Close the pdf file
# dev.off()

# setwd("/Volumes/Blaszczak Lab/FSS/Figures/SingleTSPlots")
# x <- "USGS-09034500"
# sites <- levels(dat$SiteID) 

## Function for multiple sites (and all available data)
# plotSpC <- function(x){
#   pdf(paste0(x, "_singleTS.pdf"))
#   p <- ggplot(subset(dat, dat$SiteID == x))+
#     theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#     geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
#     geom_line(subset(low_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = lower_quart), color = "black")+
#     geom_line(subset(med, avg$SiteID == x), mapping = aes(x = doy, y = median), color = "black")+
#     geom_line(subset(up_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = upper_quart), color = "black")+
#     geom_line(subset(avg, avg$SiteID == x), mapping = aes(x = doy, y = mean), color = "red")
#   print(p)
#   dev.off()
# }

# plotSpC(x)
# lapply(sites, plotSpC)

# Bring in list of sites with continuous SC data
dat$SiteID <- factor(dat$SiteID)
levels(dat$SiteID) # 85 sites

# is.nan.data.frame <- function(x)
#   do.call(cbind, lapply(x, is.nan))
# 
# dat[is.nan(dat)] <- NA

## Rerun Quantile and Mean Code for Filtered Dataset ####
## Mean
avg <- dat
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Spc_Qcms", .funs = c("mean" = mean))

## Upper Quantile
up_quart <- dat
up_quart <- up_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Spc_Qcms", .funs = c("upper_quart" = quant75))

## Median (Middle Quatile)
med <- dat
med <- med %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Spc_Qcms", .funs = c("median" = median))

## Lower Quantile
low_quart <- dat
low_quart <- low_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Spc_Qcms", .funs = c("lower_quart" = quant25))

## Rerun Code to Make PDF's of All Plots of All Data + Quantile + Mean
dat$SiteID <- factor(dat$SiteID)
sites <- levels(dat$SiteID)

setwd("/Volumes/Blaszczak Lab/FSS/Figures/SingleTSPlots")
plotSpC <- function(x){
  pdf(paste0(x, "_singleTS_flowcorrected.pdf"))
  p <- ggplot(subset(dat, dat$SiteID == x))+
    theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_line(mapping = aes(x = doy, y = Spc_Qcms, color = Year))+
    geom_line(subset(low_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = lower_quart), color = "black")+
    geom_line(subset(med, avg$SiteID == x), mapping = aes(x = doy, y = median), color = "black")+
    geom_line(subset(up_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = upper_quart), color = "black")+
    geom_line(subset(avg, avg$SiteID == x), mapping = aes(x = doy, y = mean), color = "red")
  print(p)
  dev.off()
}

# plotSpC(x)

lapply(sites, plotSpC) # already ran and saved PDFs

# Some plots only show one line, this is because those sites only have 1 year of continuous dat(a
dat$Year <- factor(sub_cont$Year)

## REDO THIS: #####
# See how many sites any given year has SC data for
# the highest %age of sites we have in one year of continuous data is __% :( 
# make barplot

# ggplot(count_sy, aes(x = vars, y = n))+
#   geom_bar(stat = "identity", fill = "turquoise3")+
#   theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 40))+
#   labs(x = "Year", y = "Available Sites", title = "# of Continuous Sites per Year")

## Do the same thing for JUST discharge data: ############
##########################################################
avg <- dat
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("mean" = mean))

## Upper Quantile
up_quart <- dat
up_quart <- up_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("upper_quart" = quant75))

## Median (Middle Quatile)
med <- dat
med <- med %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("median" = median))

## Lower Quantile
low_quart <- dat
low_quart <- low_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("lower_quart" = quant25))

## Rerun Code to Make PDF's of All Plots of All Data + Quantile + Mean
setwd("/Volumes/Blaszczak Lab/FSS/Figures/SingleTSPlots")
plotSpC <- function(x){
  pdf(paste0(x, "_singleTS_flow_only.pdf"))
  p <- ggplot(subset(dat, dat$SiteID == x))+
    theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_line(mapping = aes(x = doy, y = Q_cms, color = Year))+
    geom_line(subset(low_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = lower_quart), color = "black")+
    geom_line(subset(med, avg$SiteID == x), mapping = aes(x = doy, y = median), color = "black")+
    geom_line(subset(up_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = upper_quart), color = "black")+
    geom_line(subset(avg, avg$SiteID == x), mapping = aes(x = doy, y = mean), color = "red")
  print(p)
  dev.off()
}

# plotSpC(x)

lapply(sites, plotSpC) # already ran and saved PDFs
  