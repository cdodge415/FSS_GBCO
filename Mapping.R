# SC DATA ####
# Example mapping code for Chloe
# Combine CL data into one df 
TWC_SC_CL$LatLong <- paste0(TWC_SC_CL$Lat, " ",TWC_SC_CL$Long)
TWC_SC_CL_avg <- TWC_SC_CL %>%
  group_by(LatLong, Lat, Long) %>%
  summarise_at(.vars = "SC", .funs = c("avg_SC" = mean))
  
TWC_T_CL_avg <- TWC_SC_CL %>%
  group_by(LatLong, Lat, Long) %>%
  summarise_at(.vars = "Temp", .funs = c("avg_Temp" = mean))

# MAPPING ####
# bring in site metadata
library(viridis)
library(ggplot2)
library(dplyr)
some.states <- c('california', 'nevada')
some.states.map <- map_data("state", region = some.states)

ggplot(some.states.map)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "white", color = "black")+
  geom_point(INSERT DF, mapping = aes(x = Lon, y = Lat, color = avg_SC))+
  scale_x_continuous(limits = c(-125,-101))+
  theme(panel.background = element_blank(),axis.title = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), axis.text = element_blank(), plot.title = element_text(hjust = 0.75))+
  labs(title = "Slope of USGS Daily SC Linear Models", color = "ΔSC/Time")+
  scale_color_viridis(option = "A")