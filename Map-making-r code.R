###################Static Map################
rm(list=ls())



library(ggplot2)
library(MASS)
library(gridExtra)
library(adehabitatHR)
library(OpenStreetMap)
library(rgeos)
library(ggfortify)
library(sp)
library(ks)
library(raster)

install.packages('pacman')
pacman::p_load("ggsn","leaflet","mapdata","maptools","OpenStreetMap","rgdal","tidyverse")

mapdata <- read.csv("Bat_Map_Data_2021.csv")


mapdata$lat<-as.numeric(mapdata$lat)
mapdata$lon<-as.numeric(mapdata$lon)
## this defines the spatial extent of the
## workspace (+/- some area based on the data), and creates the basemap
## the base map comes from OpenStreetMap
max(mapdata$lat, na.rm=T)
max(mapdata$lon)
mapdata_raster <- openmap(c(max(mapdata$lat)+0.010, min(mapdata$lon)-0.010), + 
                            c(min(mapdata$lat)-0.010, max(mapdata$lon)+0.010), type = "bing")

## this puts the points on the map and adds various elements
## autoplot call is part of ggplot
mapdata_raster_proj <- openproj(mapdata_raster, projection = "+proj=longlat +datum=WGS84")


autoplot.OpenStreetMap(mapdata_raster_proj, expand = TRUE) + geom_point(data=mapdata, aes(lon, lat)) +
  geom_point(data = mapdata, aes(lon, lat), size = 3.0) + aes(color = Visual_Score) +
  geom_text(data=mapdata,aes(x=lon,y=lat,label=Site_ID), color="white", vjust=-0.60, size=4.01, fontface="bold")+
  theme(axis.title = element_text(face="bold")) + labs(x="Longitude", y="Latitude") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) + theme_bw()+
  north(mapdata, location = "bottomleft", scale = 0.15, symbol = 4, anchor = c(x= -87.15, y= 36.55)) + 
  scalebar(mapdata, dist = 2, dist_unit = "mi", transform = TRUE, model = "WGS84", location = "bottomleft", st.dist = 0.05, st.size = 1, anchor = c(x=-87.8,y=36.55)) +
  annotate("text", x = -87.6, y = 36.56, label = "Trevor Walker\n October2021", size = 3.5, color="white")+
  labs(title = "Fort Campbell Kentucky/Tennessee")
#################Making Interactive map with bird data####################

alldata <- read.csv("2020data.csv")

pal <- colorNumeric(
  palette = "RdBu",
  domain = alldata$Indivs.)


leaflet(alldata) %>% 
  addTiles() %>% 
  addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer", layers = "0")%>%
  addScaleBar()%>%
addCircleMarkers(data = alldata,
                 popup = alldata$Species,
                 color = ~pal(alldata$Indivs.),
                 radius = ~sqrt(Indivs.), group = 'Species - Indivs.')%>%
  addLegend("bottomright", pal = pal, values = ~Indivs.,
            title = "bird #'s",
            labFormat = labelFormat(prefix = ""),
            opacity = 1)



