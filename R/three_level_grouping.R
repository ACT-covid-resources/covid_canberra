##current dataset.. 
#import now
library(tidyverse)
library(ggmap)
library(leaflet)
library(plotly)
library(flexdashboard)
library(DT)
library(rgdal)
library(crosstalk)
library(absmapsdata)

############very ugly function But should work.....
##ARD sep2021

last_five_days_new <- function(dataList){
  
  
  
  
  ##absmapping package extension
  mapdata <- absmapsdata::sa12016 %>%
    filter(gcc_name_2016 == "Australian Capital Territory")
  
  glimpse(mapdata)
  
  mapdata1 <- mapdata %>% 
    filter(sa3_name_2016 == "North Canberra") 
  
  glimpse(mapdata1)
  
  scale1 <- mapdata%>%  # let's just look Melbourne
    ggplot() +
    ggspatial::annotation_map_tile() +
    geom_sf(aes(geometry = geometry, fill  = areasqkm_2016), col = "black", lty = 4) #+   # use the geometry variable
  # geom_point(aes(cent_long, cent_lat))  # use the centroid long (x) and lats (y)
  
  # map
  
  mapdata <- absmapsdata::sa22016 %>%
    filter(gcc_name_2016 == "Australian Capital Territory")
  
  glimpse(mapdata)
  
  mapdata1 <- mapdata %>% 
    filter(sa3_name_2016 == "North Canberra") 
  
  glimpse(mapdata1)
  
  scale2 <- mapdata%>%  # let's just look Melbourne
    ggplot() +
    ggspatial::annotation_map_tile() +
    geom_sf(aes(geometry = geometry, fill  = areasqkm_2016), col = "black", lty = 4) #+   # use the geometry variable
  # geom_point(aes(cent_long, cent_lat))  # use the centroid long (x) and lats (y)
  
  
  mapdata <- absmapsdata::sa32016 %>%
    filter(gcc_name_2016 == "Australian Capital Territory")
  
  glimpse(mapdata)
  
  mapdata1 <- mapdata %>% 
    filter(sa3_name_2016 == "North Canberra") 
  
  glimpse(mapdata1)
  
  scale3 <- mapdata %>%  # let's just look Melbourne
    ggplot() +
    ggspatial::annotation_map_tile() +
    geom_sf(aes(geometry = geometry, fill  = areasqkm_2016), col = "black", lty = 4) #+   # use the geometry variable
  # geom_point(aes(cent_long, cent_lat))  # use the centroid long (x) and lats (y)
  
  gridExtra::grid.arrange(scale1, scale2, scale3, ncol = 2)
  # map
  # map
  
  
  
  mapdata %>%  # let's just look Melbourne
    ggplot() +
    ggspatial::annotation_map_tile() +
    geom_sf(aes(geometry = geometry, 
                fill  = areasqkm_2016), 
            col = "black", 
            lty = 4) #+   # use the geometry variable
  # geom_point(aes(cent_long, cent_lat))  # use the centroid long (x) and lats (y)
  # +
  # coord_quickmap()
  
  #latest dataset
  #or any other curated dataset
  # tab3 <- read.csv("https://raw.githubusercontent.com/green-striped-gecko/covid_canberra/main/data/last.csv")
  # # Aggregate method
  # 
  # tab3 <- read.csv("./data/allfiles/September/table_01_Sep_2021_136pm.csv")
  # # Aggregate method
  # tab3 <- read.csv("./data/allfiles/September/table_01_Sep_2021_308pm.csv")
  # #no new sites test
  # tab3 <- read.csv("./data/allfiles/August/table_21_Aug_2021_713pm.csv")
  
  #no new sites test
  tab3 <- read.csv(dataList)
  
  
  
  cols <- c( "red", "yellow","blue")
  
  addBuses <- FALSE
  
  tab3$Contact <- factor(tab3$Contact,levels=c(  "Close"  , "Casual", "Monitor") )
  cc <- as.numeric( tab3$Contact)
  ncols <- c("black","cyan")
  nn <- as.numeric(factor(tab3$Status))
  nn2 <- ifelse(nn==1,nn, 3)
  
  glimpse(tab3)
  
  tab3$Date <- substr(tab3$Date,1,10) #ditch day of the week
  labs <- paste(tab3$Contact, tab3$Status,tab3$Exposure.Location, tab3$Street, tab3$Suburb, tab3$Date,tab3$Arrival.Time, tab3$Departure.Time, tab3$doubles, sep="<br/>") 
  
  dataJoin <- absmapsdata::sa12016 %>%
    filter(gcc_name_2016 == "Australian Capital Territory") %>%
    mutate(Suburb = sa2_name_2016) %>%
    left_join(tab3, by = c("Suburb")) %>%
    select(Suburb, lat, lon, Exposure.Location, geometry,Status, Contact) %>%
    na.omit() %>%
    mutate(Status = ifelse(Status == "New", "New", "Previous"))
  
  glimpse(dataJoin)
  # <NA> and NA
  
  # remove_missing(vars = c("Exposure.Location"))%>%  # let's just look Melbourne
  plot1 <-  ggplot(dataJoin,aes(y=lat, x=lon)) +
    ggspatial::annotation_map_tile() +
    geom_sf(aes(geometry = geometry, fill = Status), 
            col = c("red"), 
            lty = 1, alpha =0.5)+ 
    scale_fill_manual(values = c("red", "yellow","blue")) + 
    geom_point(data = dataJoin, aes(y=lat, x=lon), size = 4, alpha = 1) #+ 
  # scale_fill_manual(values = c("red", "yellow","blue","black", "white")) +
  # scale_color_manual(values = c("red", "yellow","blue","black", "white")) 
  
  return(plot1)  
  
}  #function end


#lapply
p1 <- last_five_days_new(dataList = "./data/allfiles/September/table_01_Sep_2021_136pm.csv")


# dataList = "./data/allfiles/September/table_01_Sep_2021_136pm.csv"
# lapply(dataList, last_five_days_new)

p1

p2 <- last_five_days_new(dataList = "./data/allfiles/August/table_30_Aug_2021_236pm.csv")


# dataList = "./data/allfiles/September/table_01_Sep_2021_136pm.csv"
# lapply(dataList, last_five_days_new)

p2

 #+ 
   # geom_sf() +
   # coord_sf()+ 
   # geom_point(aes(y=lat, x=lon, col = Contact), size = 4, alpha = 0.6) + 
   # scale_color_manual(values = c( "red", "yellow","blue"))
#  
#  #this is not working yet
#  plotly::ggplotly(plot1)
# 
# p1 #+
#   #geom_point(data = tab3, aes(x=lat, y=lon))
# 
# leaflet(p1) %>% addCircleMarkers(lat=tab3$lat, lng=tab3$lon,popup = labs, weight=nn2, fillColor = cols[cc],color=ncols[nn], opacity =0.8, radius = 5 , fillOpacity = 0.8)
