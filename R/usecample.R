library(acs)
library(ggplot2)
library(ggmap)
library(UScensus2010)
library(RColorBrewer)
library(dplyr)
library(scales)

#http://api.census.gov/data/key_signup.html
# api.key.install(key="c369cd6ed053a84332caa62301eb8afe98bed825")

# Load in Shape File (You'll need to download this file from the census)
#ftp://ftp2.census.gov/geo/tiger/TIGER2013/TRACT/tl_2013_40_tract.zip

## load, subset shapefile
geodat<-readShapePoly("insert shapefile here", proj4string=CRS('+proj=longlat +datum=NAD83'))
geodat<-geodat[geodat$COUNTYFP==109,]

## fortify for ggplot digestion
geodat.f<-fortify(geodat,region="GEOID")

# American Community Survey Data: Median HH Income for OK Census Tracts
ok.counties=geo.make(state="OK", county="Oklahoma", tract="*")
ok.income<-acs.fetch(geography=ok.counties, table.number="B19013", endyear=2013)


# Merge Data Sets 
geo_dat<-geography(ok.income)
var_dat<-as.data.frame(estimate(ok.income))
acs_data<-cbind(geo_dat,var_dat)
acs_data$id<- paste("40109", acs_data$tract, sep = "")

## from dplyr
mapdata<-left_join(geodat.f,acs_data)

okc <- ggplot() +
  geom_polygon(data = mapdata, aes(x = long, y = lat, group = group,
                                   fill = B19013_001), color = "black", size = 0.5)+
  scale_fill_distiller(palette = "Reds", labels = comma,
                       breaks = pretty_breaks(n = 10), values = c(1,0)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  ggtitle('Map of OKC')