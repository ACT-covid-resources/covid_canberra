
##act outbreak box...
leaflet() %>% addTiles() %>%
  addRectangles(
    lng1=-118.456554, lat1=34.078039,
    lng2=-118.436383, lat2=34.062717,
    fillColor = "transparent"
  )

## abs data/r package
#most currentist
library(devtools)
#install dev branch using
devtools::install_github("mitcda/raustats", ref = "devel")

