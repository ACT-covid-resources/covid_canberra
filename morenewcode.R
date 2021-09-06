library(leaflet)

## default position is topleft next to zoom control
library(png)
library(mapview)
img <- "https://www.r-project.org/logo/Rlogo.svg"
leaflet() %>% addTiles() %>% plotladdLogo(img, url = "https://www.r-project.org/logo/")

## with local image

library(plotly)
img <- system.file("img", "Rlogo.png", package="png")
leaflet() %>% addTiles() %>% addLogo(img, src = "local", alpha = 0.3)

## dancing banana gif :-)
m <- mapview(breweries91)

addLogo(m, "https://jeroenooms.github.io/images/banana.gif",
        position = "bottomleft",
        offset.x = 5,
        offset.y = 40,
        width = 100,
        height = 100)

%>%
        # addPolygons()
