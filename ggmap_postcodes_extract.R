#finding geo code information
#ANthony Davidson
#29/08/2021
# Library
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format

# Create data 
data <- data.frame(
  time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), 
  value=runif(41)
)

# Double check time is at the date format
str(data$time)

# Switch to XTS format
data <- xts(x = data$value, order.by = data$time)

# Default = line plot --> See chart #316

# Add points
p <- dygraph(data) %>%
  dyOptions( drawPoints = TRUE, pointSize = 4 )
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/dygraphs317-1.html"))
#Capture data








ggmap::geocode(paste(c( 2001, 2050, 3123, 4254 ), 
                       "Australia"))
