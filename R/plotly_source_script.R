#source function for data from current status
#ARD
#28/08/2021

#libraries
library(lubridate)
library(tidyverse)
library(plotly)

#import data
#note this is still manual for this site
tab3 <- read_csv("https://raw.githubusercontent.com/green-striped-gecko/covid_canberra/main/data/last.csv")

#baseline csv of last has following variables
#THESE NEED TO BE STABLE
# colnames(tab3)

tab4 <- tab3 %>%
  mutate(colsN = factor(Contact, levels = c("Close", "Casual", "Monitor","Investigation location")),
         Contact = factor(Contact, levels = c("Close", "Casual","Monitor", "Investigation location")))

#fix levels so grouping works
levels(tab4$colsN) <- c("purple", "red","orange",  "grey50")
levels(tab4$colsN) <- c("red", "yellow","blue",  "teal")
# table(tab4$colsN)

##restructure ploting varibles
tab5 <- tab4 %>%
  mutate(conDate = as.Date(lubridate::dmy(Date)),
         locName = factor(Suburb),
         datNum = lubridate::day(conDate),
         weekname = lubridate::week(conDate))

Encoding(x = tab5$Exposure.Location) <- "UTF-8"
# ?Encoding
# replace all non UTF-8 character strings with an empty space
tab5$Exposure.Location <-
  iconv( x = tab5$Exposure.Location
         , from = "UTF-8"
         , to = "UTF-8"
         , sub = "" )

Encoding(x = tab5$Suburb) <- "UTF-8"
# ?Encoding
# replace all non UTF-8 character strings with an empty space
tab5$Suburb <-
  iconv( x = tab5$Suburb
         , from = "UTF-8"
         , to = "UTF-8"
         , sub = "" )

##by suburb
sum1 <- tab5 %>%
  dplyr::group_by(Suburb, Status, Contact) %>%
  dplyr::summarise(casecount = length(unique(Date)),
                   time = conDate) %>%
  ungroup()



a <- as.data.frame(table(tab5$locName))

colnames(a) <- c("locName", "contactcount")

# head(a)
# str(a)
# filter(a, contactcount >=1)

plotsumms <- right_join(tab5, a)

# just one summary per suburb
b <- distinct(plotsumms, Suburb, .keep_all = TRUE)

plotsumms$locName <- droplevels(plotsumms$locName)

# pre-processing
# ensure that all characters in the `Name` column
# are valid UTF-8 encoded
# Thank you to SO for this gem 
# https://stackoverflow.com/questions/17291287/how-to-identify-delete-non-utf-8-characters-in-r
Encoding(x = plotsumms$Exposure.Location) <- "UTF-8"
# ?Encoding
# replace all non UTF-8 character strings with an empty space
plotsumms$Exposure.Location <-
  iconv( x = plotsumms$Exposure.Location
         , from = "UTF-8"
         , to = "UTF-8"
         , sub = "" )


labs <- paste(plotsumms$Exposure.Location, plotsumms$Date,plotsumms$Arrival.Time, plotsumms$Departure.Time, sep="<br/>") 
# %>%
#     group_by(locName) %>%
#       summarise(countPlace = count(Place))
# # %>%
#   group_by(Suburb) %>%
#     summarise(FirstCase = min(conDate),
#               LastCase = max(conDate),
#               caseCount = sum(unique(Place)))

write.csv(x = plotsumms, "data/outSubs.csv")
