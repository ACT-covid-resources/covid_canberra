#plotly covid summary plot

library(lubridate)
library(tidyverse)

#######################import data for bernds github file ################
dat <- read_csv("https://raw.githubusercontent.com/green-striped-gecko/covid_canberra/main/data/last.csv")
# stringsAsFactors = F
###Oconner issues
table(str_detect(dat$Suburb, "nnor"))
#original from summaried code
# plotsumms$Suburb[35] <- "O'Connor" 
# plotsumms$locName[35] <- "O'Connor"
##another issue
# clean <- plotsumms$Exposure.Location[4] <- "Assembly The People Pub"


#####################Create colour varible ##################
#must be done later as levels change

######################## variable encoding issues  #############
# replace all non UTF-8 character strings with an empty space
# pre-processing
# ensure that all characters in the `Name` column
# are valid UTF-8 encoded
# mostly with key words:
#O'Conner =rename as= OConner
# https://stackoverflow.com/questions/17291287/how-to-identify-delete-non-utf-8-characters-in-r

###function
removeCharacterIssues_function <- function(varWITHissues){
  Encoding(x = varWITHissues) <- "UTF-8"
  fixedVar <- iconv(x = varWITHissues,
                                       from = "UTF-8",
                                       to = "UTF-8",
                                       sub = ""
  )
  return(fixedVar)
  
} #function end

dat1$Exposure.Location
# dat1[68,]$Exposure.Location
## Run function example

#Exposure.Location
varWITHissues <- dat1$`Exposure Location`
a <- removeCharacterIssues_function(varWITHissues = varWITHissues)
table(a == dat1$`Exposure.Location`)
dat1$`Exposure Location` <- a
#Suburb
varWITHissues <- dat1$Suburb
a <- removeCharacterIssues_function(varWITHissues = varWITHissues)
table(a == dat1$Suburb)
dat1$Suburb <- a
### with a list of variables??
# using lapply

######################correct factoring now for variables

#restructure variables
dat1 <- dat %>%
  mutate(colsN = factor(Contact, levels = c("Close", "Casual", "Monitor","Investigation information")),
         `Contact level` = factor(Contact, levels = c("Close", "Casual","Monitor", "Investigation information")),
         `Arrival time` = Arrival.Time,
         `Departure time` = Departure.Time,
         # `Duration` = Departure.Time - Arrival.Time, 
         `Exposure date` = as.Date(lubridate::dmy(Date)),
         `Exposure Location` = factor(Exposure.Location),
         Suburb = as.character(Suburb),
         fulls = paste0(Date, " ", Arrival.Time),
         fulle = paste0(Date, " ", Departure.Time),
         dateF = as.Date(dmy(Date))) #, "%d-%m-%y"))

dat1$Contact
# dat1$dateF
#####making start end variables for all same#####################
dat1$startF = parse_date_time(dat1$fulls, '"%d-%m-%y %I:%M %p')
#,
dat1$endF = parse_date_time(dat1$fulle, '"%d-%m-%y %I:%M %p') 
dat1$dayNameStart <- weekdays(dat1$startF)
levels(dat1$dayNameStart) = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

dat1$dayNameEnd <- weekdays(dat1$endF)
levels(dat1$dayNameEnd) = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

###week to split by
dat1$weekNumber <- week(dat1$endF)

#
#######nice plot now
##stay length
##allows for contact duration to be calculated
dat2 <- dat1 %>%
  mutate(ContactTime = endF - startF)


plot1 <- ggplot(dat2) + 
  geom_histogram(aes(x = ContactTime, group = Contact, fill = Contact), bins = 20) + 
  facet_wrap(~weekNumber)

dat3 <-   dat2 %>%
  mutate(StatusN = as.numeric(ifelse(Status == "New", 1, 0)),
         dayName = factor(dayNameStart, levels =  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) #%>%

dat3$StatusN <- replace_na(dat3$StatusN, 0)










#################actuall summary stuff
# new entries
table(dat3$dayName, dat3$weekNumber)

# StatusN
timeInSub <- dat3 %>%
  group_by(Contact, weekNumber,Suburb) %>%
    summarise(totalContactT = sum(ContactTime)
          # , StatusN  #NOT WORKING,
          ) %>%
  filter(totalContactT >= 60)
  # ungroup()

glimpse(timeInSub)

ggplot(timeInSub) + 
  geom_point(aes(x = weekNumber, y = totalContactT, colour = Contact, group  = Suburb))+ 
geom_line(aes(x = weekNumber, y = totalContactT, colour = paste0(Contact,Suburb))) +
  theme(legend.position = "none")

just33high <- filter(timeInSub, weekNumber == 33 | totalContactT >= 60)

ggplot(just33high) + 
  geom_point(aes(x = weekNumber, y = totalContactT, colour = Contact, group  = Suburb))+ 
  geom_line(aes(x = weekNumber, y = totalContactT, colour = paste0(Contact,Suburb))) +
  theme(legend.position = "none")


###sumarise by day

# dat3 %>%
  # group_by(dateF, Contact) %>%
  # summarise(totalContactT = sum(ContactT))








ggplot(dat1) +
  geom_point(aes(x = dayNameStart, y = Suburb)) + 
  geom_point(aes(x = dayNameEnd, y = Suburb), size = ) + 
  
  ##check overnights
  # table(dat1$dayNameEnd == dat1$dayNameStart) #no overnighters
  
  
###More data variations
# start = as.Date(Arrival.Time),
# end = Departure.Time)
# dat1$endF <- paste0(hour(datDicksonR$startF), ".", minute(datDicksonR$startF))
# datDicksonR$endTn <- paste0(hour(datDicksonR$endF), ".", minute(datDicksonR$endF))
# datDicksonR$dayS <- factor(weekdays(datDicksonR$startF))
# levels(datDicksonR$dayS) = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
# datDicksonR$dayE <- weekdays(datDicksonR$endF)
# levels(datDicksonR$dayE) = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

#################create summary tables

#by Suburb
dat1 %>%
  group_by(Suburb)
a <- as.data.frame(table(dat1$Exposure.Location))
colnames(a) <- c("Exposure.Location", "contactcount")

filter(a, contactcount >=5)


####create flexdash dataframe
plotsumms <- right_join(dat1, a, by = c("Exposure.Location"))

# Aggregate method
labs <- paste(plotsumms$Exposure.Location, plotsumms$Date,plotsumms$Arrival.Time, plotsumms$Departure.Time, sep="<br/>")

nrow(distinct(plotsumms, Suburb))

b <- distinct(plotsumms, Suburb, .keep_all = TRUE)

levels(plotsumms$Suburb)
plotsumms$Suburb <- droplevels(plotsumms$Suburb)
plotsumms$locName <- droplevels(plotsumms$locName)

############################# Labels for ploty
labs <- paste(plotsumms$Exposure.Location, plotsumms$Date,plotsumms$Arrival.Time, plotsumms$Departure.Time, sep="<br/>") 



#############rename variables#############
names(dat1)

# dat1 %>%
#   rename(`Exposure Location` = Exposure.Location,
#          )



##############plot
leaflet(plotsumms) %>% addTiles() %>%
  addCircleMarkers(lat=plotsumms$lat,
                   lng=plotsumms$lon,
                   weight = 0.2, 
                   radius = log(plotsumms$contactcount)*5, 
                   color = plotsumms$colsN,
                   stroke = TRUE,
                   fill = rep("black", length(plotsumms$colsN)),
                   popup = paste0(" COUNT:", plotsumms$contactcount),
                   fillOpacity = 0.8
  ) #%>%
  #addCircles(lat=tab4$lat,lng=tab4$lon,
  #           popup = paste0(plotsumms$Exposure.Location," ", plotsumms$Date))

# %>%
#     group_by(locName) %>%
#       summarise(countPlace = count(Place))
# # %>%
#   group_by(Suburb) %>%
#     summarise(FirstCase = min(conDate),
#               LastCase = max(conDate),
#               caseCount = sum(unique(Place)))

# write.csv(x = plotsumms, "data/outSubs.csv")



#########EXTRA

####By suburb snippits needed
# %>%
# filter(Suburb == "Dickson") 
#id = seq(1:length(Date)),
# content = Exposure.Location
# glimpse(datDicksonR)