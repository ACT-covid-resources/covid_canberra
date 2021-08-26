---
title: "unoffical-ACT covid19 resources"
date: "`r Sys.Date()`"
author:  "[Anthony Davidson](https://github.com/davan690)"
output:
  html_document:
    output_dir: "docs"
    css: index-style.css
---

```{r setup, include=FALSE, message=FALSE, warning=FALSE}
library(knitr)
library(rmdformats)

## Global options
# options(max.print="75")
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	cache = FALSE,
	cache.path = "docs/",
	# width=75,
	comment = NA,
	prompt = FALSE,
	tidy = TRUE
)
```

This website tracks COVID-19 data and mobility information for the ACT and Canberra using code and data developed in the open source community. This information is only a guide. Please refer to offical reports and press releases from goverment certified resources.

----

# Lastest information {.tabset .tabpills}

Currently a holding place for updated reporting. Currently under development.
<!-- This data is collected using code that accesses twitter, act govt site, nsw govt site, vic govt site...? -->


```{r}
#webpageurl
#scrap for warnings
#save with date

```



```{r eval = FALSE, include=TRUE}
library(tidyverse)
library(ggmap)
library(leaflet)
library(plotly)
library(flexdashboard)
library(DT)
#latest dataset
tab3 <- read_csv("https://raw.githubusercontent.com/ACT-covid-resources/covid_canberra/main/data/last.csv")
# str(tab3)
# Aggregate method

cols <- c( "yellow", "red","cyan", "blue")

cc <- as.numeric(factor(tab3$Contact))

labs <- paste(tab3$Contact, tab3$Status,tab3$Exposure.Location, tab3$Street, tab3$Suburb, tab3$Date,tab3$Arrival.Time, tab3$Departure.Time, tab3$doubles, sep="<br/>") 

###############################################
##plot the map


m <- leaflet() %>% addTiles()

m <- m %>% addCircleMarkers(lat=tab3$lat, lng=tab3$lon,popup = labs, weight=0.5, color = cols[cc], radius = 5 , fillOpacity = 0.8) 
m <- m %>% addLegend("bottomright", labels = levels(factor(tab3$Contact)), colors = cols, opacity = 1)
```

<!-- # ```{r title, results='asis'}  -->
<!-- #  -->
<!-- # cat(paste0("# ", params$lup)) -->
<!-- #  -->
<!-- # ``` -->

<sup>Disclaimer: This work This map shows the covid exposure locations in the ACT and is an **unofficial website** based on [official sources](https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations). So if in doubt, refer to the [offical website](https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations), which has now also an [**official map**](https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations/map#Map-of-ACT-exposure-locations) included. Supported by Volunteers from the University of Canberra. Contacts: Bernd Gruber [bernd.gruber@canberra.edu.au] & Anthony Davidson [anthony.davidson@canberra.edu.au]</sup>

Row {data-width=350}
-----------------------------------------------------------------------

```{r include  = FALSE}
library(lubridate)
library(tidyverse)
library(plotly)
tab3 <- read_csv("https://raw.githubusercontent.com/ACT-covid-resources/covid_canberra/main/data/last.csv")
str(tab3)
#latest dataset
# tab3 <- ltab

# ## restructure data to look nice
# # tab3 %>%
# #   rename(`Place name` = Place)
# # Aggregate method
# labs <- paste(tab3$Place, tab3$Date,tab3$Arrival.Time, tab3$Departure.Time, sep="<br/>") 
# # names(tab3)
# datyl <-factor(tab3$Contact)
# # levels(datyl)
# 
# datyl1 <- tab3 %>%
#            filter(Status >= "New")

# names(tab3)
# colsN <- cols[datyl1]

tab4 <- tab3 %>%
  mutate(colsN = factor(Contact, levels = c("Close", "Casual", "Monitor","Investigation location")),
         Contact = factor(Contact, levels = c("Close", "Casual","Monitor", "Investigation location")))


levels(tab4$colsN) <- c("purple", "red","orange",  "grey50")
levels(tab4$colsN) <- c("red", "yellow","blue",  "teal")
# table(tab4$colsN)

# names(tab4)
tab4 %>%
  mutate(conDate = as.Date(lubridate::dmy(Date)),
         locName = as.factor(Exposure.Location))


##loc summaries
tab5 <- tab4 %>%
  mutate(conDate = as.Date(lubridate::dmy(Date)),
         locName = factor(Suburb)) %>%
  mutate(datNum = lubridate::day(conDate),
         weekname = lubridate::week(conDate))

glimpse(tab5)

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

filter(tab5, Suburb == "Amaroo")$Suburb

str(tab5$Suburb)


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

print(a)
str(a)

# Aggregate method
# labs <- paste(plotsumms$Exposure.Location, plotsumms$Date,plotsumms$Arrival.Time, plotsumms$Departure.Time, sep="<br/>") 

# nrow(tab4)
#> [1] 100
# nrow(distinct(plotsumms, Suburb))
b <- distinct(plotsumms, Suburb, .keep_all = TRUE)
# subsTable <- semi_join(tab4, b)


#> [1] 69
# nrow(distinct(df, x, y))
# #> [1] 69
# levels(plotsumms$locName)
# distinct(df, x)
plotsumms <- b
plotsumms$Suburb[35] <- "O'Connor" 
plotsumms$locName[35] <- "O'Connor"
# plotsumms$Suburb <- droplevels(plotsumms$Suburb)
plotsumms$locName <- droplevels(plotsumms$locName)

clean <- plotsumms$Exposure.Location[4] <- "Assembly The People Pub"

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

# write.csv(x = plotsumms, "data/outSubs.csv")
```

### Map of Exposure sites

```{r}
library(plotly)
library(ggplotlyExtra)
library(leaflet)

leaflet(plotsumms) %>% addTiles() %>%
  addCircleMarkers(lat=plotsumms$lat,
                            lng=plotsumms$lon,
                   weight = 0.2, 
    radius = log(plotsumms$contactcount)*5,
                            popup = paste0(" COUNT:", plotsumms$contactcount),
                            fillOpacity = 0.3,
    color = "grey50"
                            ) %>%
  addCircles(lat=tab4$lat,lng=tab4$lon,
             popup = paste0(plotsumms$Exposure.Location," ", plotsumms$Date), 
                            color = plotsumms$colsN,
                            stroke = TRUE,
                            fill = rep("black", length(plotsumms$colsN)))

```

<!-- Row {data-width=150} -->
<!-- ------------------------------------ -->

### New cases: `r Sys.Date()`

```{r}
# names(plotsumms)
# glimpse(plotsumms)
# ggplot(plotsumms)
# ggplot(sum1) + 
#   geom_point(aes(x = time, y = as.numeric(casecount), group = Contact, col = Status), size = 4) + 
#   theme_bw()

p1 <- ggplot(filter(sum1, Status == "New")) + 
  geom_point(aes(x = time, y = Suburb, shape = Contact), size = 3) + 
  # geom_point(data = filter(sum1, Suburb == "Fyshwick"), aes(x = time, y = Suburb, shape = Contact), size = 1) + 
  facet_wrap(~Contact, scales = "free", ncol = 1) +
  theme_bw()

library(ggplotlyExtra)

ggplotly(p1)


##by location address
sum2 <- tab5 %>%
  dplyr::group_by(Exposure.Location, conDate) %>%
    dplyr::summarise(casecount = length(unique(Date)))

##total more than 3 cases
# sum2 %>%
#       filter(casecount >= )

# table(sum2$Status, sum2$Contact)
library(DT)
library(kableExtra)


```


```{r include = TRUE}
plotsumms %>%
  filter(Status == "New") %>%
  select("Exposure.Location",	"Street",	"Suburb",	"Date",	"Arrival.Time",	"Departure.Time",	"Contact") %>%
  kbl() %>%
  kable_paper("hover", full_width = F)
# # Use it with sparkline
# Well, this is not a feature but rather a documentation of how to use the sparkline package together with this package. The easiest way is sort of a hack. You can call  sparkline::sparkline(0) somewhere on your document where no one would mind so its dependencies could be loaded without any hurdles. Then you use  sparkline::spk_chr() to generate the text. For a working example, see: Chinese names in US babynames
# 
# # Not evaluated
# library(sparkline)
# sparkline(0)
#  spk_dt <- data.frame(
#   var = c("mpg", "wt"),
#   sparkline = c(spk_chr(mtcars$mpg), spk_chr(mtcars$wt))
# )
# 
# kbl(spk_dt, escape = F) %>%
#   kable_paper(full_width = F)
# vignette(package = "kableExtra", topic = "awesome_table_in_html")
# dtt <-DT::datatable(plotsumms[,c(1,8,2:7)],
#   rownames = FALSE, options = list(pageLength = 50, ausoHideNavigation=TRUE, 
#   initComplete = JS(
#         "function(settings, json) {",
#         "$(this.api().table().header()).css({'font-size': '50%'});",
# 
#         "}")))  %>% DT::formatStyle(columns = 1:8, fontSize='70%') 
# 
# dtt

```

Column {data-width=350}
-----------------------------------------------------------------------

### Timeline

Break down information into the following figure of contact times and locations:

```{r}
fullDatdf <- as.data.frame(tab5)
# names(fullDatdf)


p2 <- ggplot(fullDatdf) + 
  geom_point(aes(x = Date, y = Suburb, col = Contact), size = 2) + 
  geom_vline(xintercept = 7, size = 1, col = "red", lty = 1, alpha= 0.5) +
  geom_line(aes(x = Date, y = Suburb)) + 
  # geom_point(data = filter(sum1, Suburb == "Fyshwick"), aes(x = time, y = Suburb, shape = Contact), size = 1) + 
  # facet_wrap(~Contact, scales = "free", ncol = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ggplotly(p2)
```






```{r include = FALSE}
library(plotly)

df <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/GanttChart-updated.csv", 
               stringsAsFactors = F)

df$Start  <- as.Date(df$Start, format = "%m/%d/%Y")
client    <- "Sample Client"
cols      <- RColorBrewer::brewer.pal(length(unique(df$Resource)), name = "Set3")
df$color  <- factor(df$Resource, labels = cols)

p <- plot_ly()
for(i in 1:(nrow(df) - 1)){
  p <- add_trace(p,
                 x = c(df$Start[i], df$Start[i] + df$Duration[i]), 
                 y = c(i, i), 
                 mode = "lines",
                 line = list(color = df$color[i], width = 20),
                 showlegend = F,
                 hoverinfo = "text",
                 text = paste("Task: ", df$Task[i], "<br>",
                              "Duration: ", df$Duration[i], "days<br>",
                              "Resource: ", df$Resource[i]),
                 evaluate = T
  )
}

p
```

<!-- [coming] -->

## Recent contact locations

- Government page: [ACT.govt.au](https://www.covid19.act.gov.au/)<br>
- Data: [ACT govt data](https://app.powerbi.com/view?r=eyJrIjoiZTY4NTI1NzQtYTBhYy00ZTY4LTk3NmQtYjBjNzdiOGMzZjM3IiwidCI6ImI0NmMxOTA4LTAzMzQtNDIzNi1iOTc4LTU4NWVlODhlNDE5OSJ9)<br>

```{r}
#imbed storymap
knitr::include_url("https://storymaps.arcgis.com/stories/7886a3c0b4b9413f82019b60e21d02db")

# knitr::include_graphics("https://www.covid19.act.gov.au/__data/assets/image/0005/1820273/PICC0347-Exposure-locations_ACT_Aug-2021.png")
# https://ourworldindata.org/grapher/covid-confirmed-cases-since-100th-case?country=NZL+KOR+DNK
```

## Current ACT covid data

As of `5pm of Thursday 12th August 2021` the ACT went into a 7-day lockdown. This is due to a case in Canberra being active in the community for some days. Here is the current location data released on Thursday evening...

UPDATE coming....

```{r}
# ACT raw map
knitr::include_url("https://app.powerbi.com/view?r=eyJrIjoiZTY4NTI1NzQtYTBhYy00ZTY4LTk3NmQtYjBjNzdiOGMzZjM3IiwidCI6ImI0NmMxOTA4LTAzMzQtNDIzNi1iOTc4LTU4NWVlODhlNDE5OSJ9")

##html to data here


#save baseline

##check dayly


#interactive map in the future
```

## ACT vaccine rollout

BRAND NEW and amazing app :) all abc top notch journalism (reproducible and transparent)

```{r}
knitr::include_url("https://markusmannheim.github.io/act-coronavirus-teaser/")
```

## Join mail list

<iframe src="https://docs.google.com/forms/d/e/1FAIpQLSdw-QWPpUsD6eH59kFJC5zJCXD6bExu7lmLboKdixR6iXJhAg/viewform?embedded=true" width="640" height="931" frameborder="0" marginheight="0" marginwidth="0">Loading…</iframe>
