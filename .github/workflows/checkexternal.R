library(rvest)
library(RSelenium)
library(tidyverse)
library(DescTools)
library(knitr)
library(arsenal)
library(rgdal)
library(dplyr)


#####function 1 #####################
fixgeo <- function(search,  lat, lon, column="Exposure.Location",tt=tab3) {
  
  ii <- NA
  ii <- grep(search,tt[,which(column==colnames(tab3))])
  if(length(ii>0)) {
  for (c in 1:length(ii)){
    tt[ii[c],"lat"] <-lat
    tt[ii[c],"lon"] <- lon
  }
  }
  return(tt)
}

#grab from website
es <- read_html("https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations", )

#check update
ll <- es %>%
  html_nodes("strong") %>%
  html_text()
index <- grep("Page last updated:",ll)
dummy <- ll[index]
lup <- dummy
lu <- substr(strsplit(dummy,"updated:")[[1]][2],2,100)
lu <- gsub(" ", "_",lu)
lu <- gsub(":","",lu)
lu

#check if there was an update....
ff <- list.files("data/")
wu <- grep(lu, ff)

##### scrape covid exposure table from website
#can this be done from saved html file?
#or csv>



#rD <- rsDriver(browser="firefox", port=4545L, verbose=FALSE)
#remDr <- rD[["client"]]
#remDr$navigate("https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations")

#Sys.sleep(5) # give the page time to fully load
#html <- remDr$getPageSource()[[1]]

#remDr$close()
#rD$server$stop()
#rm(rD)
#gc()
#necessary to stop the server...
#system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
#signals <- read_html(html)


#tbls <- signals %>%
#  html_nodes("table") %>%
#  html_table(fill = TRUE)

#tab3 <- data.frame(tbls)

###save script here for reassessing
#write.rds