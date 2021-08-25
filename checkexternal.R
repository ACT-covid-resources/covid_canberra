library(rvest)
library(RSelenium)
library(tidyverse)
library(DescTools)
library(knitr)
library(arsenal)
library(rgdal)
library(dplyr)

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

# if(length(wu)==0)
if(length(wu)==0){
  print("Check on github")
} #loop end

#, "No stress. Nothing new on the ACT Health site")
if(length(wu)>0) {
  
  l1 <- paste("No new update available. Current data is from:", lu,"\n")
} #loop end
