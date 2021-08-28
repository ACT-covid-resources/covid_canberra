#https://raw.githubusercontent.com/amrrs/scrape-automation/main/nifty50_scraping.R

#load libs

library(tidyverse)
library(rvest)
library(janitor)

#nse top gainers

url <- 'https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations'

# extract html 

url_html <- read_html(url)

#table extraction
#check update
ll <- url_html %>%
  html_nodes("strong") %>%
  html_text()

index <- grep("Page last updated:",ll)
dummy <- ll[index]
lup <- dummy
lu <- substr(strsplit(dummy,"updated:")[[1]][2],2,100)
lu <- gsub(" ", "_",lu)
lu <- gsub(":","",lu)
lu
#"27_Aug_2021_723pm"
# lu <- "table_23_Aug_2021_919am.csv"

#check if there was an update....
ff <- list.files("data/", pattern = ".csv")

wu <- grep(lu, ff)

if(wu == 0){

 site_checks_ACT <- read.csv("data/datruns.csv")

 
 ##add run
 #folder path needs to change          
 write_csv(site_checks_ACT,paste0('data/',Sys.Date(),'_top_gainers','.csv'))    
 
 
}

if(wu >= 0){
  
  site_checks_ACT <- read.csv("data/datruns.csv")
  varsneeded <- colnames(site_checks_ACT)

  colnames(varsneeded[1], varsneeded[2], varsneeded[3]))
  ##add run
  #folder path needs to change          
  write_csv(site_checks_ACT,paste0('data/',Sys.Date(),'_top_gainers','.csv'))    
  
  
}





       
