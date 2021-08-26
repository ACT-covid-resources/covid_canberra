##############next step of code
##### scrape covid exposure table from website
#can this be done from saved html file?
#or csv>



rD <- rsDriver(browser="firefox", port=4545L, verbose=FALSE)
remDr <- rD[["client"]]
remDr$navigate("https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations")

Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

remDr$close()
rD$server$stop()
rm(rD)
gc()
# necessary to stop the server...
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
signals <- read_html(html)


tbls <- signals %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

tab3 <- data.frame(tbls)

###save script here for reassessing
#write.rds