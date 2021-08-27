# https://raw.githubusercontent.com/ACT-covid-resources/covid_canberra/main/Covid_Exposure_ACT.rmd

library(rsconnect)
rsconnect::accountInfo(name = 'the-statistics-network-ssn')
rsconnect::deployApp("index.Rmd")

rsconnect::configureApp("index.Rmd")
