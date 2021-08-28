## new exposure cases
##Anthony Davidson
##29oct2021

library(stringr)
#select each update
alldat <- list.files("data", ".csv")
# jsut tables'
# dattables <- 
table(stringr::str_detect(alldat, "table_"))
