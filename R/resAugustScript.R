# Exposure sites per Update for august
allfiles <- list.files("./data/allfiles/August/", pattern=c("table_", ".csv"), full.names = TRUE)

##########################################function 1 #######################################
################miniSpice function
#turns a list of csvs into a structured list in R
#because our datasets are similar 
#we can then run functions 
#across all aspects of each element of the list
miniSpice <- function(allfiles){
  
  allfiledat <- list() #<- list() MUST BE OUTSIDE
  
  for(i in 1:length(allfiles)) {
    tib <- read_csv(allfiles[i]) %>%
      dplyr::select(Contact, Date, Suburb, Status) %>%
      filter(Status == "New")
    #or unique?
    
    allfiledat[[i]] <- as.data.frame(tib)
  } #end loop
  
  return(allfiledat)
  
} #end function

##setup list of data
allfiledat <- list()

#run function
allfiledat <- miniSpice(allfiles = allfiles)

# bind_rows(allfiledat, allfiledat1)

##creates summary output from all data used in function above
res <- data.frame(update=NA, date=NA,locations=NA, contact=NA,nsites=NA )
cc<-1         

##loop links the above function output with the desired summary csv
for(i in 1:length(allfiles)){
  
  dd <- allfiledat[[i]]         
  tt <- table(dd$Contact)
  
  if (length(tt)>0) {
    for (ii in 1:length(tt))
    {
      res[cc, 1] <- i
      res[cc,2] <- (substr(dd$Date[i], 1,10))
      res[cc,3] <- nrow(dd)
      res[cc,4] <- names(tt)[ii]
      res[cc,5] <- tt[ii]
      cc <- cc+1
    }
  }
}

##clean up summary csv for plotting easily
res <- res[!res$contact=="Investigation information",]
res <- res[!res$contact=="Investigation location",]
res$contact <- factor(res$contact, levels=c("Close", "Casual","Monitor"))
res <- res[order(res$contact),]

##figure colours
cols <- c( "red", "yellow","blue")[as.numeric(res$contact)]

# Save csv
write.csv(res, paste0('./data/', Sys.Date(),'lastupdate_resAugust.csv'))
res <- read.csv(paste0('./data/', Sys.Date(),'lastupdate_resAugust.csv'))
