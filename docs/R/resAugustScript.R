# Exposure sites per Update for august
allfiles <- list.files("./data/allfiles/September/", pattern=c("table_", ".csv"), full.names = TRUE)

countsperupdate <- function(allfiles){
  
  allfiledat <- list() #<- list() MUST BE OUTSIDE
  
  for(i in 1:length(allfiles)) {
    tib <- read_csv(allfiles[i]) %>%
      dplyr::select(Contact, Date, Suburb, Status) %>%
      filter(Status == "New")
    
    allfiledat[[i]] <- as.data.frame(tib)
  } #end loop
  
  return(allfiledat)
  
} #end function

##setup list of data
allfiledat <- list()

#run function
allfiledat <- countsperupdate(allfiles = allfiles)

# bind_rows(allfiledat, allfiledat1)

res <- data.frame(update=NA, date=NA,locations=NA, contact=NA,nsites=NA )
cc<-1         

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


res <- res[!res$contact=="Investigation information",]
res <- res[!res$contact=="Investigation location",]
res$contact <- factor(res$contact, levels=c("Close", "Casual","Monitor"))


res <- res[order(res$contact),]
cols <- c( "red", "yellow","blue")[as.numeric(res$contact)]

# Save csv
write.csv(res, './data/resSeptember.csv')

res <- read.csv('./data/resSeptember.csv')
