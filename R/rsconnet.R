library(rsconnect)

rsconnect::setAccountInfo(name='the-statistics-network-ssn', token='4D67F6D809C6F98D58B304442413B41E', secret='ZaShE+MyyKQTCYOUk6BBwVfwyS0pQqhKB0kNHsvL')

library(rsconnect)
setAccountInfo(name='the-statistics-network-ssn', token='4D67F6D809C6F98D58B304442413B41E', secret='ZaShE+MyyKQTCYOUk6BBwVfwyS0pQqhKB0kNHsvL')
connectUser()

removeAuthorizedUser(user = 266942, account = 'the-statistics-network-ssn')
accountInfo(name = "the-statistics-network-ssn")
accountUsage()
showUsers()

appDir = "C:/Code/covid-canberra/"

rsconnect::terminateApp(ap)
applications()

library(shiny)
deployApp(appDir = here::here("flashdash.Rmd"))
removeAuthorizedUser(user = "anthonydavidson101@gmail.com")

  