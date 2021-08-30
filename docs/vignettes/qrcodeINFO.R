## creating qr code
##new and pop
#ARD aug 2021

library(qrcode)

png("qrplot.png")
qrcode_gen("https://act-covid-resources.github.io/covid_canberra/")
dev.off()

