library(rvest)
url <- "https://www.r-project.org"

imgsrc <- read_html(url) %>%
  html_node(xpath = '//*/img') %>%
  html_attr('src')

imgsrc
# [1] "/Rlogo.png"

# side-effect!
download.file(paste0(url, imgsrc), 
              destfile = basename(imgsrc))
