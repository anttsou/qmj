getcompanies <- function(){
  suffixes <- c("num", letters)
  urlPrefix <- "http://www.investorguide.com/stock/stocklist"
  end <- ".html"
  for(i in 1:27){
    url <- paste(urlPrefix, suffixes[i], end, sep='')
    dest <- paste(getwd(), "/", suffixes[i], ".text", sep='')
    download.file(url, destfile=dest)
  }
}

dataA <- read.table(a.text)
