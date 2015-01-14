getcompanies <- function() {
  i <- 1
  newLetters <- c("num",letters)
  stringfront <- "http://www.investorguide.com/stock/stocklist"
  stringback <- ".html"
  names <- NULL
  tickers <- NULL
  while(i <= length(newLetters)) {
    newUrl <- paste(stringfront,paste(letters[i],stringback,sep=""),sep="")
    con <- url(newUrl)
    htmlCode <- readLines(con)
    close(con)
    n <- 238
    while(htmlCode[n] != "<div class=\"stock-char-links\">") {
      if(htmlCode[n] != "N/A</div>") {
        newString <- sub('</div>','',htmlCode[n])
        newTicker <- gsub("[^A-Za-z]","",sub("\">.*","",sub(".*ticker=","",htmlCode[n+3])))
        names <- c(names,newString)
        tickers <- c(tickers,newTicker)
      }
      n = n+16
    }
    i = i+1
  }
  smallNames <- NULL
  smallTickers <- NULL
  indices <- c(8482,1586,6338,9087,1297,8674,6081,7800,8079,4854,3381,2469,5203,2841,7576,769,9199,2990,9122,3100)
  for(i in indices) {
    smallNames <- c(smallNames,names[i])
    smallTickers <- c(smallTickers,tickers[i])
  }
  data.frame(names = smallNames, tickers = smallTickers)
}