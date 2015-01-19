#' getcompanies
#'
#' Collects names and tickers into a dataframe of 20 chosen companies.
#' In the future, will more selectively determine companies.
#' @export

getcompanies <- function() {
  i <- 1
  newLetters <- c("num",letters)
  stringfront <- "http://www.investorguide.com/stock/stocklist"
  stringback <- ".html"
  urlFront <- "http://www.sec.gov/cgi-bin/cik.pl.c?company="
  names <- NULL
  tickers <- NULL
  ciks <- NULL
  while(i <= length(newLetters)) {
    newUrl <- paste(stringfront,paste(newLetters[i],stringback,sep=""),sep="")
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
        newString2 <- sub(" ", "%20", gsub("[[:punct:]]","",newString))
        newUrl2 <- paste(urlFront,newString2,sep="")
        con2 <- url(newUrl2)
        htmlCode2 <- readLines(con2)
        close(con2)
        if(sub("</strong>.*","", sub(".*<strong>","",htmlCode2[9])) != "0") {
          ciks <- c(ciks,sub("</a>.*","",sub(".*\">","", htmlCode2[10])))
        } else {
          ciks <- c(ciks,"N/A")
        }
      }
      n = n+16
    }
    i = i+1
  }
  smallNames <- NULL
  smallTickers <- NULL
  smallCiks <- NULL
  indices <- c(8482,1586,6338,9087,1297,8674,6081,7800,8079,4854,3381,2469,5203,2841,7576,769,9199,2990,9122,3100)
  for(i in indices) {
    smallNames <- c(smallNames,names[i])
    smallTickers <- c(smallTickers,tickers[i])
    smallCiks <- c(smallCiks,ciks[i])
  }
  write.csv(data.frame(names = smallNames, tickers = smallTickers, ciks = smallCiks),"testcsv2.csv")
}
