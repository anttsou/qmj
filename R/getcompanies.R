#' getcompanies
#'
#' Collects names and tickers into a dataframe of companies starting with the letter 'A'.
#' @examples
#' getcompanies()
#' @export

getcompanies <- function() {
  i <- 2
  newLetters <- c("num",letters)
  stringfront <- "http://www.investorguide.com/stock/stocklist"
  stringback <- ".html"
  names <- NULL
  tickers <- NULL
  while(i < 3) {
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
      }
      n = n+16
    }
    i = i+1
  }
  data.frame(names = names, tickers = tickers)
}