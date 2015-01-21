#' getcompanies
#' 
#' Collects names and tickers into a dataframe of companies starting with the letter 'A'.
#' @examples
#' \donttest{
#' getcompanies()
#' }
#' @export

getcompanies <- function() {
  i <- 2
  newLetters <- c("num",letters)
  stringfront <- "http://www.investorguide.com/stock/stocklist"
  stringback <- ".html"
  urlFront <- "http://www.sec.gov/cgi-bin/cik.pl.c?company="
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
        #         newString2 <- sub(" ", "%20", gsub("[[:punct:]]","",newString))
        #         newUrl2 <- paste(urlFront,newString2,sep="")
        #         con2 <- url(newUrl2)
        #         htmlCode2 <- readLines(con2)
        #         close(con2)
        #         if(sub("</strong>.*","", sub(".*<strong>","",htmlCode2[9])) != "0") {
        #           ciks <- c(ciks,sub("</a>.*","",sub(".*\">","", htmlCode2[10])))
        #         } else {
        #           ciks <- c(ciks,"N/A")
        #         }
      }
      n = n+16
    }
    i = i+1
  }
  companies <- data.frame(names = names, tickers = tickers)
  #saves companies to data folder.
  filepath <- system.file("data", package="qmj")
  filepath <- paste(filepath, "/companies.RData", sep='')
  #save(companies, file=filepath)
  companies
}