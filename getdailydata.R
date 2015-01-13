getdailydata <- function(x){
  ##x is a dataframe containing a list of companies.
  ##Requires quantmod package
  ##getdailydata is meant to be used only when desiring to retrieve
  ### data from the web.
  if(!require("quantmod")){
   stop("quantmod package missing for function getdailydata() in package qmj")
  } else{
    numCompanies <- length(x$tickers)
    for(i in 1:numCompanies){
      companyTicker <- x$ticker[i]
      stockData <- quantmod::getSymbols(companyTicker, src="google", auto.assign=FALSE)
      fileName <- paste("/data/", companyTicker, ".csv", sep='')
      write.zoo(stockData, file = fileName, sep=",")
    }
  }
}