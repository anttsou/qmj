getdailydata <- function(x, n=5000){
  ##x is a dataframe containing a list of companies.
  ##Requires quantmod package
  ##getdailydata is meant to be used only when desiring to retrieve
  ### data from the web.
  numCompanies <- length(x$tickers)
  if(n > numCompanies){
    errorMessage <- paste("Error, at most only ", n, " companies may be chosen.", sep='')
    stop(errorMessage)
  }else{
    randomNums <- sample(1:numCompanies, n, replace=FALSE) 
  }
  for(i in 1:n){
    companyTicker <- as.character(x$ticker[randomNums[i]])
    stockData <- tryCatch(
      quantmod::getSymbols(companyTicker, src="google", auto.assign=FALSE),
      error=function(e) e
    )
    if(!inherits(stockData, "error")){
      stockData <- stockData[,4]
      fileName <- paste("data/", companyTicker, ".csv", sep='')
      write.zoo(stockData, file = fileName, sep=",") 
    } else{
      print(paste("Error retrieving data for ", companyTicker, sep=""))
    }
  }
}