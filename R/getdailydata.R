getdailydata <- function(x){
  ##x is a dataframe containing a list of companies.
  ##Requires quantmod package
  ##getdailydata is meant to be used only when desiring to retrieve
  ### data from the web.
  numCompanies <- length(x$tickers)
  for(i in 1:numCompanies){
    companyTicker <- as.character(x$ticker[i])
    stockData <- tryCatch(
      quantmod::getSymbols(companyTicker, src="google", auto.assign=FALSE),
      error=function(e) e
    )
    if(!inherits(stockData, "error")){
      thisYear <- as.numeric(format(Sys.Date(), "%Y"))
      desiredDates <- paste(thisYear - 5, "/", sep='')
      stockData <- stockData[desiredDates,4]
      #Calculates price returns. Not total returns.
      stockData <- round(TTR::ROC(quantmod::Cl(stockData)), digits=5)
      fileName <- paste("data/", companyTicker, ".csv", sep='')
      write.zoo(stockData, file = fileName, sep=",") 
    } else{
      print(paste("Error retrieving data for ", companyTicker, sep=""))
    }
  }
  #Block of code below specially gathers the daily data for the S&P 500 for use as a benchmark.
  stockData <- quantmod::getSymbols("^GSPC", src="yahoo", auto.assign=FALSE)
  thisYear <- as.numeric(format(Sys.Date(), "%Y"))
  desiredDates <- paste(thisYear - 5, "/", sep='')
  stockData <- stockData[desiredDates,4]
  stockData <- round(TTR::ROC(quantmod::Cl(stockData)), digits=5)
  write.zoo(stockData, file = "data/GSPC.csv", sep=",") 
}