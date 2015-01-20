#' getdailydata
#'
#' Given a list of companies (names and tickers), writes .csv files for every company
#' storing price returns.
#' @param x A dataframe of company names and tickers.
#' @export

getdailydata <- function(x){
  ##x is a dataframe containing a list of companies.
  ##Requires quantmod package
  ##getdailydata is meant to be used only when desiring to retrieve
  ### data from the web.
  filepath <- system.file("data", package="qmj")
  numCompanies <- length(x$tickers)
  thisYear <- as.numeric(format(Sys.Date(), "%Y"))
  desiredDates <- paste(thisYear - 5, "/", sep='')
  
  #Block of code below specially gathers the daily data for the S&P 500 for use as a benchmark.
  stockData <- quantmod::getSymbols("^GSPC", src="yahoo", auto.assign=FALSE)
  stockData <- stockData[desiredDates,4]
  #stockData <- round(TTR::ROC(quantmod::Cl(stockData)), digits=5)
  stockData[,1] <- pricereturns(stockData)
  fileName <- paste(filepath, "/", "GSPC.RData", sep='')
  save(stockData, file=fileName)
  for(i in 1:numCompanies){
    companyTicker <- as.character(x$ticker[i])
    stockData <- tryCatch(
      quantmod::getSymbols(companyTicker, src="google", auto.assign=FALSE),
      error=function(e) e
    )
    if(!inherits(stockData, "error")){
      stockData <- stockData[desiredDates,4]
      #Calculates price returns. Not total returns.
      #stockData <- round(TTR::ROC(quantmod::Cl(stockData)), digits=5)
      stockData[,1] <- pricereturns(stockData)
      fileName <- paste(filepath, "/", companyTicker, ".RData", sep='')
      save(stockData, file=fileName)
    } else{
      print(paste("Error retrieving data for ", companyTicker, sep=""))
      fileName <- paste(filepath, "/", companyTicker, ".RData", sep='')
      nullData <- rep(NA, 1)
      save(nullData, file=fileName)
    }
  }
  fileList <- list.files(path=filepath, pattern="\\.RData$", full.names=TRUE)
  compiled <- matrix()
  load(fileList[1])
  compiled = cbind(compiled, stockData)
  for(i in 2:length(fileList)){
    load(fileList[i])
    compiled = cbind(compiled, stockData)
  }
  data.frame(compiled)
}