#' Gets the daily prices and returns of companies for the past five years.
#'
#' Given a list of companies (names and tickers), writes .csv files for every company
#' storing price returns.
#' @param x A dataframe of company names and tickers.
#' @examples
#' x <- data(companies)
#' \dontrun{get_dailydata(x)}
#' @export

get_dailydata <- function(x){
  filepath <- system.file("extdata", package="qmj")
  numCompanies <- length(x$ticker)
  thisYear <- as.numeric(format(Sys.Date(), "%Y"))
  desiredDates <- paste(thisYear - 2, "/", sep='')
  listfiles <- rep("", (numCompanies + 1))
  #Block of code below specially gathers the daily data for the S&P 500 for use as a benchmark.
  stockData <- quantmod::getSymbols("^GSPC", src="yahoo", auto.assign=FALSE)
  stockData <- stockData[desiredDates,6]
  priceData <- stockData[,1]
  #Calculates price returns. Not total returns.
  stockData$pret <- pricereturns(stockData)
  stockData <- stockData[-1,]
  fileName <- paste(filepath, "/", "GSPC.RData", sep='')
  listfiles[1] <- fileName
  
  save(stockData, file=fileName)
  
  filesInDest <- list.files(path=filepath)
  for(i in 1:numCompanies){
    companyTicker <- as.character(x$ticker[i])
    print(companyTicker)
    fileName <- paste(filepath, "/", companyTicker, ".RData", sep='')
    listfiles[(i) + 1] <- fileName
    file <- paste(companyTicker, ".RData", sep='')
    if(is.element(file, filesInDest)){
      print(paste(companyTicker, "information found in extdata. Resuming Download.", sep=' '))
    } else{
      stockData <- tryCatch(
        quantmod::getSymbols(companyTicker, src="google", auto.assign=FALSE),
        error=function(e) e
      )
      if(!inherits(stockData, "error") && length(stockData[,1]) > 1 && length(stockData[desiredDates,4]) > 1){
        stockData <- stockData[desiredDates,4]
        priceData <- stockData
        stockData$pret <- pricereturns(stockData)
        stockData <- stockData[-1,]
        save(stockData, file=fileName)
      } else{
        print(paste("Error retrieving data for ", companyTicker, sep=""))
        nullData <- data.frame(companyTicker, NA)
        colnames(nullData) <- c(companyTicker, companyTicker)
        save(nullData, file=fileName)
      }
    }
  }
  compiled <- matrix()
  load(listfiles[1])
  compiled = cbind(compiled, stockData)
  for(i in 2:(numCompanies+1)){
    load(listfiles[i])
    compiled = cbind(compiled, stockData)
  }
  file.remove(listfiles)
  dailydata <- data.frame(compiled, stringsAsFactors=FALSE)[,-1]
#   filepath2 <- paste(filepath, "/dailydata.RData", sep='')
#   save(dailydata, file="dailydata.RData")
  dailydata
}