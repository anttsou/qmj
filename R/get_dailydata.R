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
  numCompanies <- length(x$tickers)
  thisYear <- as.numeric(format(Sys.Date(), "%Y"))
  desiredDates <- paste(thisYear - 5, "/", sep='')
  listfiles <- rep("", 2*(numCompanies + 1))
  #Block of code below specially gathers the daily data for the S&P 500 for use as a benchmark.
  stockData <- quantmod::getSymbols("^GSPC", src="yahoo", auto.assign=FALSE)
  stockData <- stockData[desiredDates,6]
  priceData <- stockData[,1]
  #Calculates price returns. Not total returns.
  stockData[,1] <- qmj::pricereturns(stockData)
  fileName <- paste(filepath, "/", "GSPC.RData", sep='')
  fileName2 <- paste(filepath,"/", "GSPCprice.RData", sep='')
  listfiles[1] <- fileName
  listfiles[2] <- fileName2
  
  save(stockData, file=fileName)
  save(priceData, file=fileName2)
  for(i in 1:numCompanies){
    companyTicker <- as.character(x$ticker[i])
    print(companyTicker)
    stockData <- tryCatch(
      quantmod::getSymbols(companyTicker, src="google", auto.assign=FALSE),
      error=function(e) e
    )
    if(!inherits(stockData, "error") && length(stockData[,1]) > 1 && length(stockData[desiredDates,4]) > 1){
      stockData <- stockData[desiredDates,4]
      priceData <- stockData
      stockData[,1] <- qmj::pricereturns(stockData)
      fileName <- paste(filepath, "/", companyTicker, ".RData", sep='')
      fileName2 <- paste(filepath, "/", companyTicker, "price", ".RData", sep='')
      listfiles[(2*i) + 1] <- fileName
      listfiles[(2*i) + 2] <- fileName2
      
      save(stockData, file=fileName)
      save(priceData, file=fileName2)
    } else{
      print(paste("Error retrieving data for ", companyTicker, sep=""))
      fileName <- paste(filepath, "/", companyTicker, ".RData", sep='')
      fileName2 <- paste(filepath, "/", companyTicker, "price", ".RData", sep='')
      listfiles[(2*i) + 1] <- fileName
      listfiles[(2*i) + 2] <- fileName2
      nullData <- data.frame(companyTicker, NA)
      colnames(nullData) <- c(companyTicker, companyTicker)
      save(nullData, file=fileName)
      save(nullData, file=fileName2)
    }
  }
  compiled <- matrix()
  load(listfiles[1])
  load(listfiles[2])
  compiled = cbind(compiled, stockData)
  compiled = cbind(compiled, priceData)
  for(i in 2:numCompanies){
    load(listfiles[(2*i) - 1])
    load(listfiles[2*i])
    compiled = cbind(compiled, stockData)
    compiled = cbind(compiled, priceData)
  }
  file.remove(listfiles)
  dailydata <- data.frame(compiled)[,-1]
  filepath2 <- paste(filepath, "/dailydata.RData", sep='')
  save(dailydata, file="dailydata.RData")
  dailydata
}