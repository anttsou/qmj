#' Gets the daily prices and returns of companies for the past two years.
#'
#' Given a list of companies (names and tickers), writes .RData files for every company in the /extdata folder
#' in the package folder. If canceled partway through, function is able to find and re-read this data, allowing
#' resumption of progress.
#' @param x A dataframe of company names and tickers.
#' @examples
#' x <- data(companies)
#' \dontrun{get_prices(x)}
#' @export

get_prices <- function(x){
  if(length(x$ticker) == 0) {
    stop("first parameter requires a ticker column.")
  }
  
  pricereturns <- function(x){
    closingprices <- as.numeric(x[,1])
    numEntries <- length(closingprices)
    pricereturns <- rep(0, numEntries)
    for(i in 2:numEntries){
      pricereturns[i] <- log(closingprices[i]) - log(closingprices[i-1])
    }
    pricereturns
  }
  
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
    
    file <- paste(companyTicker, ".RData", sep='')
    fileName <- paste(filepath, "/", companyTicker, ".RData", sep='')
    if(is.element(file, filesInDest)){
      print(paste(companyTicker, "information found in extdata. Resuming Download.", sep=' '))
      listfiles[i+1] <- fileName
    } else{
      stockData <- tryCatch(
        quantmod::getSymbols(companyTicker, src="google", auto.assign=FALSE),
        error=function(e) e
      )
      if(!inherits(stockData, "error") && length(stockData[,1]) > 1 && length(stockData[desiredDates,4]) > 1){
        print(companyTicker)
        stockData <- stockData[desiredDates,4]
        priceData <- stockData
        stockData$pret <- pricereturns(stockData)
        stockData <- stockData[-1,]
        listfiles[(i) + 1] <- fileName
        save(stockData, file=fileName)
      } else{
        print(paste("Error retrieving data for ", companyTicker, sep=""))
        warning(paste("No daily data for",companyTicker,sep=" "))
      }
    }
  }
  listfiles <- listfiles[listfiles != ""]
  compiled <- matrix()
  load(listfiles[1])
  compiled = cbind(compiled, stockData)
  if(length(listfiles) > 1){
    for(i in 2:(length(listfiles))){
      load(listfiles[i])
      compiled = cbind(compiled, stockData)
    } 
  }
  file.remove(listfiles)
  prices <- data.frame(compiled, stringsAsFactors=FALSE)[,-1]
  #   filepath2 <- paste(filepath, "/prices.RData", sep='')
  #   save(prices, file="prices.RData")
  prices
}