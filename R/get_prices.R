#' Gets the daily prices and returns of companies for the past two years.
#'
#' Given a data frame of companies (names and tickers), writes .RData files for every company in the /extdata folder
#' in the package folder. If canceled partway through, function is able to find and re-read this data, allowing
#' resumption of progress.
#' @param companies A data frame of company names and tickers.
#' @seealso \code{\link{get_info}}
#' @examples
#' data(companies)
#' companies <- companies[1:2,]
#' get_prices(companies)
#' @export

get_prices <- function(companies){
  if(length(companies$ticker) == 0) {
    stop("parameter requires a ticker column.")
  }
  
  #' @describeIn pricereturns Calculates price returns for an xts object.
  pricereturns <- function(x){
    closingprices <- as.numeric(x[,1])
    numEntries <- length(closingprices)
    pricereturns <- rep(0, numEntries)
    for(i in 2:numEntries){
      pricereturns[i] <- log(closingprices[i]) - log(closingprices[i-1])
    }
    pricereturns
  }
  
  numCompanies <- length(x$ticker)
  filepath <- system.file("extdata", package="qmj") #folder destination of all temp files.
  thisYear <- as.numeric(format(Sys.Date(), "%Y"))
  desiredDates <- paste(thisYear - 2, "/", sep='') #We only desire stock data for the past two years.
  listfiles <- rep("", (numCompanies + 1)) #listfiles stores the location of all temp files we use/create during this process.
  
  #Code below specially gathers the daily data for the S&P 500 for use as a benchmark.
  stockData <- quantmod::getSymbols("^GSPC", src="yahoo", auto.assign=FALSE)
  stockData <- stockData[desiredDates,6]
  stockData$pret <- pricereturns(stockData)
  stockData <- stockData[-1,]
  fileName <- paste(filepath, "/", "GSPC.RData", sep='')
  listfiles[1] <- fileName
  save(stockData, file=fileName)
  
  filesInDest <- list.files(path=filepath) #List of all files in extdata, which should contain all temp files.
  for(i in 1:numCompanies){
    companyTicker <- as.character(x$ticker[i])
    
    file <- paste(companyTicker, ".RData", sep='')
    fileName <- paste(filepath, "/", companyTicker, ".RData", sep='')
    if(is.element(file, filesInDest)){
      #If the temp file already exists, we skip downloading this company's information.
      print(paste(companyTicker, "information found in extdata. Resuming Download.", sep=' '))
      listfiles[i+1] <- fileName
    } else{
      stockData <- tryCatch(
        quantmod::getSymbols(companyTicker, src="google", auto.assign=FALSE),
        error=function(e) e
      )
      if(!inherits(stockData, "error") && length(stockData[,1]) > 1 && length(stockData[desiredDates,4]) > 1){
        #If we successfully retrieved the data, and there's enough of that data to be worth keeping, we save it as a temp file.
        print(companyTicker)
        stockData <- stockData[desiredDates,4]
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
    #Go through all our temp files and aggregate them.
    for(i in 2:(length(listfiles))){
      load(listfiles[i])
      compiled = cbind(compiled, stockData)
    } 
  }
  
  file.remove(listfiles) #Remove all our temp files.
  prices <- data.frame(compiled, stringsAsFactors=FALSE)[,-1]
  #   filepath2 <- paste(filepath, "/prices.RData", sep='')
  #   save(prices, file="prices.RData")
  prices
}