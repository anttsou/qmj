#' Gets the daily prices and returns of companies for the past two years.
#'
#' Given a data frame of companies (names and tickers), writes .RData 
#' files for every company in the /extdata folder in the package folder. 
#' If canceled partway through, function is able to find and re-read this 
#' data, allowing resumption of progress.
#' 
#' @param companies A data frame of company names and tickers.
#' 
#' @seealso \code{\link{get_info}}
#' 
#' @examples
#' companies <- qmjdata::companies[1:2,]
#' get_prices(companies)
#' @export

get_prices <- function(companies = qmjdata::companies){
  numCompanies <- length(companies$ticker)
  
  if(numCompanies == 0) {
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
  
  ## Folder destination of all temp files.
  
  filepath <- system.file("extdata", package="qmj") 

  ## We only desire stock data for the past two years.
  
  startDate <- as.POSIXlt(Sys.Date())
  startDate$year <- startDate$year - 2
  startDate <- as.Date(startDate)
  
  ## listfiles stores the location of all temp files we use/create during this process.
  
  listfiles <- rep("", (numCompanies + 1)) 
  
  ## Code below specially gathers the daily data for the S&P 500 for use as a benchmark.
  
  stockData <- quantmod::getSymbols("^GSPC", src="yahoo", auto.assign=FALSE, from=startDate)
  stockData$pret <- pricereturns(stockData)
  stockData <- stockData[-1,]
  absoluteFilePath <- paste0(filepath, "/", "GSPC.RData")
  listfiles[1] <- absoluteFilePath
  save(stockData, file=absoluteFilePath)
  
  ## List of all files in extdata, which should contain all temp files.
  
  filesInDest <- list.files(path = filepath) 
  
  for(i in 1:numCompanies){
    companyTicker <- as.character(companies$ticker[i])
    file <- paste0(companyTicker, ".RData")
    absoluteFilePath <- paste0(filepath, "/", companyTicker, ".RData")
    
    if(is.element(file, filesInDest)){
      
      ## If the temp file already exists, we skip downloading this company's information.
      
      message(paste0(companyTicker, " information found in extdata. Resuming Download."))
      listfiles[i+1] <- absoluteFilePath
    } else{
      stockData <- tryCatch(
        quantmod::getSymbols(companyTicker, src="google", auto.assign = FALSE, from = startDate),
        error = function(e) e
        )
      
      if(!inherits(stockData, "error") && length(stockData[,1]) > 1) {
        
        ## If we successfully retrieved the data, and there's enough of that data to be worth keeping, 
        ## we save it as a temp file.
        
        message(paste0("Price data for ", companyTicker, sep=''))
        stockData$pret <- pricereturns(stockData)
        stockData <- stockData[-1,]
        listfiles[(i) + 1] <- absoluteFilePath
        save(stockData, file=absoluteFilePath)
      } else{
        message(paste0("Error retrieving data for ", companyTicker))
        warning(paste0("No daily data for ", companyTicker))
      }
    }
  }
  
  listfiles <- listfiles[listfiles != ""]
  compiled <- matrix()
  load(listfiles[1])
  compiled = cbind(compiled, stockData)
  
  ## Go through all our temp files and aggregate them.
  
  if(length(listfiles) > 1){
    for(i in 2:(length(listfiles))) {
      load(listfiles[i])
      compiled = cbind(compiled, stockData)
    } 
  }
  
  ## Remove all our temp files.
  
  file.remove(listfiles) 
  
  prices <- data.frame(compiled, stringsAsFactors = FALSE)[,-1]
  prices
}