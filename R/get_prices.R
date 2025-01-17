#' Grab daily prices and price returns for the previous two years.
#'
#' \code{get_prices} grabs price-related data for a given data frame
#' of companies and returns a matrix-like object containing relevant
#' price data.
#' 
#' \code{get_prices} is also able to write .RData files
#' to the user's temporary directory. If canceled partway through,
#' the function is able to find and re-read this data to resume progress.
#' Once complete, the function deletes all used temporary data.
#' 
#' Parameter defaults to provided data set of companies if empty.
#' 
#' @return A matrix-like object containing relevant price data. The rows
#' of the matrix are dates in the international standard of YYYY-MM-DD. Each
#' column specifies what data it covers in the form of TICKER.DATA, with the
#' exception of price returns, which are stored as pret.#, where # refers to
#' the the i-th company given. 
#' 
#' The first company calculated is always the S&P 500, and its price return
#' column is simply 'pret'.
#' 
#' @param companies A data frame of company names and tickers.
#' 
#' @seealso \code{\link{get_info}}
#' @seealso \code{\link{clean_downloads}}
#' @seealso \code{\link{tidy_prices}}
#' 
#' @examples
#' get_prices(companies_r3k16[companies_r3k16$ticker %in% c("AAPL", "AMZN"), ])
#' 
#' @importFrom quantmod getSymbols
#' @export

get_prices <- function(companies = qmj::companies_r3k16) {
  numCompanies <- length(companies$ticker)
  if (numCompanies == 0) {
    stop("parameter requires a ticker column.")
  }
  
  #' @describeIn pricereturns Calculates price returns for an xts object.
  pricereturns <- function(x) {
    closingprices <- as.numeric(x[, 1])
    numEntries <- length(closingprices)
    pricereturns <- rep(0, numEntries)
    for (i in 2:numEntries) {
      pricereturns[i] <- log(closingprices[i]) - log(closingprices[i - 1])
    }
    pricereturns
  }
  
  ## Folder destination of all temp files.
  filepath <- tempdir()
  
  ## We only desire stock data for the past two years.
  startDate <- as.POSIXlt(Sys.Date())
  startDate$year <- startDate$year - 2
  startDate <- as.Date(startDate)
  
  ## listfiles stores the location of all temp files we use/create during this process.
  listfiles <- rep("", (numCompanies + 1))
  
  ## Code below specially gathers the daily data for the S&P 500 for use as a benchmark.
  stockData <- quantmod::getSymbols("^GSPC", src="yahoo", auto.assign=FALSE, from=startDate,
                                    warnings=FALSE, messages=FALSE)
  stockData$pret <- pricereturns(stockData)
  stockData <- stockData[-1, ]
  absoluteFilePath <- paste0(filepath, "/", "GSPC.RData")
  listfiles[1] <- absoluteFilePath
  save(stockData, file = absoluteFilePath)
  
  ## List of all files in temp directory, which should contain all temp files.
  filesInDest <- list.files(path = filepath)
  
  for (i in 1:numCompanies) {
    companyTicker <- as.character(companies$ticker[i])
    file <- paste0(companyTicker, ".RData")
    absoluteFilePath <- paste0(filepath, "/", companyTicker, ".RData")
    
    if (is.element(file, filesInDest)) {
      
      ## If the temp file already exists, we skip downloading this company's information.
      message(paste0(companyTicker, " information found in temp directroy. Resuming Download."))
      listfiles[i + 1] <- absoluteFilePath
      
    } else {
      stockData <- tryCatch(quantmod::getSymbols(companyTicker, src="yahoo", auto.assign=FALSE, from=startDate,
                                                 warnings=FALSE, messages=FALSE), 
                            error = function(e) e)
      
      ## If we successfully retrieved the data, and there's enough of that data to be worth keeping, we save it as a temp file.
      if (!inherits(stockData, "error") && length(stockData[, 1]) > 1) {
        ## Don't want TICK.Adjusted column
        stockData <- stockData[, -ncol(stockData)]
        
        ## Inform the user of progress.
        message(paste0("Price data for ", companyTicker, sep = ""))
        
        ## Add price return data to stockData, remove oldest row (as pret data for that point is undefined), and save the information in a temporary file.
        stockData$pret <- pricereturns(stockData)
        stockData <- stockData[-1, ]
        listfiles[(i) + 1] <- absoluteFilePath
        save(stockData, file = absoluteFilePath)
      } else {
        message(paste0("Error retrieving data for ", companyTicker))
      }
    }
  }
  
  listfiles <- listfiles[listfiles != ""]
  compiled <- matrix()
  load(listfiles[1])
  compiled = cbind(compiled, stockData)
  
  ## Go through all our temp files and aggregate them.
  if (length(listfiles) > 1) {
    for (i in 2:(length(listfiles))) {
      load(listfiles[i])
      compiled = cbind(compiled, stockData)
    }
  }
  
  ## Remove all our temp files.
  file.remove(listfiles)
  
  prices <- data.frame(compiled, stringsAsFactors = FALSE)[, -1]
  prices
} 
