#' Gets raw financial statements from Google Finance.
#'
#' \code{get_info} grabs annual financial data for a given data frame of companies.
#' 
#' For each ticker in the data frame of companies, \code{get_info} grabs
#' financial data using the quantmod package and generates a list with 
#' three sub-lists. Also writes .RData files to the user's temporary directory. 
#' If cancelled partway through, \code{get_info} is able to find and re-read this
#' data, quickly resuming its progress. Once complete, \code{get_info} deletes
#' all used temporary data.
#' 
#' Parameter data frame defaults to provided \code{companies_r3k16} data set if not specified.
#' 
#' @return A list with three elements. Each element is a list containing
#' all financial documents of a specific type for each company. These lists are, in order,
#' all cash flow statements, all income statements, and all balance sheets.
#' 
#' @param companies A data frame of companies. Must have a ticker column.
#' 
#' @seealso \code{\link{get_prices}}
#' @seealso \code{\link{clean_downloads}}
#' @seealso \code{\link{tidyinfo}}
#' 
#' @examples
#' \donttest{
#' # Takes more than 10 secs
#' if (reticulate::py_module_available("yfinance")) 
#'   get_info(companies_r3k16[companies_r3k16$ticker %in% c("AAPL", "AMZN"), ])
#' }
#' 
#' @importFrom quantmod getFinancials viewFinancials
#' @importFrom reticulate py_run_string import
#' @export

get_info <- function(companies = qmj::companies_r3k16) {
  tickers = companies$ticker
  if (length(tickers) == 0) {
    stop("parameter requires a ticker column.")
  }
  
  ## These variables are responsible for temporarily storing fetched data.
  filepath <- tempdir()
  listfiles <- rep("", length(tickers))
  filesInDest <- list.files(path = filepath)
  
  for (ticker in tickers) {
    file <- paste0(ticker, "-fin", ".RData")
    fileName <- paste0(filepath, "/", ticker, "-fin.RData")
    
    if (is.element(file, filesInDest)) {
      
      ## If the temp file already exists, skip downloading this company's information and inform the user.
      message(paste0(ticker, " information found in temp directory. Resuming Download."))
      listfiles[ticker] <- fileName
    } else {
      py_run_string("import yfinance as yf")
      yf <- import("yfinance")
    
      tryCatch({
        # Use yfinance to download financial data for the ticker
        stock <- yf$Ticker(ticker)
        
        # Retrieve financial statements
        cashflow <- stock$cashflow
        incomestatement <- stock$financials
        balancesheet <- stock$balance_sheet
        
        # Process cash flow statement
        if (!is.null(cashflow)) {
          colnames(cashflow) <- sub("[-][0-9]*[-][0-9]*.*", "", paste0(ticker, " ", colnames(cashflow)))
          cashflow_matrix <- as.matrix(cashflow)
        } else {
          cashflow_matrix <- matrix()
        }
        
        # Process income statement
        if (!is.null(incomestatement)) {
          colnames(incomestatement) <- sub("[-][0-9]*[-][0-9]*.*", "", paste0(ticker, " ", colnames(incomestatement)))
          income_matrix <- as.matrix(incomestatement)
        } else {
          income_matrix <- matrix()
        }
        
        # Process balance sheet
        if (!is.null(balancesheet)) {
          colnames(balancesheet) <- sub("[-][0-9]*[-][0-9]*.*", "", paste0(ticker, " ", colnames(balancesheet)))
          balance_matrix <- as.matrix(balancesheet)
        } else {
          balance_matrix <- matrix()
        }
        
        ## Create a temporary list containing the data, and save the result to an RData file.
        clist <- list(cashflow_matrix, income_matrix, balance_matrix)
        listfiles[ticker] <- fileName
        save(clist, file = fileName)
      }, error = function(e) {
        message(paste0("Error retrieving data for ", ticker, ": ", e$message))
        warning(paste0("No financials for ", ticker))
      })
    }
  }
  
  ## Extract information from files to compile cash flows, income statements, and balance sheets.
  listfiles <- listfiles[listfiles != ""]
  cashflows <- list()
  incomestatements <- list()
  balancesheets <- list()
  
  if (length(listfiles) >= 1) {
    for (i in 1:(length(listfiles))) {
      load(listfiles[i])
      cashflows <- c(cashflows, clist[1])
      incomestatements <- c(incomestatements, clist[2])
      balancesheets <- c(balancesheets, clist[3])
    }
  }
  resultlist <- list(cashflows, incomestatements, balancesheets)
  file.remove(listfiles)
  resultlist
} 
