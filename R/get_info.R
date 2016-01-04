#' Gets raw company balance sheets, income statements, and 
#' cash flows from Google Finance.
#'
#' \code{get_info} grabs financial data for a given data frame of companies.
#' 
#' For each ticker in the data frame of companies, \code{get_info} grabs
#' financial data using the quantmod package and generates a list with 
#' three sub-lists. Also writes .RData files to the user's temporary directory. 
#' If cancelled partway through, \code{get_info} is able to find and re-read this
#' data, quickly resuming its progress.
#' 
#' Parameter data frame defaults to provided \code{companies} data set if not specified.
#' 
#' @return A list with three elements. Each element is a list containing
#' all financial documents of a specific type for each company. These lists are, in order,
#' all cash flow statements, all income statements, and all balance sheets.
#' 
#' @param companies A data frame of companies. Must have a ticker column.
#' @seealso \code{\link{get_prices}}
#' @seealso \code{\link{clean_downloads}}
#' 
#' @examples
#' ## If no data frame is provided, the default is the package's companies data set.
#' 
#' get_info()
#' 
#' ## If we want to get information for a specific data frame of companies, called
#' ## comps
#' 
#' get_info(comps)
#' 
#' ## If we then decide to quit the process partway through, and then resume downloading,
#' ## the function usage is identical.
#' 
#' get_info(comps)
#' 
#' ## If we quit the process partway through, and then decide to clean the data to start
#' ## from scratch.
#' 
#' clean_downloads(comps)
#' get_info(comps)
#' @importFrom quantmod getFinancials viewFinancials
#' @export

get_info <- function(companies = qmjdata::companies) {
  tickers = companies$ticker
  if(length(tickers) == 0) {
    stop("parameter requires a ticker column.")
  }
  
  ## These variables are responsible for temporarily storing fetched data.
  filepath <- Sys.getenv("temp")
  listfiles <- rep("", length(tickers))
  filesInDest <- list.files(path = filepath)
  
  for(i in tickers) {
    file <- paste0(i, "-fin", ".RData")
    fileName <- paste0(filepath, "/", i, "-fin.RData")
    if(is.element(file, filesInDest)) {
      
      ## If the temp file already exists, skip downloading this company's information and inform the user.
      message(paste0(i, " information found in temp directory. Resuming Download."))
      listfiles[i] <- fileName
    } else {
      
      ## Test to see if quantmod can successfully grab the financial data.
      prospective <- tryCatch(quantmod::getFinancials(i, auto.assign = FALSE),
                              error = function(e) e)
      matr <- matrix()
      if(!inherits(prospective,"error")) {
        
        ## Grab cash flows from Google Finance.
        ## Structure of statements is extremely similar for income statements 
        ## and balance sheets.
        
        ## First check if a positive number of rows exist. 
        if(nrow(matr <- viewFinancials(prospective, type = 'CF', period = 'A'))) {
          
          ## Rename columns to include the ticker and the year.
          colnames(matr) <- sub("[-][0-9]*[-][0-9]*", "", paste0(i, " ", colnames(matr)))
          
          ## Add company cash flows to building list.
          cashflow <- matr
        }
        
        ## Grab income statements from Google Finance.    
        if(nrow(matr <- viewFinancials(prospective, type = 'IS', period = 'A'))) {
          colnames(matr) <- sub("[-][0-9]*[-][0-9]*", "", paste0(i, " ", colnames(matr)))
          incomestatement <- matr
        }
        
        ## Grab balance sheets from Google Finance 
        if(nrow(matr <- viewFinancials(prospective, type = 'BS', period = 'A'))) {
          colnames(matr) <- sub("[-][0-9]*[-][0-9]*", "", paste0(i, " ", colnames(matr)))
          balancesheet <- matr
        }
        
        clist <- list(cashflow, incomestatement, balancesheet)
        listfiles[i] <- fileName
        save(clist, file = fileName)
      } else {
        message(paste0("Error retrieving data for ", i))
        warning(paste0("No financials for ", i))
      }
    }
  }
  
  ## Extract information from files to compile cash flows, income statements, 
  ## and balance sheets.
  
  listfiles <- listfiles[listfiles != ""]
  cashflows <- list()
  incomestatements <- list()
  balancesheets <- list()
  if(length(listfiles) >= 1){
    for(i in 1:(length(listfiles))) {
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