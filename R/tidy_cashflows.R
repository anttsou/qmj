#' Makes raw cash flow data usable and readable.
#'
#' Processes raw cash flow data from quantmod to return a tidied data frame. Raw cash flow data
#' must be formatted in a list such that every element is a data frame or matrix containing quantmod data.
#' 
#' \code{tidy_cashflows} produces a data frame that is "tidy" or more readily readable by a user and usable by other functions
#' within this package.
#' @param x A list of raw cash flow data produced from quantmod
#' @return Returns a data set that's been "tidied" up for use by other functions in this package.
#' @seealso \code{\link{get_info}}
#' @seealso \code{\link{tidy_prices}}
#' @seealso \code{\link{tidy_balancesheets}}
#' @seealso \code{\link{tidy_incomestatements}}
#' @examples
#' \dontrun{
#' companies <- data(companies)
#' raw_data <- get_info(companies)
#' tidycash <- tidy_cashflows(x[[1]])
#' }
#' @export

tidy_cashflows <- function(x) {
  numCompanies <- length(x)
  cashflows <- matrix(nrow=numCompanies*4, ncol=21) #Pre-allocate space for speed reasons.
  
  for(i in 1:numCompanies){
    cdata <- x[[i]] #Extract this specific company's data from the raw data list.
    ticker <- gsub('[0-9 ]', '', colnames(cdata))[1] #Extract the company's ticker from a column.
    years <- gsub('[ABCDEFGHIJKLMNOPQRSTUVWXYZ .]', '', colnames(cdata)) #Extract the years in which the annual statements have been filed.
    if(length(unique(years)) < length(years)){
      #If more than one annual statement is published in a single year, add a suffix to all years in order to maintain order and remove
      #duplicate years during a later merging process.
      for(j in 1:length(years)){
        years[j] <- paste(years[j], ".", j, sep='')
      }
    }
    for(k in 1:length(colnames(cdata))){
      cashflows[k + (i-1)*4,1] <- ticker
      cashflows[k + (i-1)*4,2] <- years[k]
      cashflows[k + (i-1)*4,3] <- cdata[1,k]
      cashflows[k + (i-1)*4,4] <- cdata[2,k]
      cashflows[k + (i-1)*4,5] <- cdata[3,k]
      cashflows[k + (i-1)*4,6] <- cdata[4,k]
      cashflows[k + (i-1)*4,7] <- cdata[5,k]
      cashflows[k + (i-1)*4,8] <- cdata[6,k]
      cashflows[k + (i-1)*4,9] <- cdata[7,k]
      cashflows[k + (i-1)*4,10] <- cdata[8,k]
      cashflows[k + (i-1)*4,11] <- cdata[9,k]
      cashflows[k + (i-1)*4,12] <- cdata[10,k]
      cashflows[k + (i-1)*4,13] <- cdata[11,k]
      cashflows[k + (i-1)*4,14] <- cdata[12,k]
      cashflows[k + (i-1)*4,15] <- cdata[13,k]
      cashflows[k + (i-1)*4,16] <- cdata[14,k]
      cashflows[k + (i-1)*4,17] <- cdata[15,k]
      cashflows[k + (i-1)*4,18] <- cdata[16,k]
      cashflows[k + (i-1)*4,19] <- cdata[17,k]
      cashflows[k + (i-1)*4,20] <- cdata[18,k]
      cashflows[k + (i-1)*4,21] <- cdata[19,k]

    }
  }
  cashflows <- cashflows[rowSums(!is.na(cashflows)) >= 1,] #Remove rows which are entirely NAs.
  
  cashframe <- data.frame(cashflows, stringsAsFactors=FALSE)
  #These are the categories we expect from the raw data.
  colnames(cashframe) <- c("ticker", "year", "NI.SL", "DP.DPL", "AM", "DT", "NCI", "CWC", "COA", "CX", "OICF", "CIA", 
                           "FCFI", "TCDP", "ISN", "IDN", "CFA", "FEE", "NCC", "CIP", "CTP")
  cashframe[,2:21] = sapply(cashframe[,2:21],as.numeric)
  cashframe
  
}