#' Main helper function for all tidy functions.
#'
#' This function does the main work of converting
#' raw financial data into organized data frames. 
#' It is used by qmj's tidy functions to reuse common
#' code and to avoid potential mistakes from repeating
#' similar processes.
#' 
#' @param x A matrix containing financial information, 
#' either cash flows, balancesheets, or income statements,
#' downloaded from Google Finance. The formatting of the 
#' matrix has not been altered yet, as if just retrieved.
#' 
#' @seealso \code{\link{tidy_cashflows}}
#' @seealso \code{\link{tidy_balancesheets}}
#' @seealso \code{\link{tidy_incomestatements}}

tidy_helper <- function(x) {

  ## Extract the ticker from the first column. 
  ## Remove the unecessary digits to grab only
  ## the ticker letters.
  symbol <- gsub('[0-9 ]', '', colnames(x))[1] 
  
  ## Extract the years these financial statements were filed. 
  ## Remove the uncessary letters to grab only the year digits.
  years <- gsub('[A-Z .-]', '', colnames(x))
  
  ## If a ticker has multiple filings in the same calendar year, we need
  ## to differentiate the two filings. Preferably by XXXX.1, and XXXX.2
  
  
  ## Takes the transpose to make the rows become columns and appends
  ## the ticker and year columns. 
  temp <- cbind(data.frame(ticker = symbol, year = as.numeric(years), 
                           stringsAsFactors = FALSE), 
                data.frame(t(x)))
}