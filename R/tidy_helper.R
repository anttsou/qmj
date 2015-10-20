#' Main helper function for all tidy functions.
#'
#' This function does the main work of converting
#' raw financial data into organized data frames. 
#' It is used by qmj's tidy functions to reuse common
#' code and to avoid potential mistakes from repeating
#' similar processes.
#' @seealso \code{\link{tidy_cashflows}}
#' @seealso \code{\link{tidy_balancesheets}}
#' @seealso \code{\link{tidy_incomestatements}}
#' 
#' @export

tidy_helper <- function(cdata) {
  
  ## Extract the ticker from the first column. 
  ## Remove the unecessary digits to grab only
  ## the ticker letters.
  
  symbol <- gsub('[0-9 ]', '', colnames(cdata))[1] 
  
  ## Extract the years these financial statements were filed. 
  ## Remove the uncessary letters to grab only the year digits.
  
  years <- gsub('[A-Z]', '', colnames(cdata)) 
  
  ## Takes the transpose to make the rows become columns and appends
  ## the ticker and year columns. 
  
  temp <- cbind(data.frame(ticker = symbol, year = as.numeric(years), 
                           stringsAsFactors = FALSE), 
                data.frame(t(cdata)))
}