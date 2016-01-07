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
  
  ## Extract the ticker from the first column.  Remove the unecessary digits to grab only the ticker letters.
  symbol <- gsub("[0-9 ]", "", colnames(x))[1]
  
  ## Extract the years these financial statements were filed.  Remove the uncessary letters to grab only the year digits.
  years <- gsub("[A-Z .-]", "", colnames(x))
  
  ## In case a ticker has multiple filings in the same calendar year, we need to differentiate between those rows. We do this by assigning a number in the
  ## order we process that ticker's data
  order <- 1:(ncol(x))
  
  ## Takes the transpose to make the rows become columns and appends the ticker and year columns.
  temp <- cbind(data.frame(ticker = symbol, year = as.numeric(years), order = order, stringsAsFactors = FALSE), data.frame(t(x)))
} 
