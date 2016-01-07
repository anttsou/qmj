#' Makes raw cash flow data usable and readable.
#'
#' Processes raw cash flow data from quantmod to return 
#' a tidied data frame. Raw cash flow data must be formatted 
#' in a list such that every element is a data frame or 
#' matrix containing quantmod data.
#' 
#' \code{tidy_cashflows} produces a data frame that is 'tidy' 
#' or more readily readable by a user and usable by other 
#' functions within this package.
#' 
#' @param x A list of raw cash flow data produced from quantmod
#' 
#' @return Returns a data set that's been 'tidied' up for use
#' by other functions in this package.
#' 
#' @seealso \code{\link{get_info}}
#' @seealso \code{\link{tidyinfo}}
#' @seealso \code{\link{tidy_balancesheets}}
#' @seealso \code{\link{tidy_incomestatements}}
#' 
#' @examples
#' sub_comps <- qmjdata::companies[1:2,]
#' raw_data <- get_info(sub_comps)
#' tidycash <- tidy_cashflows(raw_data[[1]])

tidy_cashflows <- function(x) {
  
  ## Calls tidy_helper to construct a list of data.frames and merges the list elements into one large data.frame
  cashflows <- do.call(rbind, lapply(x, tidy_helper))
  
  ## Remove all rows that are solely NAs.
  cashflows <- cashflows[rowSums(!is.na(cashflows)) >= 1, ]
  rownames(cashflows) <- NULL
  
  ## These are the categories we expect from the raw data, with abbreviations for each of the variables found in the cash flows
  names(cashflows) <- c("ticker", "year", "order", "NI.SL", "DP.DPL", "AM", "DT", "NCI", "CWC", "COA", "CX", "OICF", "CIA", "FCFI", "TCDP", "ISN", "IDN", 
    "CFA", "FEE", "NCC", "CIP", "CTP")
  cashflows
} 
