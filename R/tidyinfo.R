#' Formats raw financial data.
#'
#' \code{tidyinfo} works by formatting and curtailing the 
#' raw data generated by quantmod (and, by extension, 
#' the \code{get_info} function of this package)
#' 
#' @param x A list of lists of financial statements. 
#' Generated from get_info(companies).
#' 
#' @return Returns a data set that is usable by the 
#' other functions of this package, as well as being 
#' generally more readable.
#' 
#' @seealso \code{\link{get_info}}
#' @seealso \code{\link{tidy_cashflows}}
#' @seealso \code{\link{tidy_balancesheets}}
#' @seealso \code{\link{tidy_incomestatements}}
#' 
#' @examples
#' \donttest{
#' if (reticulate::py_module_available("yfinance")) {
#'   my_companies <- data.frame(ticker = c('GOOG', 'IBM'))
#'   raw_data <- get_info(my_companies)
#'   financials <- tidyinfo(raw_data)
#' }
#' }
#' 
#' @return data.frame of cleaned info (cash flows, income statements, balance sheets)
#' 
#' @export

tidyinfo <- function(x) {
  # Index is the current structure of the output of the get_info function.
  tidycash <- tidy_cashflows(x[[1]])
  tidyincome <- tidy_incomestatements(x[[2]])
  tidybalance <- tidy_balancesheets(x[[3]])
  
  financials <- merge(tidybalance, merge(tidycash, tidyincome, by = c("ticker", "order", "year")), by = c("ticker", "order", "year"))
  
  # The columns below are the only ones used in our formulas, and so the other columns are culled out.
  keep <- c(
    "ticker", "year", "order",
    # Cash flow columns
    "AM", "CWC", "CX", "DIVC", "DP.DPL", 
    # Income statement columns
    "DO", "GPROF", "IAT", "IBT", "NI", "NINT", "TREV", 
    # Balance sheet columns 
    "NRPS", "RPS", "TA", "TCA", "TCL", "TCSO", "TD", "TL", "TLSE")
  financials <- financials[keep]
  rownames(financials) <- NULL  # We're not interested in keeping the row numbers.
  financials[financials$order <= 4, ]
} 
