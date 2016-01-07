#' Makes raw incomestatement data usable and readable.
#'
#' Tidies raw income statement data produced from quantmod and returns 
#' the tidied data frame. Raw income statement data must be formatted 
#' in a list such that every element is a data frame or matrix containing
#' quantmod data.
#' 
#' \code{tidy_incomestatements} produces a data frame that is 'tidy' 
#' or more readily readable by a user and usable by other functions within 
#' this package.
#' 
#' @param x A list of raw incomestatement file data produced from quantmod
#' 
#' @return Returns a data set that's been 'tidied' up for use by other 
#' functions in this package.
#' 
#' @seealso \code{\link{get_info}}
#' @seealso \code{\link{tidyinfo}}
#' @seealso \code{\link{tidy_cashflows}}
#' @seealso \code{\link{tidy_balancesheets}}
#' 
#' @examples
#' sub_comps <- qmjdata::companies[1:2,]
#' raw_data <- get_info(sub_comps)
#' tidyincome <- tidy_incomestatements(raw_data[[2]])

tidy_incomestatements <- function(x) {
  
  ## Calls tidy_helper to construct a list of data.frames and merges the list elements into one large data.frame
  incomestatements <- do.call(rbind, lapply(x, tidy_helper))
  
  ## Remove all rows that are solely NAs.
  incomestatements <- incomestatements[rowSums(!is.na(incomestatements)) >= 1, ]
  rownames(incomestatements) <- NULL
  
  ## These are the categories we expect from the raw data, with abbreviations for each of the variables found in the income statements
  names(incomestatements) <- c("ticker", "year", "order", "REV", "OREV", "TREV", "CREV", "GPROF", "SGAE", "RD", "DP.AM", "NINT", "UI", "OOE", "TOE", "OI", 
    "INT", "GSA", "OTH", "IBT", "IAT", "MI", "EIA", "NIBEI", "AC", "DO", "EI", "NI", "PD", "IACEEI", "IACIEI", "BWAS", "BEPSEEI", "BEPSIEI", "DILADJ", 
    "DILWAS", "DILEPSEEI", "DILEPSIEI", "DIVC", "GDIV", "NIASBCE", "BEPSSBCE", "DEPSSBCE", "DPSUP", "TSI", "NIBT", "ESIIT", "ITISI", "NIAT", "NIAC", 
    "BNEPS", "DNEPS")
  incomestatements
} 
