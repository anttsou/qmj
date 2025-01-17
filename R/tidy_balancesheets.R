#' Makes raw balancesheet data usable and readable.
#'
#' Processes raw balance sheet data produced from quantmod 
#' into a tidy data frame. Raw balance sheet data must be 
#' formatted in a list such that every element is a data 
#' frame or matrix containing quantmod data.
#' 
#' \code{tidy_balancesheets} produces a data frame that is
#' 'tidy' or more readily readable by a user and usable by
#' other functions within this package.
#' 
#' @param x A list of raw cash flow data produced from quantmod
#' 
#' @return Returns a data set that's been 'tidied' up for use 
#' by other functions in this package.
#' 
#' @seealso \code{\link{get_info}}
#' @seealso \code{\link{tidyinfo}}
#' @seealso \code{\link{tidy_cashflows}}
#' @seealso \code{\link{tidy_incomestatements}}

tidy_balancesheets <- function(x) {
  
  ## Calls tidy_helper to construct a list of data.frames and merges the list elements into one large data.frame
  # balancesheets <- do.call(rbind, lapply(x, tidy_helper))
  balancesheets <- dplyr::bind_rows(lapply(x, tidy_helper))
  
  ## Remove all rows that are solely NAs.
  balancesheets <- balancesheets[rowSums(!is.na(balancesheets)) >= 1, ]
  rownames(balancesheets) <- NULL
  
  ## These are the categories we expect from the raw data, with abbreviations for each of the variables found in the balancesheet
  ## The wanted categories were based on previous outputs from Google Finance API which is discontinued now
  ## So we have to map the current output (from Yahoo Finance) to previous categories
  balancesheets <- balancesheets[, c("ticker", "year", "order", 'Preferred.Stock', 'Preferred.Stock', 'Total.Assets', 'Current.Assets', 'Current.Liabilities', 'Ordinary.Shares.Number', 'Total.Debt', 'Total.Liabilities.Net.Minority.Interest', 'Stockholders.Equity')]
  
  balancesheets[, -which(names(balancesheets) == "ticker")] <- 
    lapply(balancesheets[, -which(names(balancesheets) == "ticker")], function(column) {
      sapply(column, function(x) if (is.null(x)) NA else x)
    })
  balancesheets[, -which(names(balancesheets) == "ticker")] <- 
    lapply(balancesheets[, -which(names(balancesheets) == "ticker")], as.numeric)
  balancesheets$TLSE <- balancesheets$Total.Liabilities.Net.Minority.Interest + balancesheets$Stockholders.Equity
  balancesheets$Stockholders.Equity <- NULL
  names(balancesheets) <- c("ticker", "year", "order", "NRPS", "RPS", "TA", "TCA", "TCL", "TCSO", "TD", "TL", "TLSE")
  
  balancesheets
} 
