#' Makes raw balancesheet data usable and readable.
#'
#' Processes raw balance sheet data produced from quantmod 
#' into a tidy data frame. Raw balance sheet data must be 
#' formatted in a list such that every element is a data 
#' frame or matrix containing quantmod data.
#' 
#' \code{tidy_balancesheets} produces a data frame that is
#' "tidy" or more readily readable by a user and usable by
#' other functions within this package.
#' 
#' @param x A list of raw cash flow data produced from quantmod
#' 
#' @return Returns a data set that's been "tidied" up for use 
#' by other functions in this package.
#' 
#' @seealso \code{\link{get_info}}
#' @seealso \code{\link{tidy_prices}}
#' @seealso \code{\link{tidy_cashflows}}
#' @seealso \code{\link{tidy_incomestatements}}
#' 
#' @examples
#' sub_comps <- qmjdata::companies[1:2,]
#' raw_data <- get_info(sub_comps)
#' tidybalance <- tidy_balancesheets(raw_data[[3]])
#' @export

tidy_balancesheets <- function(x) {
  
  ## Calls tidy_helper to construct a list of data.frames and
  ## merges the list elements into one large data.frame
  
  balancesheets <- do.call(rbind, lapply(x, tidy_helper))

  ## Remove all rows that are solely NAs.
  
  balancesheets <- balancesheets[rowSums(!is.na(balancesheets)) >= 1,] 
  rownames(balancesheets) <- NULL

  ## These are the categories we expect from the raw data, with abbreviations
  ## for each of the variables found in the balanceshee

  names(balancesheets) <- c("ticker", "year", "CE", "STI", "CSTI", "AR", "RE", "TR", "TI", "PE", "OCA", "TCA", 
                            "PPE", "AD", "GDW", "INT", "LTI", "OLTA", "TA", "AP", "AE", "STD", "CL", "OCL", 
                            "TCL", "LTD", "CLO", "TLTD", "TD", "DIT", "MI", "OL", "TL", "RPS", "NRPS", 
                            "CS", "APIC", "RE", "TS", "OE", "TE", "TLSE", "SO", "TCSO")
  
  balancesheets
}