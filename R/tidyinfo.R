#' Tidies raw financial data.
#'
#' Reads in raw financial data in the /extdata folder of the qmj package,
#' tidies the data found, and then saves that data in the /data folder.
#' @param x A list of lists of financial statements. Generated from get_info(companies).
#' @export

tidyinfo <- function(x){
  #   destpath <- system.file("data", package="qmj")
  #   filepath <- system.file("extdata", package="qmj")   
  #   filepathb <- paste(filepath, "/balancesheets.RData", sep='')
  #   destpathb <- paste(destpath, "/tidybalance.RData", sep='')
  #   load(filepathb)
  tidybalance <- tidy_balancesheets(x[[3]])
  #   save(tidybalance, file=destpathb)
  
  #   filepathc <- paste(filepath, "/cashflows.RData", sep='')
  #   destpathc <- paste(destpath, "/tidycash.RData", sep='')
  #   load(filepathc)
  tidycash <- tidy_cashflows(x[[1]])
  #   save(tidycash, file=destpathc)
  
  #   filepathi <- paste(filepath, "/incomestatements.RData", sep='')
  #    destpathi <- paste(destpath, "/tidyincome.RData", sep='')
  #   load(filepathi)
  tidyincome <- tidy_incomestatements(x[[2]])
  #   save(tidyincome, file=destpathi)

  financials <- merge(tidybalance, merge(tidycash, tidyincome, by=c("ticker", "year")), by=c("ticker", "year"))
  unique(financials)
}