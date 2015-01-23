#' Tidies raw financial data.
#'
#' Reads in raw financial data in the /extdata folder of the qmj package,
#' tidies the data found, and then saves that data in the /data folder.
#' @export

tidyinfo <- function(){
  destpath <- system.file("data", package="qmj")
  filepath <- system.file("extdata", package="qmj")
  
  filepathb <- paste(filepath, "/balancesheets.RData", sep='')
  destpathb <- paste(destpath, "/tidybalance.RData", sep='')
  load(filepathb)
  save(qmj::tidy_balancesheets(balancesheets), file=destpathb)
  
  filepathc <- paste(filepath, "/cashflows.RData", sep='')
  destpathc <- paste(destpath, "/tidycash.RData", sep='')
  load(filepathc)
  save(qmj::tidy_cashflows(cashflows), file=destpathc)
  
  filepathi <- paste(filepath, "/incomestatements.RData", sep='')
  destpathi <- paste(destpath, "/tidyincome.RData", sep='')
  load(filepathi)
  save(qmj::tidy_incomestatements(incomestatements), file=destpathi)
}