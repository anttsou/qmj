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
  tidybalance <- qmj::tidy_balancesheets(balancesheets)
  save(tidy_balance, file=destpathb)
  
  filepathc <- paste(filepath, "/cashflows.RData", sep='')
  destpathc <- paste(destpath, "/tidycash.RData", sep='')
  load(filepathc)
  tidycash <- qmj::tidy_cashflows(cashflows)
  save(tidy_cash, file=destpathc)
  
  filepathi <- paste(filepath, "/incomestatements.RData", sep='')
  destpathi <- paste(destpath, "/tidyincome.RData", sep='')
  load(filepathi)
  tidyincome <- qmj::tidy_incomestatements(incomestatements)
  save(tidy_income, file=destpathi)
}