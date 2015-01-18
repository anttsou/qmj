#' update
#'
#' Temp function to read in data initially.
#' @export

update <- function(){
  companies <- qmj::getcompanies()
  qmj::getdailydata(companies)
  qmj::getbalancesheets()
  qmj::getcashflows()
  qmj::getincomestatements()
  print(paste("Downloading data to: ", system.file("data", package="qmj"), sep=''))
}