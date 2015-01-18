#' update_qmj
#'
#' Temp function to read in data initially.
#' @export

update_qmj <- function(){
  companies <- qmj::getcompanies()
  qmj::getdailydata(companies)
  qmj::getbalancesheets()
  qmj::getcashflows()
  qmj::getincomestatements()
  print(paste("Downloading data to: ", system.file("data", package="qmj"), sep=''))
}