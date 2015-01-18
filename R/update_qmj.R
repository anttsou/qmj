#' update_qmj
#'
#' Temp function to read in data initially.
#' @export

update_qmj <- function(){
  print(paste("Downloading data to: ", system.file("data", package="qmj"), sep=''))
  companies <- qmj::getcompanies()
  qmj::getdailydata(companies)
  filepath <- system.file("data", package="qmj")
  filepath <- paste(filepath, "/companies.csv", sep='')
  write.csv(companies, file=filepath)
  qmj::getbalancesheets()
  qmj::getcashflows()
  qmj::getincomestatements()
}