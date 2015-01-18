#' update_qmj
#'
#' Temp function to read in data initially.
#' @export

update_qmj <- function(){
  print(paste("Downloading data to: ", system.file("data", package="qmj"), sep=''))
  companies <- qmj::getcompanies()
  filepath <- system.file("data", package="qmj")
  filepath <- paste(filepath, "/companies.csv", sep='')
  write.csv(companies, file=filepath)
  qmj::getdailydata(companies)
  qmj::getbalancesheets()
  qmj::getcashflows()
  qmj::getincomestatements()
}