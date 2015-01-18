#' update_qmj
#'
#' Temp function to read in data initially.
#' @export

update_qmj <- function(){
  print(paste("Downloading data to: ", system.file("data", package="qmj"), sep=''))
  companies <- qmj::getcompanies()
  print("test")
  filepath <- system.file("data", package="qmj")
  filepath <- paste(filepath, "/companies.csv", sep='')
  print("test")
  write.csv(companies, file=filepath)
  qmj::getdailydata(companies)
  print("test")
  qmj::getbalancesheets()
  print("test")
  qmj::getcashflows()
  print("test")
  qmj::getincomestatements()
  print("test")
}