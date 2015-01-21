#' collectmarketdata
#'
#' Reads data from companies.csv and calculates market growth, payouts, safety, and profitability
#' for later processing.
#' @examples
#' collectmarketdata()
#' @export

collectmarketdata <- function(){
  ##Collect market data focuses on collecting needed
  ##  means and sd's for use in other functions.
  filepath <- system.file(package="qmj")
  data(companies, package="qmj")
  data(tidybalance, package="qmj")
  data(tidycash, package="qmj")
  data(tidyincome, package="qmj")
  #companies <- read.csv(paste(filepath, "/companies.csv", sep=''))
  numCompanies <- length(companies$tickers)
  BS <- tidybalance
  CF <- tidycash
  IS <- tidyincome
  
  #What to do with missing data?
  # If we're missing a lot of data, then simply assigning 0's skews
  # the mean and SD. However, short term solution to getting a result.
  BS[is.na(BS)] <- 0
  CF[is.na(CF)] <- 0
  IS[is.na(IS)] <- 0
  
  profitability <- qmj::collectmarketprofitability(companies, BS, CF, IS)
  growth <- qmj::collectmarketgrowth(companies, BS, CF, IS)
  safety <- rep(NaN, numCompanies)
  #safety <- qmj::collectmarketsafety(companies, BS, CF, IS)
  payouts <- qmj::collectmarketpayout(companies, BS, CF, IS)
  
  names <- companies$names
  tickers <- companies$tickers
  data.frame(names, tickers, profitability, growth, safety, payouts)
}