#' collectmarketdata
#'
#' Reads data from companies.csv and calculates market growth, payouts, safety, and profitability
#' for later processing.
#' @export

collectmarketdata <- function(){
  ##Collect market data focuses on collecting needed
  ##  means and sd's for use in other functions.
  
  companies <- read.csv("data/companies.csv")
  numCompanies <- length(companies$tickers)
  BS <- read.csv("data/balancesheets.csv")
  CF <- read.csv("data/cashflows.csv")
  IS <- read.csv("data/incomestatements.csv")
  
  #What to do with missing data?
  # If we're missing a lot of data, then simply assigning 0's skews
  # the mean and SD. However, short term solution to getting a result.
  BS[is.na(BS)] <- 0
  CF[is.na(CF)] <- 0
  IS[is.na(IS)] <- 0
  
  profitability <- qmj::collectmarketprofitability(companies, BS, CF, IS)
  growth <- qmj::collectmarketgrowth(companies, BS, CF, IS)
  safety <- qmj::collectmarketsafety(companies, BS, CF, IS)
  payouts <- qmj::collectmarketpayout(companies, BS, CF, IS)
  
  names <- companies$names
  tickers <- companies$tickers
  data.frame(names, tickers, profitability, growth, safety, payouts)
}