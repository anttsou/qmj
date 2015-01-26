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
  data(tidydaily, package="qmj")
  data(extrafin, package="qmj")
  numCompanies <- length(companies$tickers)
  BS <- tidybalance
  CF <- tidycash
  IS <- tidyincome
  daily <- tidydaily
  #What to do with missing data?
  # If we're missing a lot of data, then simply assigning 0's skews
  # the mean and SD. However, short term solution to getting a result.
  BS[is.na(BS)] <- 0
  CF[is.na(CF)] <- 0
  IS[is.na(IS)] <- 0
  
  profitability <- qmj::collectmarketprofitability(companies, BS, CF, IS)$profitability
  growth <- qmj::collectmarketgrowth(companies, BS, CF, IS)$growth
  safety <- qmj::collectmarketsafety(companies, BS, CF, IS, extrafin, daily)$safety
  payouts <- qmj::collectmarketpayout(companies, BS, IS)$payouts
  quality <- profitability + growth + safety + payouts
  
  names <- companies$names
  tickers <- companies$tickers
  marketdata <- data.frame(names = names, 
                           tickers = tickers, 
                           profitability = profitability, 
                           growth = growth, 
                           safety = safety, 
                           payouts = payouts, 
                           quality = quality)
  marketdata <- marketdata[order(marketdata$quality,decreasing=TRUE,na.rm=TRUE),]
#   filepath2 <- paste(filepath, "/data/marketdata.RData", sep='')
#   save(marketdata,file=filepath2)
}