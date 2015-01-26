#' Collects all relevant market data (growth, payout, profitability, and safety) and orders companies
#' by descending quality.
#'
#' Reads data from companies.csv and calculates market growth, payouts, safety, and profitability
#' for later processing.
#' @examples
#' collect_market_data()
#' @export

collect_market_data <- function(){
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
  
  BS[is.na(BS)] <- 0
  CF[is.na(CF)] <- 0
  IS[is.na(IS)] <- 0
  
  profitability <- qmj::collect_market_profitability(companies, BS, CF, IS)$profitability
  growth <- qmj::collect_market_growth(companies, BS, CF, IS)$growth
  safety <- qmj::collect_market_safety(companies, BS, CF, IS, extrafin, daily)$safety
  payouts <- qmj::collect_market_payout(companies, BS, IS)$payouts
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
}