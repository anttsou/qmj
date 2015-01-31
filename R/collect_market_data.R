#' Collects all relevant market data (growth, payout, profitability, and safety) and orders companies
#' by descending quality.
#'
#' Calculates market growth, payouts, safety, and profitability of our list of companies
#' for later processing.
#' @param x A dataframe of company names and tickers.
#' @param financials a formatted data frame containing financial information for the given companies.
#' @param extrafin A dataframe containing a few extran financial statements not consistently found through other methods.
#' @param daily A dataframe containing the daily market closing prices and returns. 
#' @examples
#' data(companies)
#' data(financials)
#' collect_market_data(companies, financials)
#' @export

collect_market_data <- function(x, financials, extrafin, daily){
  numx <- length(x$tickers)
  
  profitability <- collect_market_profitability(x, financials)$profitability
  growth <- collect_market_growth(x, financials)$growth
  safety <- collect_market_safety(x, financials, extrafin, daily)$safety
  payouts <- collect_market_payout(x, financials)$payouts
  quality <- profitability + growth + safety + payouts
  
  name <- x$name
  ticker <- x$ticker
  marketdata <- data.frame(name = name, 
                           ticker = ticker, 
                           profitability = profitability, 
                           growth = growth, 
                           safety = safety, 
                           payouts = payouts, 
                           quality = quality)
  marketdata <- marketdata[order(marketdata$quality,decreasing=TRUE, na.last=NA),]
}