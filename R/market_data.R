#' Collects all relevant market data (growth, payout, profitability, and safety) and orders companies
#' by descending quality.
#'
#' Calculates market growth, payouts, safety, and profitability of our list of companies
#' for later processing.
#' @param companies A data frame of company names and tickers.
#' @param financials A data frame containing financial information for the given companies.
#' @param prices A data frame containing the daily market closing prices and returns. 
#' @seealso \code{\link{market_profitability}}
#' @seealso \code{\link{market_growth}}
#' @seealso \code{\link{market_safety}}
#' @seealso \code{\link{market_payouts}}
#' @examples
#' data(companies)
#' data(financials)
#' data(prices)
#' sub_comps <- companies[1:5,]
#' market_data(sub_comps, financials, prices)
#' @importFrom dplyr arrange
#' @export

market_data <- function(x, financials, daily){
  if(length(x$ticker) == 0) {
    stop("first parameter requires a ticker column.")
  }
  if(length(which(financials$TCSO < 0))) {
    stop("Negative TCSO exists.")
  }
  profitability <- market_profitability(x, financials)$profitability
  growth <- market_growth(x, financials)$growth
  safety <- market_safety(x, financials, daily)$safety
  payouts <- market_payouts(x, financials)$payouts
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
  marketdata <- dplyr::arrange(marketdata, desc(quality))
}