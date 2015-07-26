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
#' companies <- qmjdata::companies[50:51,]
#' market_data(companies, qmjdata::financials, qmjdata::prices)
#' @importFrom dplyr arrange
#' @export

market_data <- function(companies = qmjdata::companies, 
                        financials = qmjdata::financials, 
                        prices = qmjdata::prices){
  if(length(companies$ticker) == 0) {
    stop("first parameter requires a ticker column.")
  }
  if(length(which(financials$TCSO < 0))) {
    stop("Negative TCSO exists.")
  }
  profitability <- market_profitability(companies, financials)$profitability
  growth <- market_growth(companies, financials)$growth
  safety <- market_safety(companies, financials, prices)$safety
  payouts <- market_payouts(companies, financials)$payouts
  quality <- profitability + growth + safety + payouts
  
  name <- companies$name
  ticker <- companies$ticker
  marketdata <- data.frame(name = name, 
                           ticker = ticker, 
                           profitability = profitability, 
                           growth = growth, 
                           safety = safety, 
                           payouts = payouts, 
                           quality = quality)
  marketdata <- dplyr::arrange(marketdata, desc(quality))
  marketdata
}