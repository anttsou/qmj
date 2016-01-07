#' Produces component and quality scores when given relevant market data.
#'
#' Calculates market growth, payouts, safety, and 
#' profitability of our list of companies for later 
#' processing.
#' 
#' All parameters default to package data sets.
#' 
#' @return A data frame containing company names, tickers, 
#' profitability z-scores, growth z-scores, safety z-scores,
#' payout z-scores, and quality z-scores. Organized by
#' quality in descending order.
#' 
#' @param companies A data frame of company names and 
#' tickers. 
#' @param financials A data frame containing financial 
#' information for the given companies.
#' @param prices A data frame containing the daily 
#' market closing prices and returns. 
#' 
#' @seealso \code{\link{market_profitability}}
#' @seealso \code{\link{market_growth}}
#' @seealso \code{\link{market_safety}}
#' @seealso \code{\link{market_payouts}}
#' 
#' @examples
#' ## To immediately get quality scores using package data sets.
#' 
#' market_data()
#' 
#' ## If we desire to produce a set of quality scores for a specific
#' ## data frame of companies, which we'll call \code{companies}
#' 
#' # Remove old temporary data, if present.
#' clean_downloads(companies)
#' 
#' # Get raw financial and price data.
#' raw_financials <- get_info(companies)
#' raw_prices <- get_prices(companies)
#' 
#' # Clean raw data for use in market_data.
#' financials <- tidyinfo(raw_financials)
#' prices <- tidy_prices(raw_prices)
#' 
#' quality_scores <- market_data(companies, financials, prices)
#' @importFrom dplyr arrange
#' @import qmjdata
#' @export

market_data <- function(companies = qmjdata::companies, financials = qmjdata::financials, prices = qmjdata::prices) {
  if (length(companies$ticker) == 0) {
    stop("first parameter requires a ticker column.")
  }
  if (length(which(financials$TCSO < 0))) {
    stop("Negative TCSO exists.")
  }
  
  ## First Filter: All companies must have an annual financial statement posted two years ago, we'll call this the target-year. target_year <-
  ## as.numeric(format(Sys.Date(), '%Y')) - 2
  
  ## Valid tickers are all tickers that have financial information for the target year. t <- dplyr::filter(financials, year==2014)
  
  
  
  
  ## Second Filter: All companies must have 3-5 years of contiguous financial data including the target year.
  
  ## Calculate component scores.
  profitability <- market_profitability(companies, financials)$profitability
  growth <- market_growth(companies, financials)$growth
  safety <- market_safety(companies, financials, prices)$safety
  payouts <- market_payouts(companies, financials)$payouts
  
  ## Calculate quality scores and get z-scores.
  quality <- profitability + growth + safety + payouts
  quality <- scale(quality)
  
  name <- companies$name
  ticker <- companies$ticker
  marketdata <- data.frame(name = name, ticker = ticker, profitability = profitability, growth = growth, safety = safety, payouts = payouts, quality = quality)
  
  ## Arrange data by
  marketdata <- dplyr::arrange(marketdata, desc(quality))
  marketdata
} 
