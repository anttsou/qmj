#' A dataframe of price returns and closing prices for companies in the Russell 3000 Index
#'
#' Stores price returns and closing prices for the past two years (if available) for the Russell 3000 Index companies
#' as well as the S&P 500 (uniquely taken from Yahoo finance), to serve as a benchmark.
#' 
#' Prices is used to calculate the safety score of companies, and stores closing stock prices and price returns for
#' every company in \code{\link{companies}} for the past two years. Price data varies significantly among companies,
#' and companies that do not return price data are not represented here. Price returns are also calculated using two
#' adjacent days in the dataset, a timespan which may cover one day or several depending on the company and what
#' day is being considered.
#' 
#' The Russell 3000 Index is an equity index that tracks the performance of the "3000" (this number may actually
#' vary from year to year, but is always in the neighborhood of 3000) largest
#' US companies as measured by market cap. The component companies that make up this index are
#' reconstituted once a year, usually between May and June. At this reconstitution, all companies
#' are reranked based on their market caps for the year, and any companies which become "ineligible" by,
#' for example, going bankrupt, becoming acquired, or becoming private, are replaced at this time.
#' 
#' This Index was chosen because the majority of the information used in this package relies on
#' data sources that are US-centric, in addition to giving reasonable output by using companies which
#' are at least of sufficient size to produce less erroneous items
#' (such as a tiny company doubling in profitability, though the actual change is very small in magnitude)
#' as well as producing items which are more likely to interest the user.
#' 
#' @format A data frame with 1,475,934 rows and 4 variables
#' \itemize{
#'    \item ticker = Company ticker, of class \code{"character"}.
#'    \item date = Date in format YYYY-MM-DD, of class \code{"character"}.
#'    \item pret = Price returns, of class \code{"numeric"}.
#'    \item close = Closing stock prices for the day, of class \code{"numeric"}.
#'  }
#' @source Google Finance, accessed through quantmod
#' @name prices
#' @seealso \code{\link{companies}}
#' @seealso \code{\link{financials}}
#' @seealso \code{\link{market_data}}
#' @seealso \code{\link{market_safety}}
#' @seealso \code{\link{get_prices}}
#' @seealso \code{\link{tidy_prices}}
#' @examples
#' data(companies)
#' data(financials)
#' data(prices)
#' sub_comps <- companies[50:51,]
#' new_quality <- market_data(sub_comps, financials, prices)
#' @docType data
#' @keywords data
NULL