#' A list of all companies in the Russell 3000 Index
#'
#' Stores the names and tickers for all companies in the Russell 3000 Index as of January 2015.
#' The list from which the data was culled was last updated 6/27/2014.
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
#' Companies crucially provides tickers to many functions in the package, allowing the package to connect
#' financial statements and price information to a specific company. It is also the basis of the many "get"
#' functions of the package, which retrieves and then formats data from the web. Companies is the "base" data
#' that produces financials, prices, and ultimately quality scores.
#' 
#' @format A data frame with 2999 rows and 2 variables.
#' \itemize{
#'    \item name = The name of the company. Of class \code{"character"}.
#'    \item ticker = The ticker of the company. Of class \code{"character"}.
#'  }
#' @source \url{https://www.russell.com/documents/indexes/membership/membership-russell-3000.pdf}
#' @name companies
#' @seealso \code{\link{financials}}
#' @seealso \code{\link{prices}}
#' @seealso \code{\link{get_info}}
#' @seealso \code{\link{tidyinfo}}
#' @seealso \code{\link{get_prices}}
#' @seealso \code{\link{tidy_prices}}
#' @examples
#' data(companies)
#' data(financials)
#' data(prices)
#' sub_comps <- companies[50:51,]
#' market_data(sub_comps, financials, prices)
#' @docType data
#' @keywords data
NULL