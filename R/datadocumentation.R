#' Financial statements of all companies for the past four years
#'
#' A data frame containing all financial statements (balancesheets,
#' cashflows, and income statements) for the past four years if available.
#' Data
#' 
#' Data is gathered from Google Finance. Data set contains:
#'  \itemize{
#'    \item CE = Cash & Equivalents.
#'    \item STI = Short Term Investments.
#'    \item CSTI = Cash and Short Term Investments
#'    \item AR = Accounts Receivable - Trade, Net
#'    \item RE = Receivables - Other
#'    
#'  }
#'
#' @name financials
#' @docType data
#' @keywords data
NULL

#' A list of all companies of interest
#'
#' Stores sample set of companies (namely, all those starting with the letter "A" as found on investorguide.com)
#'
#' @name companies
#' @docType data
#' @keywords data
NULL

#' A dataframe of price returns and closing prices for companies
#'
#' Stores price returns and closing prices for the past two years (if available) for the Russell 3000 Index companies
#' as well as the S&P 500, to serve as a benchmark.
#'
#' @name prices
#' @docType data
#' @keywords data
NULL

#' A dataframe quality scores for companies listed in the Russell 3000
#'
#' Displays overall quality scores as well as the scores for profitability, growth,
#' safety, and payouts.
#' 
#' Last updated: January 2015
#'
#' @name quality
#' @docType data
#' @keywords data
NULL