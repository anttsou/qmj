#' A dataframe of safety scores for companies listed in the Russell 3000
#'
#' Displays safety scores as well as its components for companies in the Russell 3000.
#' More information may be found on the technical vignette.
#' Last updated: January 2015
#' 
#' @format A data frame with 2999 rows and 8 variables:
#' \itemize{
#'    \item BAB = Beta, calculated by comparison with the S&P 500
#'    \item IVOL = Idiosyncratic Volatility, also calculated using the S&P 500
#'    \item LEV = Leverage
#'    \item O = Ohlson O-Score.
#'    \item Z = Altman Z-Score
#'    \item EVOL = Standard deviation of annual ROE over the past four years.
#'  }
#'  
#' @name safety
#' @docType data
#' @keywords data
NULL

#' A dataframe of profitability scores for companies listed in the Russell 3000
#'
#' Displays profitability scores as well as its components for companies in the Russell 3000.
#' More information may be found on the technical vignette.
#' 
#' Last updated: January 2015
#' @format A data frame with 2999 rows and 7 variables:
#' \itemize{
#'    \item GPOA = Gross Profits over Assets
#'    \item ROE = Return on Equity
#'    \item ROA = Return on Assets
#'    \item CFOA = Cash Flow over Assets
#'    \item GMAR = Gross Margin
#'    \item ACC = Accruals
#'  }
#'  
#' @name profitability
#' @docType data
#' @keywords data
NULL

#' A dataframe of growth scores for companies listed in the Russell 3000
#'
#' Displays growth scores as well as its components for companies in the Russell 3000.
#' More information may be found on the technical vignette.
#' @format A data frame with 2999 rows and 7 variables:
#' \itemize{
#'    \item GPOA =  Four year growth in Gross Profits over Assets
#'    \item ROE = Four year growth in Return on Equity
#'    \item ROA = Four year growth in Return on Assets
#'    \item CFOA = Four year growth in Cash Flow over Assets
#'    \item GMAR = Four year growth in Gross Margin
#'    \item ACC = Four year growth in Accruals
#'  }
#'
#' @name growth
#' @docType data
#' @keywords data
NULL

#' A dataframe of payout scores for companies listed in the Russell 3000
#'
#' Displays growth scores as well as its components for companies in the Russell 3000.
#' More information may be found on the technical vignette.
#' @format A data frame with 2999 rows and 5 variables:
#' \itemize{
#'    \item EISS = Minus one-year percent change in split-adjusted number of shares.
#'    \item DISS = Minus one-year percent change in total debt.
#'    \item NPOP = Sum of total net payout over the past 4 years divided by total profits over the past 4 years.
#'  }
#'  
#' @name payouts
#' @docType data
#' @keywords data
NULL

#' A list of qmj objects.
#'
#' A list of qmj objects. These objects allow for much more in-depth analysis of a specific company
#' using the summarize, plot_quality, and view functions. (view_profitability, view_growth, view_safety, view_payouts)
#' Last updated: January 2015
#'
#' @name qmjs
#' @docType data
#' @keywords data
NULL