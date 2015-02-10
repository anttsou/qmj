#' Financial statements of all companies in the Russell 3000 index for the past four years
#'
#' A data frame containing all financial statements (balancesheets,
#' cashflows, and income statements) for the past four years if available. 
#' 
#' Some companies may store "weird" data, such as having information solely for the years 1997-2001, or by having multiple 
#' annual reports within the same year (such as one report being filed in March of 2013, and another filed in December of 2013). In the case of companies
#' reporting multiple annual data from the same year, the years of their reports are suffixed with their order. For
#' example, GOOG may have data from 2013.1, 2013.2, 2012.3, 2011.4. This means Google's most recent data set is from
#' 2013 (2013.1), another data set was published in 2013 (2013.2), and the remaining years are also suffixed for convenience.
#' 
#' @format A data frame with 11,112 rows and 23 variables
#'  \itemize{
#'    \item AM = Amortization
#'    \item CWC = Changes in Working Capital
#'    \item CX = Capital Expenditures
#'    \item DIVC = Dividends per Share
#'    \item DO = Discontinued Operations
#'    \item DP.DPL = Depreciation/Depletion
#'    \item GPROF = Gross Profits
#'    \item IAT = Income After Taxes
#'    \item IBT = Income Before Taxes
#'    \item NI = Net Income
#'    \item NINT = Interest and Expense - Net Operating
#'    \item NRPS = Non-redeemable Preferred Stock
#'    \item RPS = Redeemable Preferred Stock
#'    \item TA = Total Assets
#'    \item TCA = Total Current Assets
#'    \item TCL = Total Current Liabilities
#'    \item TCSO = Total Common Shares Outstanding
#'    \item TD = Total Debt
#'    \item TL = Total Liabilities
#'    \item TLSE = Total Liabilities and Shareholders' Equity
#'    \item TREV = Total Revenue
#'  }
#'  
#' @source Google Finance, accessed through quantmod
#' @name financials
#' @docType data
#' @keywords data
NULL

#' A list of all companies in the Russell 3000 Index.
#'
#' Stores the names and tickers for all companies in the Russell 3000 Index as of January 2015.
#' The list from which the data was culled was last updated 6/27/2014.
#' @format A data frame with 2999 rows and 2 variables.
#' \itemize{
#'    \item name = The name of the company.
#'    \item ticker = The ticker of the company.
#'  }
#' @source \url{https://www.russell.com/documents/indexes/membership/membership-russell-3000.pdf}
#' @name companies
#' @docType data
#' @keywords data
NULL

#' A dataframe of price returns and closing prices for companies
#'
#' Stores price returns and closing prices for the past two years (if available) for the Russell 3000 Index companies
#' as well as the S&P 500 (uniquely taken from Yahoo finance), to serve as a benchmark.
#' 
#' @format A data frame with 1,475,934 rows and 4 variables
#' @source Google Finance, accessed through quantmod
#' @name prices
#' @docType data
#' @keywords data
NULL

#' A dataframe of quality scores for companies listed in the Russell 3000
#'
#' Displays overall quality scores as well as the scores for profitability, growth,
#' safety, and payouts. Companies are sorted in order of quality score, with NAs stored
#' at the end of the data set. 
#' 
#' If partial information exists (i.e., a profitability score
#' was able to be calculated), then those scores are kept for that company, even if
#' insufficient information exists to produce a quality score. More details may be found
#' on the technical vignette.
#' 
#' Last updated: January 2015
#'
#' @format A data frame with 2999 rows and 7 variables
#' \itemize{
#'    \item quality
#'    \item profitability
#'    \item growth
#'    \item safety
#'    \item payouts
#'  }
#'
#' @name quality
#' @docType data
#' @keywords data
NULL

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

#' A list of Company Portfolios
#'
#' A list of company portfolios. These portfolios allow for much more in-depth analysis of a specific company
#' using the summarize, plot_quality, and view functions. (view_profitability, view_growth, view_safety, view_payouts)
#' Last updated: January 2015
#'
#' @name portfolios
#' @docType data
#' @keywords data
NULL