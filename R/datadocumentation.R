#' Financial statements of all companies for the past four years
#'
#' A data frame containing all financial statements (balancesheets,
#' cashflows, and income statements) for the past four years if available.
#' 
#' 
#' Data is gathered from Google Finance. Data set contains:
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
#' @name financials
#' @docType data
#' @keywords data
NULL

#' A list of all companies of interest
#'
#' Stores the names and tickers for all companies in the Russell 3000 Index.
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
#' insufficient information exists to produce a quality score.
#' 
#' Last updated: January 2015
#'
#' @format A data frame with 2999 rows and 7 variables
#'
#' @name quality
#' @docType data
#' @keywords data
NULL

#' A dataframe of safety scores for companies listed in the Russell 3000
#'
#' Displays safety scores as well as its components for companies in the Russell 3000.
#' More information may be found on the Technical vignette.
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
#' Displays profitability scores as well as its components, this includes:
#' \itemize{
#'    \item GPOA = Gross Profits over Assets
#'    \item ROE = Return on Equity
#'    \item ROA = Return on Assets
#'    \item CFOA = Cash Flow over Assets
#'    \item GMAR = Gross Margin
#'    \item ACC = Accruals
#'  }
#'  
#'  More information may be found on the Technical vignette.
#' Last updated: January 2015
#'
#' @name profitability
#' @docType data
#' @keywords data
NULL

#' A dataframe of growth scores for companies listed in the Russell 3000
#'
#' Displays growth scores as well as its components, this includes:
#' \itemize{
#'    \item GPOA =  Four year growth in Gross Profits over Assets
#'    \item ROE = Four year growth in Return on Equity
#'    \item ROA = Four year growth in Return on Assets
#'    \item CFOA = Four year growth in Cash Flow over Assets
#'    \item GMAR = Four year growth in Gross Margin
#'    \item ACC = Four year growth in Accruals
#'  }
#'  
#'  More information may be found on the Technical vignette.
#' Last updated: January 2015
#'
#' @name growth
#' @docType data
#' @keywords data
NULL

#' A dataframe of payout scores for companies listed in the Russell 3000
#'
#' Displays payout scores as well as its components, this includes:
#' \itemize{
#'    \item EISS = Minus one-year percent change in split-adjusted number of shares.
#'    \item DISS = Minus one-year percent change in total debt.
#'    \item NPOP = Sum of total net payout over the past 4 years divided by total profits over the past 4 years.
#'  }
#'  
#'  More information may be found on the Technical vignette.
#' Last updated: January 2015
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