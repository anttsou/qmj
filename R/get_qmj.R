#' Returns a qmj object for a specific company.
#'
#' Given a data frame of companies, a ticker, a data frame of financial statements, and a data frame of prices, creates
#' a particular company qmj object for the company of the corresponding ticker.
#' @param companies A data frame of companies.
#' @param ticker A company ticker as a Character. Must already be present in the companies data frame.
#' @param financials A data frame containing financial information for the given companies.
#' @param prices A data frame containing the daily market closing prices and returns. 
#' @examples
#' data(companies)
#' ticker <- "AAPL"
#' data(financials)
#' data(prices)
#' get_qmj(companies,ticker,financials,prices)
#' @export

get_qmj <- function(companies,ticker,financials,prices) {
  sub.comp <- companies[companies$ticker==ticker,]
  profitability <- market_profitability(sub.comp,financials)
  growth <- market_growth(sub.comp,financials)
  safety <- market_safety(sub.comp,financials,prices)
  payouts <- market_payout(sub.comp,financials)
  quality <- profitability$profitability + growth$growth + safety$safety + payouts$payouts
  
  #add all of the values that go into each component
  analysis <- qmj(
                  ticker = ticker, 
                  profitability = profitability$profitability, 
                  pGPOA = profitability$GPOA,
                  pROE = profitability$ROE,
                  pROA = profitability$ROA,
                  pCFOA = profitability$CFOA,
                  pGMAR = profitability$GMAR,
                  pACC = profitability$ACC,
                  growth = growth$growth,
                  gGPOA = growth$GPOA,
                  gROE = growth$ROE,
                  gROA = growth$ROA,
                  gCFOA = growth$CFOA,
                  gGMAR = growth$GMAR,
                  gACC = growth$ACC,
                  safety = safety$safety,
                  sBAB = safety$BAB,
                  sIVOL = safety$IVOL,
                  sLEV = safety$LEV,
                  sO = safety$O,
                  sZ = safety$Z,
                  sEVOL = safety$EVOL,
                  payouts = payouts$payouts,
                  pEISS = payouts$EISS,
                  pDISS = payouts$DISS,
                  pNPOP = payouts$NPOP,
                  quality = quality)
  analysis
}