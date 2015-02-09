#' Creates a list of portfolios.
#'
#' Creates a list of portfolios, which allow for much greater analysis of
#' individual companies.
#' @param x A dataframe of company names and tickers.
#' @param financials a formatted data frame containing financial information for the given companies.
#' @param prices A dataframe containing the daily market closing prices and returns. 
#' @export

create_portfolios <- function(x, financials, prices){
  create_portfolio <- function(ticker, profitability, pGPOA, pROE, pROA, pCFOA, pGMAR, pACC,
                             growth, gGPOA, gROE, gROA, gCFOA, gGMAR, gACC, safety,
                             sBAB, sIVOL, sLEV, sO, sZ, sEVOL, payouts, pEISS, pDISS, pNPOP,
                             quality){
    Portfolio(ticker = ticker, 
                   profitability = profitability, 
                   pGPOA = pGPOA,
                   pROE = pROE,
                   pROA = pROA,
                   pCFOA = pCFOA,
                   pGMAR = pGMAR,
                   pACC = pACC,
                   growth = growth,
                   gGPOA = gGPOA,
                   gROE = gROE,
                   gROA = gROA,
                   gCFOA = gCFOA,
                   gGMAR = gGMAR,
                   gACC = gACC,
                   safety = safety,
                   sBAB = sBAB,
                   sIVOL = sIVOL,
                   sLEV = sLEV,
                   sO = sO,
                   sZ = sZ,
                   sEVOL = sEVOL,
                   payouts = payouts,
                   pEISS = pEISS,
                   pDISS = pDISS,
                   pNPOP = pNPOP,
                   quality = quality)
  }
  
  data_profitability <- market_profitability(x, financials)
  data_growth <- market_growth(x, financials)
  data_safety <- market_safety(x, financials, prices)
  data_payouts <- market_payout(x, financials)
  quality <- data_profitability$profitability + data_growth$growth + data_safety$safety + data_payouts$payouts
  
  mapply(create_portfolio, x$ticker, data_profitability$profitability, data_profitability$GPOA, data_profitability$ROE,
         data_profitability$ROA, data_profitability$CFOA, data_profitability$GMAR, data_profitability$ACC, data_growth$growth,
         data_growth$GPOA, data_growth$ROE, data_growth$ROA, data_growth$CFOA, data_growth$GMAR, data_growth$ACC,
         data_safety$safety, data_safety$BAB, data_safety$IVOL, data_safety$LEV, data_safety$O, data_safety$Z,
         data_safety$EVOL, data_payouts$payouts, data_payouts$EISS, data_payouts$DISS, data_payouts$NPOP, quality)
}