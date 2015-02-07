get_company <- function(companies,ticker,financials,prices) {
  sub.comp <- companies[companies$ticker==ticker,]
  profitability <- market_profitability(sub.comp,financials)
  growth <- market_growth(sub.comp,financials)
  safety <- market_safety(sub.comp,financials,prices)
  payouts <- market_payout(sub.comp,financials)
  quality <- profitability$profitability + growth$growth + safety$safety + payouts$payouts
  
  #add all of the values that go into each component
  company <- Company(
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
  company
}