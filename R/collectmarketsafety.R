collectmarketsafety <- function(x, BS, CF, IS){
  # CollectMarketProfitability collects data on overall safety
  ## In the market for individual companies for later processing.
  ## x is the list of companies to be processed. BS, CF, IS are financial statements.
  
  numCompanies <- length(x$tickers)
  safety <- rep(0, numCompanies)
  BAB <- rep(0, numCompanies)
  IVOL <- rep(0, numCompanies)
  LEV <- rep(0, numCompanies)
  O <- rep(0, numCompanies)
  Z <- rep(0, numCompanies)
  EVOL <- rep(0, numCompanies)
}