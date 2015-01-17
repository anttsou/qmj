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
  benchmarkReturns <- read.csv("data/GSPC.csv")[,2]
  
  for(i in 1:numCompanies){
    cBS <- BS[,(4*i)-2]
    cCF <- CF[,(4*i)-2]
    cIS <- IS[,(4*i)-2]
    returnsFile <- paste(x$tickers[i], ".csv", sep='')
    companyFile <- read.csv(returnsFile)
    #BAB
    BAB[i] <- PerformanceAnalytics::CAPM.beta(companyFile[,2], benchmarkReturns)
    
  }
  
  #Scale converts the individual scores for these values into z-scores.
  GPOA <- scale(GPOA)
  
  GPOA[is.nan(GPOA)] <- 0
  
  for(i in 1:numCompanies){
    safety[i] <- GPOA[i] + ROE[i] + ROA[i] + CFOA[i] + GMAR[i] + ACC[i]
  }
  scale(safety) 
}