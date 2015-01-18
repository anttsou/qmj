#' collectmarketsafety
#'
#' Given a list of companies (names and tickers), a balance sheet, a cash flow statement,
#' and an income statement, calculates BAB, IVOL, LEV, O, Z, and EVOL.
#' and determines the z-score of overall profitability based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param x A dataframe of company names and tickers.
#' @param BS A dataframe containing balance sheet information for every company.
#' @param CF A dataframe containing cash flow information for every company.
#' @param IS A dataframe containing income statement information for every company.
#' @export

collectmarketsafety <- function(x, BS, CF, IS){
  # CollectMarketSafety collects data on overall safety
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
  benchmarkReturns <- read.csv("data/GSPC.csv", sep=',', row.names=1)
  
  for(i in 1:numCompanies){
    cBS <- BS[,(4*i)-2]
    cCF <- CF[,(4*i)-2]
    cIS <- IS[,(4*i)-2]
    returnsFile <- paste(x$tickers[i], ".csv", sep='')
    companyFile <- tryCatch(
      read.csv(returnsFile, sep=',', row.names=1),
      error=function(e) e
    )
    if(!inherits(companyFile, "error")){
    #BAB
    BAB[i] <- PerformanceAnalytics::CAPM.beta(companyFile[,2], benchmarkReturns)
    }
  }
  
  #Scale converts the individual scores for these values into z-scores.
  BAB <- scale(BAB)
  
  BAB[is.nan(BAB)] <- 0
  
  for(i in 1:numCompanies){
    safety[i] <- BAB[i]
  }
  scale(safety)
}