#' collectmarketpayout
#'
#' Given a list of companies (names and tickers), a balance sheet, a cash flow statement,
#' and an income statement, calculates EISS, DISS, NPOP
#' and determines the z-score of overall payout based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A3-4.
#' @param x A dataframe of company names and tickers.
#' @param BS A dataframe containing balance sheet information for every company.
#' @param CF A dataframe containing cash flow information for every company.
#' @param IS A dataframe containing income statement information for every company.
#' @export

collectmarketpayout <- function(x, BS, CF, IS){
  # CollectMarketPayout collects data on overall payouts
  ## In the market for individual companies for later processing.
  ## x is the list of companies to be processed. BS, CF, IS are financial statements.
  numCompanies <- length(x$tickers)
  payouts <- rep(0, numCompanies)
  EISS <- rep(0, numCompanies)
  DISS <- rep(0, numCompanies)
  NPOP <- rep(0, numCompanies)
  for(i in 1:numCompanies){
    cBS <- BS[[i]]
    cBS[is.na(cBS)] <- 0
    cBS <- data.frame(cBS)
    cBSm1y <- cBS[,2]
    cBSm2y <- cBS[,3]
    cBSm3y <- cBS[,4]
    cBS <- cBS[,1]
    
    cCF <- CF[[i]]
    cCF[is.na(cCF)] <- 0
    cCF <- data.frame(cCF)
    cCFm1y <- cCF[,2]
    cCFm2y <- cCF[,3]
    cCFm3y <- cCF[,4]
    cCF <- cCF[,1]
    
    cIS <- IS[[i]]
    cIS[is.na(cCF)] <- 0
    cIS <- data.frame(cIS)
    cISm1y <- cIS[,2]
    cISm2y <- cIS[,3]
    cISm3y <- cIS[,4]
    cIS <- cIS[,1]
    
    #EISS
    # Issuance (retirement) of stock, net - CF 14
    ##Total number of Shares - BS 43
    EISS[i] <- -log(cBS[43]/cBSm1y[43])
    #DISS
    # Issuance (retirement) of debt, net - CF 15
    #Total debt - BS 28
    DISS[i] <- -log(cBS[28]/cBSm1y[28])
    
    #NPOP
    # (Net income - changes in book equity) / (total profits over the past 5 years)
    # Net income - CF 2
    # Total equity - BS 40
    # Gross profits - IS 6
    totalNetPayouts <- (cCF[2] - cBS[40]) + (cCFm1y[2] - cBSm1y[40]) + (cCFm2y[2] - cBSm2y[40]) + (cCFm3y[2] - cBSm3y[40]) 
    totalProfits <- cIS[6] + cISm1y[6] + cISm2y[6] + cISm3y[6]
    NPOP[i] <- totalNetPayouts/totalProfits
  }
  
  #Scale converts the individual scores for these values into z-scores.
  EISS <- scale(EISS)
  DISS <- scale(DISS)
  NPOP <- scale(NPOP)
  
  EISS[is.na(EISS)] <- 0
  DISS[is.na(DISS)] <- 0
  NPOP[is.na(NPOP)] <- 0
  
  for(i in 1:numCompanies){
    payouts[i] <- EISS[i] + DISS[i] + NPOP[i]
  }
  scale(payouts)
}