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
    cBS <- BS[,(4*i)-2]
    cBSm1y <- BS[,(4*i)-1]
    cBSm2y <- BS[,(4*i)]
    cBSm3y <- BS[,(4*i)+1]
    cCF <- CF[,(4*i)-2]
    cCFm1y <- CF[,(4*i)-1]
    cCFm2y <- CF[,(4*i)]
    cCFm3y <- CF[,(4*i)+1]
    cIS <- IS[,(4*i)-2]
    cISm1y <- IS[,(4*i)-1]
    cISm2y <- IS[,(4*i)]
    cISm3y <- IS[,(4*i)+1]
    
    #EISS
    # Issuance (retirement) of stock, net - CF 13
    ##Total number of Shares - BS 42
    EISS[i] <- -log(cBS[42]/cBSm1y[42])
    #DISS
    # Issuance (retirement) of debt, net - CF 14
    #Total debt - BS 27
    DISS[i] <- -log(cBS[27]/cBSm1y[27])
    
    #NPOP
    # (Net income - changes in book equity) / (total profits over the past 5 years)
    # Net income - CF 1 
    # Total equity - BS 39
    # Gross profits - IS 5
    totalNetPayouts <- (cCF[1] - cBS[39]) + (cCFm1y[1] - cBSm1y[39]) + (cCFm2y[1] - cBSm2y[39]) + (cCFm3y[1] - cBSm3y[39]) 
    totalProfits <- cIS[5] + cISm1y[5] + cISm2y[5] + cISm3y[5]
    NPOP[i] <- totalNetPayouts/totalProfits
  }
  
  #Scale converts the individual scores for these values into z-scores.
  EISS <- scale(EISS)
  DISS <- scale(DISS)
  NPOP <- scale(NPOP)
  
  EISS[is.nan(EISS)] <- 0
  DISS[is.nan(DISS)] <- 0
  NPOP[is.nan(NPOP)] <- 0
  
  for(i in 1:numCompanies){
    payouts[i] <- EISS[i] + DISS[i] + NPOP[i]
  }
  scale(payouts)
}