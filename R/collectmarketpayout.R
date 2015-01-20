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
    readattempt = tryCatch({
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
    EISS[i] <- -log(cBS[42]/cBSm1y[42])
    #DISS
    # Issuance (retirement) of debt, net - CF 15
    #Total debt - BS 28
    DISS[i] <- -log(cBS[27]/cBSm1y[27])
    
    #NPOP
    # (Net income - changes in book equity) / (total profits over the past 5 years)
    # Net income - CF 2
    # Total equity - BS 40
    # Gross profits - IS 6
    totalNetPayouts <- (cCF[1] - cBS[39]) + (cCFm1y[1] - cBSm1y[39]) + (cCFm2y[1] - cBSm2y[39]) + (cCFm3y[1] - cBSm3y[39]) 
    totalProfits <- cIS[5] + cISm1y[5] + cISm2y[5] + cISm3y[5]
    NPOP[i] <- totalNetPayouts/totalProfits
    }, error = function(e){
      EISS[i] <- NA
      DISS[i] <- NA
      NPOP[i] <- NA
    })
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