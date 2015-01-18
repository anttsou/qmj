#' collectmarketgrowth
#'
#' Given a list of companies (names and tickers), a balance sheet, a cash flow statement,
#' and an income statement, calculates GPOA, ROE, ROA, CFOA, GMAR, ACC
#' and determines the z-score of overall growth based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param x A dataframe of company names and tickers.
#' @param BS A dataframe containing balance sheet information for every company.
#' @param CF A dataframe containing cash flow information for every company.
#' @param IS A dataframe containing income statement information for every company.
#' @export

collectmarketgrowth <- function(x, BS, CF, IS){
  numCompanies <- length(x$tickers)
  
  growth <- rep(0, numCompanies)
  GPOA <- rep(0, numCompanies)
  ROE <- rep(0, numCompanies)
  ROA <- rep(0, numCompanies)
  CFOA <- rep(0, numCompanies)
  GMAR <- rep(0, numCompanies)
  ACC <- rep(0, numCompanies)
  
  for(i in 1:numCompanies){
    cBS <- BS[,(4*i)-2]
    cBSm3y <- BS[,(4*i)+1]
    cCF <- CF[,(4*i)-2]
    cCFm3y <- CF[,(4*i)+1]
    cIS <- IS[,(4*i)-2]
    cISm3y <- IS[,(4*i)+1]
    ###GROWTH
    #GPOA
    #(5 year change in gross profits)/Total assets
    #GP - IS 6
    #Total assets - BS 18
    GPOA[i] <- (cIS[6] - cISm3y[6])/(cBSm3y[18])
    
    #(5 year change in Net income)/book equity
    #Net income - CF 2
    # Book equity (Total equity) - BS 40
    ROE[i] <- (cCF[2] - cCFm3y[2])/(cBSm3y[40])
    
    #(5 year change in net income)/total assets
    # Net income - CF 2
    # Total assets - BS 18
    ROA[i] <- (cCF[2] - cCFm3y[2])/(cBSm3y[18])
    #(5 year change in cash flow over assets)
    #Change in cash flow = net income + depreciation - change in working capital - capital expenditure
    # IB (Net income) - CF 2
    # Depreciation - CF 3
    # Change in working capital - CF 7
    # Capital expenditure - CF 9
    #Total assets - BS 18
    changeCF1 <- (cCF[2] + cCF[3] - cCF[7] - cCF[9])
    changeCF2 <- (cCFm3y[2] + cCFm3y[3] - cCFm3y[7] - cCFm3y[9])
    CFOA[i] <- (changeCF1 - changeCF2)/(cBSm3y[18])
    #(5 year change in gross profit)/(total sales)
    # GP - IS 6
    # Total sales (total revenues) - IS 4
    GMAR[i] <- (cIS[6] - cISm3y[6])/(cISm3y[4])
    
    #(5 year change in (low) accruals)/total assets
    # Low accruals = DP - (change in WC)
    # DP - CF 3
    #Change in working capital - CF 7
    #Total assets - BS 18
    accrual1 <- cCF[3] - cCF[7]
    accrual2 <- cCFm3y[3] - cCFm3y[7]
    ACC[i] <- (accrual1 - accrual2)/(cBSm3y[18])
  }
  
  #Scale converts the individual scores for these values into z-scores.
  GPOA <- scale(GPOA)
  ROE <- scale(ROE)
  ROA <- scale(ROA)
  CFOA <- scale(CFOA)
  GMAR <- scale(GMAR)
  ACC <- scale(ACC)
  
  GPOA[is.nan(GPOA)] <- 0
  ROE[is.nan(ROE)] <- 0
  ROA[is.nan(ROA)] <- 0
  CFOA[is.nan(CFOA)] <- 0
  GMAR[is.nan(GMAR)] <- 0
  ACC[is.nan(ACC)] <- 0
  
  for(i in 1:numCompanies){
    growth[i] <- GPOA[i] + ROE[i] + ROA[i] + CFOA[i] + GMAR[i] + ACC[i]
  }
  scale(growth)
}