#' collectmarketprofitability
#'
#' Given a list of companies (names and tickers), a balance sheet, a cash flow statement,
#' and an income statement, calculates GPOA, ROE, ROA, CFOA, GMAR, ACC,
#' and determines the z-score of overall profitability based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param x A dataframe of company names and tickers.
#' @param BS A dataframe containing balance sheet information for every company.
#' @param CF A dataframe containing cash flow information for every company.
#' @param IS A dataframe containing income statement information for every company.
#' @export

#use sapply to make columns numeric
collectmarketprofitability <- function(x, BS, CF, IS){
  # CollectMarketProfitability collects data on overall profitability
  ## In the market for individual companies for later processing.
  ## x is the list of companies to be processed. BS, CF, IS are financial statements.
  numCompanies <- length(x$tickers)
  profitability <- rep(0, numCompanies)
  GPOA <- rep(0, numCompanies)
  ROE <- rep(0, numCompanies)
  ROA <- rep(0, numCompanies)
  CFOA <- rep(0, numCompanies)
  GMAR <- rep(0, numCompanies)
  ACC <- rep(0, numCompanies)
  BS[is.na(BS)] <- 0
  IS[is.na(IS)] <- 0
  CF[is.na(CF)] <- 0
  for(i in 1:numCompanies) {
    cBS <- subset(BS,ticker == as.character(x$tickers[i]))
    cIS <- subset(IS,ticker == as.character(x$tickers[i]))
    cCF <- subset(CF,ticker == as.character(x$tickers[i]))
    
    if(nrow(cBS) > 0 && nrow(cIS) > 0 && nrow(cCF) > 0) {
      #GPOA = (revenue - cost of goods sold)/(total assets)
      #?#GROSS PROFITS OVER TOTAL ASSETS. THIS CAN BE EASILY FOUND.
      #Cost of goods sold = Beginning Inventory + Inventory Purchases - End Inventory
      ##Gross profit - Income statement
      ##Total assets - in balance sheet.
      # gpoa = gross profits/total assets    
      GPOA[i] = as.numeric(cIS$GPROF[1])/as.numeric(cBS$TA[1])
      #ROE
      # Net income /book equity
      # Net income - Cash flow
      #?#Book equity = Total equity (BS) 
      # 1)book equity = total liabilities and shareholder's equity - total liabilities - preferred stock
      # 2)book equity = total assets - (total liabilities + minority interest) - preferred stock
      ROE[i] = as.numeric(cIS$NI[1])/(as.numeric(cBS$TLSE[1]) - 
                                      as.numeric(cBS$TL[1]) - 
                                     (as.numeric(cBS$RPS[1]) + as.numeric(cBS$NRPS[1])))
      #ROA
      #Net income / Total assets
      # Net income - CF
      # Total assets - BS
      ROA[i] = as.numeric(cCF$NI[1])/as.numeric(cBS$TA[1])
      
      #CFOA
      #(net income + depreciation - (change in working capital) - capital expenditures)/(total assets)
      # Net income - CF
      # Depreciation - CF
      # Change in working capital - CF
      # Capital Expenditures - CF
      # Total assets - BS
      CFOA[i] = (as.numeric(cCF$NI[1]) + as.numeric(cCF$DP[1]) - as.numeric(cCF$CWC[1]) - as.numeric(cCF$CX[1]))/
                 as.numeric(cBS$TA[1])
      
      #GMAR
      # (Revenue - costs of goods sold)/(total sales)
      # = Gross profit/(total sales)
      #?# Using different equation:
      # Gross profit/ (Total revenue)
      #Gross profit - IS
      #Total Revenue - IS
      GMAR[i] = as.numeric(cIS$GPROF[1])/as.numeric(cIS$TREV[1])
      
      #ACC
      # (depreciation - changes in working capital)/(total assets)
      #*# Going from equation they show. Slight difference from their own
      ## words.
      # Depreciation - CF
      # Changes in working capital - CF
      # Total assets - BS 
      ACC[i] = (as.numeric(cCF$DP[1]) - as.numeric(cCF$CWC[1]))/as.numeric(cBS$TA[1])
    }
  }
  
  GPOA[is.nan(GPOA)] <- 0
  ROE[is.nan(ROE)] <- 0
  ROA[is.nan(ROA)] <- 0
  CFOA[is.nan(CFOA)] <- 0
  GMAR[is.nan(GMAR)] <- 0
  ACC[is.nan(ACC)] <- 0
  
  GPOA[is.infinite(GPOA)] <- 0
  ROE[is.infinite(ROE)] <- 0
  ROA[is.infinite(ROA)] <- 0
  CFOA[is.infinite(CFOA)] <- 0
  GMAR[is.infinite(GMAR)] <- 0
  ACC[is.infinite(ACC)] <- 0
  
  #Scale converts the individual scores for these values into z-scores.
  GPOA <- scale(GPOA)
  ROE <- scale(ROE)
  ROA <- scale(ROA)
  CFOA <- scale(CFOA)
  GMAR <- scale(GMAR)
  ACC <- scale(ACC)
  
  for(i in 1:numCompanies){
    profitability[i] <- GPOA[i] + ROE[i] + ROA[i] + CFOA[i] + GMAR[i] + ACC[i]
  }
  scale(profitability)
  #data.frame(x$names, x$tickers, profitability, GPOA, ROE, ROA, CFOA, GMAR, ACC)
}