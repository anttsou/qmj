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
  for(i in 1:numCompanies){
    print(i)
    cBS <- BS[,(4*i)-2]
    cCF <- CF[,(4*i)-2]
    cIS <- IS[,(4*i)-2]
    #GPOA = (revenue - cost of goods sold)/(total assets)
    #?#GROSS PROFITS OVER TOTAL ASSETS. THIS CAN BE EASILY FOUND.
    #Cost of goods sold = Beginning Inventory + Inventory Purchases - End Inventory
    ##Gross profit - Income statement
    ##Total assets - in balance sheet.
    GPOA[i] = cIS[5]/cBS[17]
    #ROE
    # Net income /book equity
    # Net income - Cash flow
    #?#Book equity = Total equity (BS) 
    ROE[i] = cCF[1]/cBS[39]
    #ROA
    #Net income / Total assets
    # Net income - CF
    # Total assets - BS
    ROA[i] = cCF[1]/cBS[17]
    
    #CFOA
    #(net income + depreciation - (change in working capital) - capital expenditures)/(total assets)
    # Net income - CF
    # Depreciation - IS
    # Change in working capital - CF
    # Capital Expenditures - CF
    # Total assets - BS
    CFOA[i] = (cCF[1] + cIS[8] - cCF[6] - cCF[8])/cBS[17]
    
    #GMAR
    # (Revenue - costs of goods sold)/(total sales)
    # = Gross profit/(total sales)
    #?# Using different equation:
    # Gross profit/ (Total revenue)
    #Gross profit - IS
    #Total Revenue - IS
    GMAR[i] = cIS[5]/cIS[3]
    
    #ACC
    # (depreciation - changes in working capital)/(total assets)
    #*# Going from equation they show. Slight difference from their own
    ## words.
    # Depreciation - CF
    # Changes in working capital - CF
    # Total assets - BS 
    ACC[i] = (cIS[8] - cCF[6])/cBS[17]
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
    profitability[i] <- GPOA[i] + ROE[i] + ROA[i] + CFOA[i] + GMAR[i] + ACC[i]
  }
  scale(profitability) 
}