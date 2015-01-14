collectmarketdata <- function(x){
  ##Collect market data focuses on collecting needed
  ##  means and sd's for use in other functions.
  
  #We need:
  # gross profits over assets (GPOA)
  # Return on equity (ROE)
  # return on assets (ROA)
  # Cash flow over assets (GPOA)
  # Gross margin (GMAR)
  # Fraction of earnings composed of cash
  #   i.e., low accruals, ACC
  
  numCompanies <- length(x$tickers)
  profitability <- rep(0, numCompanies)
  for(i in 1:numCompanies){
    #GPOA = (revenue - cost of goods sold)/(total assets)
    #?#GROSS PROFITS OVER TOTAL ASSETS. THIS CAN BE EASILY FOUND.
    #Cost of goods sold = Beginning Inventory + Inventory Purchases - End Inventory
    ##Gross profit - Income statement
    ##Total assets - in balance sheet.
    
    #ROE
    # Net income /book equity
    # Net income - Cash flow
    #?#Book equity = Total equity (BS) 
    
    #ROA
    #Net income / Total assets
    # Net income - CF
    # Total assets - BS
    
    #CFOA
    #(net income + depreciation - (change in working capital) - capital expenditures)/(total assets)
    # Net income - CF
    # Depreciation - IS
    # Change in working capital - CF
    # Capital Expenditures - CF
    # Total assets - BS
    
    #GMAR
    # (Revenue - costs of goods sold)/(total sales)
    # = Gross profit/(total sales)
    #?# Using different equation:
    # Gross profit/ (Total revenue)
    #Gross profit - IS
    #Total Revenue - IS
    
    #ACC
    # (depreciation - changes in working capital)/(total assets)
    #*# Going from equation they show. Slight difference from their own
    ## words.
    # Depreciation - CF
    # Changes in working capital - CF
    # Total assets - BS 
  }
}