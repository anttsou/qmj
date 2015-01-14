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
  #GPOA = (revenue - cost of goods sold)/(total assets)
  #Cost of goods sold = Beginning Inventory + Inventory Purchases - End Inventory
  ##Total assets - in balance sheet.
  
}