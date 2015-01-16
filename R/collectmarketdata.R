collectmarketdata <- function(){
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
  companies <- read.csv("data/companies.csv")
  numCompanies <- length(companies$tickers)
  BS <- read.csv("data/balancesheets.csv")
  CF <- read.csv("data/cashflows.csv")
  IS <- read.csv("data/incomestatements.csv")
  
  #What to do with missing data?
  # If we're missing a lot of data, then simply assigning 0's skews
  # the mean and SD. However, short term solution to getting a result.
  BS[is.na(BS)] <- 0
  CF[is.na(CF)] <- 0
  IS[is.na(IS)] <- 0
  
  profitability <- collectmarketprofitability(companies, BS, CF, IS)
  growth <- rep(0, numCompanies)
  safety <- rep(0, numCompanies)
  ###GROWTH
  #(5 year change in gross profits)/Total assets
  
  #(5 year change in Net income)/book equity
  
  #(5 year change in net income)/total assets
  
  #(5 year change in cash flow over assets)

  #(5 year change in gross profit)/(total sales)
  
  #(5 year change in (low) accruals)/total assets
  
  ###SAFETY
  #BAB
  # - market beta
  profitability
}