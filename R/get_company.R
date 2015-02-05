get_company <- function(companies,ticker,financials,prices) {
  sub.comp <- companies[companies$ticker==ticker,]
  profitability <- market_profitability(sub.comp,financials)
  growth <- market_growth(sub.comp,financials)
  safety <- market_safety(sub.comp,financials,prices)
  payouts <- market_payout(sub.comp,financials)
  quality <- profitability + growth + safety + payouts
  #company <- new("Company",...)
}