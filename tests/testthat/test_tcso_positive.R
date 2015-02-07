test_that("tcso is positive", {
  data(companies)
  data(financials)
  data(prices)
  temp <- financials
  temp <- rbind(temp,rep(-1,length(colnames(temp))))
  expect_error(qmj::market_data(companies,temp,prices))
  expect_error(qmj::market_profitability(companies,temp))
  expect_error(qmj::market_growth(companies,temp))
  expect_error(qmj::market_safety(companies,temp,prices))
  expect_error(qmj::market_payout(companies,temp))
})