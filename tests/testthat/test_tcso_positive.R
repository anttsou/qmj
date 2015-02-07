test_that("tcso is positive", {
  data(companies)
  data(financials)
  data(prices)
  temp <- financials
  temp <- rbind(temp,rep(-1,length(colnames(temp))))
  expect_error(qmj::market_data(companies,temp,prices))
  expect_error(qmj::market_profitability(x,temp))
  expect_error(qmj::market_growth(x,temp))
  expect_error(qmj::market_safety(x,temp,prices))
  expect_error(qmj::market_payout(x,temp))
})