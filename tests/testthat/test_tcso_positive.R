test_that("tcso is positive", {
  data(companies)
  data(financials)
  data(prices)
  temp <- financials
  temp <- rbind(temp,rep(-1,length(colnames(temp))))
  expect_error(qmj::market_data(companies,temp,prices),"Negative TCSO exists.")
  expect_error(qmj::market_profitability(companies,temp),"Negative TCSO exists.")
  expect_error(qmj::market_growth(companies,temp),"Negative TCSO exists.")
  expect_error(qmj::market_safety(companies,temp,prices),"Negative TCSO exists.")
  expect_error(qmj::market_payouts(companies,temp),"Negative TCSO exists.")
})