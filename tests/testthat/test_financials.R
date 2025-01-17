context("Attributes for Financials")

test_that("tcso is positive", {
  temp <- financials_r3k16
  temp <- rbind(rep(-1, length(colnames(temp))), temp)
  expect_error(market_data(companies_r3k16,temp,prices),"Negative TCSO exists.")
  expect_error(market_profitability(companies_r3k16,temp),"Negative TCSO exists.")
  expect_error(market_growth(companies_r3k16,temp),"Negative TCSO exists.")
  expect_error(market_safety(companies_r3k16,temp,prices),"Negative TCSO exists.")
  expect_error(market_payouts(companies_r3k16,temp),"Negative TCSO exists.")
})