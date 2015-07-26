context("Attributes for Financials")

companies <- qmjdata::companies
financials <- qmjdata::financials
prices <- qmjdata::prices

test_that("tcso is positive", {
  temp <- financials
  temp <- rbind(temp,rep(-1,length(colnames(temp))))
  expect_error(market_data(companies,temp,prices),"Negative TCSO exists.")
  expect_error(market_profitability(companies,temp),"Negative TCSO exists.")
  expect_error(market_growth(companies,temp),"Negative TCSO exists.")
  expect_error(market_safety(companies,temp,prices),"Negative TCSO exists.")
  expect_error(market_payouts(companies,temp),"Negative TCSO exists.")
})