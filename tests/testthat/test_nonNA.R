context("Beta and Idiosyncratic Volatility")

data(companies)
data(financials)
data(prices)

test_that("calculation returns non-NA value", {
  temp <- companies
  temp$name[5] <- "test"
  temp$ticker[5] <- "TICK9"
  expect_warning(market_safety(temp,financials,prices),"BAB for TICK9 generated NA")
  expect_warning(market_safety(temp,financials,prices),"IVOL for TICK9 generated NA")
})