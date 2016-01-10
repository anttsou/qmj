context("Beta and Idiosyncratic Volatility")

companies <- qmjdata::companies
financials <- qmjdata::financials
prices <- qmjdata::prices

test_that("calculation returns non-NA value", {
  temp <- companies[1:2,]
  temp$name[1] <- "test"
  temp$ticker[1] <- "TICK9"
  expect_warning(market_safety(temp,financials,prices),"BAB for TICK9 generated NA")
  expect_warning(market_safety(temp,financials,prices),"IVOL for TICK9 generated NA")
})