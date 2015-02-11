test_that("calculation returns non-NA value", {
  data(companies)
  data(financials)
  data(prices)
  temp <- companies
  temp$name[5] <- "test"
  temp$ticker[5] <- "TICK9"
  expect_warning(qmj::market_safety(temp,financials,prices))
})