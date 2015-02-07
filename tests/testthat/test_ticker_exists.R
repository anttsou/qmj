test_that("companies list has ticker column", {
  x <- data.frame(name = "test")
  data(financials)
  data(prices)
  expect_error(qmj::market_data(x,financials,prices))
})