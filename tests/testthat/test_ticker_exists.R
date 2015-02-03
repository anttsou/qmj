test_that("companies list has ticker column", {
  x <- data.frame(name = "test")
  data(financials)
  data(extrafin)
  data(daily)
  expect_warning(market_data(x,financials,extrafin,daily),"companies parameter requires ticker column.")
})