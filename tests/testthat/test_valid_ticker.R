test_that("ticker is valid", {
  x <- data.frame(name = "test", ticker = "TICK9")
  expect_warning(qmj::get_info(x),"No financials for TICK9")
  expect_warning(qmj::get_prices(x),"No daily data for TICK9")
})