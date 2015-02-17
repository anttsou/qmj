test_that("ticker is valid", {
  x <- data.frame(name = "test", ticker = "TICK9")
  expect_warning(qmj::get_info(x),"parameter requires a ticker column.")
  expect_warning(qmj::get_prices(x))
})