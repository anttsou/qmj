test_that("ticker is valid", {
  x <- data.frame(name = "test", ticker = "TICK9")
  expect_warning(qmj::get_info(x))
  expect_warning(qmj::get_prices(x))
})