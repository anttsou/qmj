test_that("ticker is valid", {
  x <- data.frame(name = "test", ticker = "TICK9")
  expect_warning(qmj::get_prices(x),"Some ticker(s) do not have daily data. 
                 NAs have been introduced as a result.")
})