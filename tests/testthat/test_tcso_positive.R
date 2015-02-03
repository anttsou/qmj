test_that("tcso is positive", {
  data(companies)
  data(financials)
  data(extrafin)
  data(daily)
  temp <- financials
  temp <- rbind(temp,rep(-1,length(colnames(temp))))
  expect_warning(market_data(x,temp,extrafin,daily),"Negative TCSO exists.")
})