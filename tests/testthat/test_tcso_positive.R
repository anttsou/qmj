test_that("tcso is positive", {
  data(companies)
  data(financials)
  data(prices)
  temp <- financials
  temp <- rbind(temp,rep(-1,length(colnames(temp))))
  print(which(temp$TCSO < 0))
  qmj::market_data(companies,temp,prices)
  #expect_error(qmj::market_data(companies,temp,prices))
})