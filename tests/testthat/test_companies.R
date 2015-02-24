test_that("tickers and names same in companies dataset and get_companies function",{
  data(companies)
  temp <- qmj::get_companies()
  expect_that(length(intersect(companies$ticker,temp$ticker)),equals(length(temp$ticker)))
  expect_that(length(intersect(companies$name,temp$name)),equals(length(temp$name)))
}) 