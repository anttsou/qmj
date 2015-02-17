test_that("there are no duplicate tickers",{
    data(companies)
    data(quality)
    expect_that(length(companies$ticker[duplicated(companies$ticker)]),equals(0))
    expect_that(length(quality$ticker[duplicated(quality$ticker)]),equals(0))
  })