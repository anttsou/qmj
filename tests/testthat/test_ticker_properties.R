context("Ticker properties")

data(companies)
data(financials)
data(prices)
data(quality)

test_that("tickers and names same in companies dataset and get_companies function",{
  temp <- get_companies()
  expect_that(length(intersect(companies$ticker,temp$ticker)),equals(length(temp$ticker)))
  expect_that(length(intersect(companies$name,temp$name)),equals(length(temp$name)))
}) 

test_that("there are no duplicate tickers",{
    expect_that(length(companies$ticker[duplicated(companies$ticker)]),equals(0))
    expect_that(length(quality$ticker[duplicated(quality$ticker)]),equals(0))
  })

test_that("tickers identical in company and quality",{
  temp_profitability <- market_profitability(companies,financials)
  temp_growth <- market_growth(companies,financials)
  temp_safety <- market_safety(companies,financials,prices)
  temp_payouts <- market_payouts(companies,financials)
  expect_that(length(intersect(companies$ticker,quality$ticker)),equals(length(companies$ticker)))
  expect_that(length(intersect(companies$ticker,temp_profitability$ticker)),equals(length(companies$ticker)))
  expect_that(length(intersect(companies$ticker,temp_growth$ticker)),equals(length(companies$ticker)))
  expect_that(length(intersect(companies$ticker,temp_safety$ticker)),equals(length(companies$ticker)))
  expect_that(length(intersect(companies$ticker,temp_payouts$ticker)),equals(length(companies$ticker)))
}) 

test_that("companies list has ticker column", {
  x <- data.frame(name = "test")
  expect_error(get_info(x),"parameter requires a ticker column.")
  expect_error(get_prices(x),"parameter requires a ticker column.")
  expect_error(market_data(x,financials,prices),"first parameter requires a ticker column.")
  expect_error(market_profitability(x,financials),"first parameter requires a ticker column.")
  expect_error(market_growth(x,financials),"first parameter requires a ticker column.")
  expect_error(market_safety(x,financials,prices),"first parameter requires a ticker column.")
  expect_error(market_payouts(x,financials),"first parameter requires a ticker column.")
})

test_that("ticker is valid", {
  x <- data.frame(name = "test", ticker = "TICK9")
  expect_warning(get_info(x),"No financials for TICK9")
  expect_warning(get_prices(x),"No daily data for TICK9")
})