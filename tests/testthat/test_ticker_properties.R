context("Ticker properties")

companies <- qmjdata::companies[4:6,]
financials <- qmjdata::financials
prices <- qmjdata::prices
quality <- qmjdata::quality

test_that("tickers and names same in companies dataset and get_companies function",{
  temp <- get_companies()[4:6,]
  expect_equal(length(intersect(companies$ticker,temp$ticker)), length(temp$ticker))
  expect_equal(length(intersect(companies$name,temp$name)), length(temp$name))
}) 

test_that("there are no duplicate tickers",{
    expect_equal(length(companies$ticker[duplicated(companies$ticker)]), 0)
    expect_equal(length(quality$ticker[duplicated(quality$ticker)]), 0)
  })

test_that("tickers identical in company and quality",{
  temp_profitability <- market_profitability(companies,financials)
  temp_growth <- market_growth(companies,financials)
  temp_safety <- market_safety(companies,financials, prices)
  temp_payouts <- market_payouts(companies,financials)
  temp_quality <- market_data(companies, financials, prices)
  expect_equal(length(intersect(companies$ticker,temp_quality$ticker)), length(companies$ticker))
  expect_equal(length(intersect(companies$ticker,temp_profitability$ticker)), length(companies$ticker))
  expect_equal(length(intersect(companies$ticker,temp_growth$ticker)), length(companies$ticker))
  expect_equal(length(intersect(companies$ticker,temp_safety$ticker)), length(companies$ticker))
  expect_equal(length(intersect(companies$ticker,temp_payouts$ticker)), length(companies$ticker))
}) 

test_that("companies list with no ticker column sparks error", {
  x <- data.frame(name = "test")
  expect_error(get_info(x),"parameter requires a ticker column.")
  expect_error(get_prices(x),"parameter requires a ticker column.")
  expect_error(market_data(x,financials,prices),"first parameter requires a ticker column.")
  expect_error(market_profitability(x,financials),"first parameter requires a ticker column.")
  expect_error(market_growth(x,financials),"first parameter requires a ticker column.")
  expect_error(market_safety(x,financials,prices),"first parameter requires a ticker column.")
  expect_error(market_payouts(x,financials),"first parameter requires a ticker column.")
})
