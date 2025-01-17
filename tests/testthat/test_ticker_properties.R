context("Ticker properties")

test_that("tickers and names same in companies dataset and get_companies function",{
  these_companies <- companies_r3k16[4:6,]
  temp <- get_companies()[4:6,]
  expect_equal(length(intersect(these_companies$ticker,temp$ticker)), length(temp$ticker))
  expect_equal(length(intersect(these_companies$name,temp$name)), length(temp$name))
}) 

test_that("there are no duplicate tickers",{
    these_companies <- companies_r3k16[4:6,]
    expect_equal(length(these_companies$ticker[duplicated(these_companies$ticker)]), 0)
    expect_equal(length(quality_r3k16$ticker[duplicated(quality_r3k16$ticker)]), 0)
  })

test_that("tickers identical in company and quality",{
  these_companies <- companies_r3k16[4:6,]
  temp_profitability <- market_profitability(these_companies,financials_r3k16)
  temp_growth <- market_growth(these_companies,financials_r3k16)
  temp_safety <- market_safety(these_companies,financials_r3k16, prices_r3k16)
  temp_payouts <- market_payouts(these_companies,financials_r3k16)
  temp_quality <- market_data(these_companies, financials_r3k16, prices_r3k16)
  expect_equal(length(intersect(these_companies$ticker,temp_quality$ticker)), length(these_companies$ticker))
  expect_equal(length(intersect(these_companies$ticker,temp_profitability$ticker)), length(these_companies$ticker))
  expect_equal(length(intersect(these_companies$ticker,temp_growth$ticker)), length(these_companies$ticker))
  expect_equal(length(intersect(these_companies$ticker,temp_safety$ticker)), length(these_companies$ticker))
  expect_equal(length(intersect(these_companies$ticker,temp_payouts$ticker)), length(these_companies$ticker))
}) 

test_that("these_companies list with no ticker column sparks error", {
  x <- data.frame(name = "test")
  expect_error(get_info(x),"parameter requires a ticker column.")
  expect_error(get_prices(x),"parameter requires a ticker column.")
  expect_error(market_data(x,financials_r3k16,prices_r3k16),"first parameter requires a ticker column.")
  expect_error(market_profitability(x,financials_r3k16),"first parameter requires a ticker column.")
  expect_error(market_growth(x,financials_r3k16),"first parameter requires a ticker column.")
  expect_error(market_safety(x,financials_r3k16,prices_r3k16),"first parameter requires a ticker column.")
  expect_error(market_payouts(x,financials_r3k16),"first parameter requires a ticker column.")
})
