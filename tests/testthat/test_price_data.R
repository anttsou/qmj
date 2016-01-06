context("Price Data Gathering and Processing Tests")

companies <- qmjdata::companies
raw_prices <- qmj::get_prices(companies)

test_that("Every ticker in the raw price data is unique with a predicted number of columns", {
  ## Grab column names and remove everything but the ticker.
  cols <- colnames(raw_prices)
  cols <- gsub('\\.([^.]*$)(.*)', '', cols)  # Remove everything after the last '.' to get rid of '.____' suffixes.
  cols <- cols[cols != 'GSPC' & cols != 'pret']  # We're not interested in either the GSPC or pret columns.
  occurrences <- table(cols)
  
  ## After removing pret, every ticker should only occur 5 times.
  apply(occurrences, MARGIN=1, FUN=function(ticker_occurrences){expect_equal(ticker_occurrences, 5)})
  
  ## Make sure we covered all columns in the raw price data.
  ## We use length(table) as we presume that price data for some tickers were not found.
  num_columns_used_by_GSPC <- 7  # GSPC.Open, GSPC.High, GSPC.Low, GSPC.Close, GSPC.Volume, GSPC.Adjusted, pret
  num_of_columns_per_ticker <- 6  # TICK.Open, TICK.High, TICK.Low, TICK.Close, TICK.Volume, pret.num
  
  expected_num_columns = (num_columns_used_by_GSPC) + (num_of_columns_per_ticker * length(table))
  expect_equal(expected_num_columns, ncol(raw_prices))
})