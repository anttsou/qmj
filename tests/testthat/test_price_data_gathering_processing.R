#'
#' In this file we're concerned with testing the accuracy of our functions for retrieving
#' price data, as well as to make sure that the raw data is correctly mapped onto
#' the processed, tidy data.
#' 
#' RAW DATA TESTS:
#' - Every ticker in the raw price data is unique with a predicted number of columns.
#' - Every ticker for which we grab data, has some data.
#' - Any company for which we have no raw data, it is specifically because quantmod provides no data.
#' 
#' PROCESSED DATA TESTs:
#'

context("Price Data Gathering and Processing Tests")

companies <- qmjdata::companies
raw_prices <- qmj::get_prices(companies)

test_that("Every ticker in the raw price data is unique with a predicted number of columns", {
  ## Grab column names and remove everything but the ticker.
  tickers <- colnames(raw_prices)
  tickers <- gsub('\\.([^.]*$)(.*)', '', tickers)  # Remove everything after the last '.' to get rid of '.____' suffixes.
  tickers <- tickers[tickers != 'GSPC' & tickers != 'pret']  # We're not interested in either the GSPC or pret columns.
  occurrences <- table(tickers)
  
  ## After removing pret, every ticker should only appear 5 times.
  apply(occurrences, MARGIN=1, FUN=function(ticker_occurrences){expect_equal(ticker_occurrences, 5)})
  
  ## We use length(table) as we presume that price data for some tickers were not found.
  num_columns_used_by_GSPC <- 7  # GSPC.Open, GSPC.High, GSPC.Low, GSPC.Close, GSPC.Volume, GSPC.Adjusted, pret
  num_of_columns_per_ticker <- 6  # TICK.Open, TICK.High, TICK.Low, TICK.Close, TICK.Volume, pret.num
  
  ## Make sure we covered all columns in the raw price data.
  expected_num_columns = (num_columns_used_by_GSPC) + (num_of_columns_per_ticker * length(table))
  expect_equal(expected_num_columns, ncol(raw_prices))
})



test_that("Missing Information is Solely Due To Quantmod Finding No Data", {
  ## First get all companies for which we do have data.
  tickers <- colnames(raw_prices)
  tickers <- gsub('\\.([^.]*$)(.*)', '', tickers)  # Remove everything after the last '.' to get rid of '.____' suffixes.
  tickers <- tickers[tickers != 'GSPC' & tickers != 'pret']  # We're not interested in either the GSPC or pret columns.
  tickers <- unique(tickers)
  
  ## Then determine which companies have no price data
  missing_tickers <- companies$ticker[!companies$ticker %in% tickers]
  
  ## We only want stock data from the past 2 years
  start <- as.POSIXlt(Sys.Date())
  start$year <- start$year - 2
  start <- as.Date(start)
  
  #' @describeIn For each missing ticker, ensure that quantmod produces either an error or an empty
  #' xts object.
  download_check <- function(ticker){
     price_data <- tryCatch(
      quantmod::getSymbols(companyTicker, src="google", auto.assign=FALSE, from=start),
      error = function(e) e
      )
     
     ## Check to make sure that either price_data is an error, or that it has a single
     ## row of NA's.
     expect_equal(inherits(price_data, "error") || (length(price_data[,1])==1 && sum(!is.na(price_data))==0), TRUE,
                  label=paste0(ticker, " price information missing due to download error or lack of data"))
  }
  
  lapply(missing_tickers, FUN=download_check)
})