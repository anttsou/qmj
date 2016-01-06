#'
#' In this file we're concerned with testing the accuracy of our functions for retrieving
#' price data, as well as to make sure that the raw data is correctly mapped onto
#' the processed, tidy data.
#' 
#' RAW DATA TESTS:
#' - Every ticker in the raw price data is unique with a predicted number of columns.
#' - Every ticker for which we successfully grabbed data, has some data
#' - Missing Companies is Solely Due To Quantmod Finding No Data
#' 
#' PROCESSED DATA TESTs:
#' - Raw data matches processed data for any given company
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

test_that("Every ticker for which we successfully grabbed data, has some data"{
  #' @describeIn Check to make sure every column has at least one non-na entry
  col_check <- function(column) {
    expect_true(sum(!is.na(column)) >= 1, label="grabbed data contains some data")
  }
  
  ## For completeness, we check all columns to ensure they have at least one non-NA entry.
  apply(raw_prices, MARGIN=2, FUN=col_check)
})

test_that("Missing Companies is Solely Due To Quantmod Finding No Data", {
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

test_that("Raw data matches processed data for any given company", {
  ## Grab all .Close and pret columns from the raw data.
  close_indices <- grep('(.Close)', colnames(raw_prices))
  pret_indices <- grep('(pret)', colnames(raw_prices))
  
  subset_close <- raw_prices[, close_indices]
  subset_pret <- raw_prices[,pret_indices]
  column_names <- colnames(subset_close)
  
  ## A quick sanity check. Make sure subset_close and subset_pret have
  ## equal numbers of columns.
  expect_equal(ncol(subset_close), ncol(subset_pret), label='col numbers of close')
  
  ## Get the tidied prices for use in the compare_data function.
  prices <- tidy_prices(raw_prices)
  
  #' @describeIn Compares raw close and raw pret with their
  #' processed counterparts.
  compare_row <- function(close, pret, tidyclose, tidypret) {
    expect_equal(close, tidyclose, label='closing values match')
    expect_equal(pret, tidypret, label='pret values match')
  }
  
  #' @describeIn Accept raw columns containing closing prices and pret,
  #' find the relevant chunk of processed data, and compare the values.
  compare_transcription <- function(colname, close, pret) {
    # Grab the ticker by removing everything past, and including, the last '.'
    ticker <- gsub('\\.([^.]*$)(.*)', '', colname)
    
    price_subset <- prices[prices$ticker==ticker,]
    
    ## Compare values with their counterparts row-by-row.
    mapply(compare_row, close, pret, price_subset$close, price_subset$pret)
  }
  
  ## Now, iterate through both subsetted columns and compare data with the
  ## processed prices data. subset_close and subset_pret have equal col
  ## numbers from an earlier check.
  for(i in 1:ncol(subset_close)) {
    compare_transcription(column_names[i], subset_close[,i], subset_pret[,i])
  }
})