#'
#' In this file, we want to ensure that our functions for gathering
#' and cleaning financial data are both accurate and completed
#' to the fullest possible extent.
#' 
#' RAW DATA TESTS:
#' - Missing these_companies are missing specifically and only because quantmod
#'   does not provide data.
#' 
#' PROCESSED/TIDY DATA TESTS:
#' - For any given ticker, tidied financial information has at most 4 rows.
#'

context("Gathering Raw Financial Data Tests")

test_that("Missing these_companies are solely because quantmod provides no data", {
  these_companies <- companies_r3k16 %>% filter(ticker %in% c('AAPL', 'AMZN'))
  
  if (!reticulate::py_module_available("yfinance")) 
    testthat::skip()
  
  raw_fins <- qmj::get_info(these_companies)
  
  ## If no data at all was gathered, assume internet connection is bad.
  if(length(raw_fins[[1]]) == 0)
    testthat::skip()
  
  
  #' @describeIn Grabs the ticker from an element in one of the sublists
  #' of raw_fins
  grab_ticker <- function(financial_statement) {
    gsub('[0-9 ]', '', financial_statement)
  }
  
  ## It may be worthwhile to make this more robust in the future, in case we 
  ## ever change, for whatever reason, the order of raw_fins.
  cf_tickers <- sapply(raw_fins[[1]], colnames)
  cf_tickers <- sapply(cf_tickers, grab_ticker)
  cf_tickers <- unique(cf_tickers)
  
  missing_tickers <- these_companies$ticker[! these_companies$ticker %in% cf_tickers]
  
  #' @describeIn Check to ensure that quantmod produces no data for missing
  #' these_companies.
  download_check <- function(ticker){
    fin_data <- tryCatch(quantmod::getFinancials(i, auto.assign = FALSE), error = function(e) e)
    
    expect_true(inherits(fin_data, "error"), TRUE,
                label=paste0(ticker, " financial information is missing due to download error or lack of data"))
  }
  
  lapply(missing_tickers, FUN=download_check)
  
})

#################################################
##
## Tests related to data processing.
##
#################################################

context("Processing/Tidying Financial Data Tests")

test_that("Every ticker appears in the tidied financial data set at most four times", {
  these_companies <- companies_r3k16 %>% filter(ticker %in% c('AAPL', 'AMZN'))
  
  if (!reticulate::py_module_available("yfinance")) 
    testthat::skip()
  
  raw_fins <- qmj::get_info(these_companies)
  
  ## If no data at all was gathered, assume internet connection is bad.
  if(length(raw_fins[[1]]) == 0)
    testthat::skip();

  fins <- tidyinfo(raw_fins)
  
  occurrences <- table(fins$ticker)
  apply(occurrences, MARGIN=1, FUN=function(ticker_occurrences){expect_true(ticker_occurrences <= 4)})
  
})