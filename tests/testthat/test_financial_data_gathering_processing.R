#'
#' In this file, we want to ensure that our functions for gathering
#' and cleaning financial data are both accurate and completed
#' to the fullest possible extent.
#' 
#' RAW DATA TESTS:
#' - Missing companies are missing specifically and only because quantmod
#'   does not provide data
#' - 
#' 
#' PROCESSED/TIDY DATA TESTS:
#'

companies <- qmjdata::companies[1,]
raw_fins <- qmj::get_info(companies)

context("Gathering Raw Financial Data Tests")

test_that("Missing companies are solely because quantmod provides no data", {
  
  #' @describeIn Grabs the ticker from an element in one of the sublists
  #' of raw_fins
  #grab_ticker <- function(financial_statement) {
  #  gsub('[0-9 ]', '', financial_statement)
  #}
  
  ## It may be worthwhile to make this more robust in the future, in case we 
  ## ever change, for whatever reason, the order of raw_fins.
  ##cf_tickers <- sapply(raw_fins[[1]], grab_ticker)
  
})


context("Processing/Tidying Financial Data Tests")

test_that("Every ticker appears in the tidied financial data set at most four times", {

  #fins <- tidyinfo(raw_fins)
  
  #occurrences <- table(fins$ticker)
  #apply(occurrences, MARGIN=1, FUN=function(ticker_occurrences){is_true(ticker_occurrences <= 4)})
  
})