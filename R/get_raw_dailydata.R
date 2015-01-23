#' Gets raw daily data.
#'
#' Retrieves raw data for stock price returns and closing prices. Returns a data frame, where columns
#' are associated with a company's calculated price return and its recorded closing price for the date.
#' @export

get_raw_dailydata <- function(){
  filepath <- system.file("extdata", package="qmj")
  filepath <- paste(filepath, "/dailydata.RData", sep='')
  load(filepath)
  dailydata
}