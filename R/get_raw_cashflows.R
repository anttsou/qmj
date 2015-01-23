#' Gets raw cash flow statements.
#'
#' Retrieves raw data for cash flows. Returns a large matrix, where
#'  individual elements correspond to a company's cash flow statements
#'  over four years.
#' @export

get_raw_cashflows <- function(){
  filepath <- system.file("extdata", package="qmj")
  filepath <- paste(filepath, "/cashflows.RData", sep='')
  load(filepath)
  cashflows
}