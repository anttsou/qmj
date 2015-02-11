#' Gets raw financial statements from the extdata folder of the package.
#'
#' Retrieves raw data for financial statements as returned by the quantmod package.
#' Structured as a list of three elements, which each element contains either balance sheets,
#' cash flows, or income statements for all companies.
#' @export

get_raw_financials <- function(){
  filepath <- system.file("extdata", package="qmj")
  filepath <- paste(filepath, "/rawfinancials.RData", sep='')
  load(filepath)
  rawfinancials
}