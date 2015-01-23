#' Gets raw income statements.
#'
#' Retrieves raw data for income statements. Returns a large matrix, where
#'  individual elements correspond to a company's income statements
#'  over four years.
#' @export

get_raw_incomestatements <- function(){
  filepath <- system.file("extdata", package="qmj")
  filepath <- paste(filepath, "/incomestatements.RData", sep='')
  load(filepath)
  incomestatements
}