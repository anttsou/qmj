#' Gets raw balancesheets.
#'
#' Retrieves raw data for balancesheets. Returns a large matrix, where
#'  individual elements correspond to a company's balancesheet statements
#'  over four years.
#' @export

get_raw_balancesheets <- function(){
  filepath <- system.file("extdata", package="qmj")
  filepath <- paste(filepath, "/balancesheets.RData", sep='')
  load(filepath)
  balancesheets
}