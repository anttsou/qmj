#' Saves a data frame of companies to the package data set.
#'
#' Given a list of companies with "tickers" column, stores data as an RData file in the /data folder of qmj.
#' The list of companies you use MUST have a variable name of "companies".
#' @param companies A dataframe a "tickers" column.
#' @export

save_companies <- function(companies){
  filepath <- system.file("data", package="qmj")
  filepath <- paste(filepath, "/companies.RData", sep='')
  save(companies,filepath)
}