#' Updates and tidies daily data.
#'
#' Takes in a data frame of companies (must include "tickers" column), reads in the daily data and stores that information
#' in the /extdata folder, then tidies that raw data to be saved as "tidydaily" in the /data folder.
#'
#' @param x A data frame of company names and tickers.
#' @export

update_dailydata <- function(x){
  destpath <- system.file("data", package="qmj")
  destpath <- paste(destpath, "/tidydaily.RData", sep='')
  filepath <- system.file("extdata", package="qmj")
  
  companypath <- paste(destpath, "/companies.RData", sep='')
  load(companypath)
  
  dailydata <- getdailydata(companies)
  tidydaily <- tidy_dailydata(dailydata)
  save(tidydaily, file=destpath)
}