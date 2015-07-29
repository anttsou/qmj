#' Gets the company names and tickers from the most recent 
#' Russell 3000 Index.
#' 
#' Reads in the contents of a text file created from the 
#' pdf of company names and tickers given by the Russell 
#' 3000 Index. As of now, there does not seem to be a very 
#' good way of converting from pdf to txt file in R, so 
#' the txt file is directly copied and pasted from the pdf 
#' and uses the following regex: [ ](?=[^ ]+$)
#' to create comma separated values. 
#' 
#' @examples
#' get_companies()
#' 
#' @importFrom dplyr arrange
#' @export

get_companies <- function() {
  filepath <- system.file("extdata", package="qmj")
  filepath <- paste0(filepath, "/companies.txt")
  companies <- read.csv(filepath, stringsAsFactors = FALSE)

  companies <- companies[,c("ticker","name")]
  companies <- dplyr::arrange(companies, ticker)
  companies
}