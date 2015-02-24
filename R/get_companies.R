#' Gets the company names and tickers from the most recent Russell 3000 Index.
#' 
#' Reads in the contents of a text file created from the pdf of company names 
#' and tickers given by the Russell 3000 Index.
#' 
#' @examples
#' get_companies()
#' @export
get_companies <- function() {
  filepath <- system.file("inst/doc",package="qmj")
  filepath <- paste(filepath, "/companies.txt",sep="")
  companies <- read.csv(filepath,stringsAsFactors=FALSE)
  companies
}