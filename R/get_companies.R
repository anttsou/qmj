#' Parses a text file, copied and pasted directly from the 
#' Russell 3000 Index membership list, and returns a data 
#' frame of company names and tickers.
#' 
#' \code{get_companies} reads in the contents of a text file
#' created from the pdf of company names and tickers given by
#' the Russell 3000 Index.
#' 
#' The user must copy and paste the contents of the Russell
#' 3000 Index into a text file for this function to process the
#' data correctly. Simply select all of the component list and paste 
#' the contents into an empty document with
#' the .txt extension. The list may be found 
#' \href{https://www.russell.com/documents/indexes/membership/membership-russell-3000.pdf}{here}.
#' 
#' \code{get_companies} by default uses a text file created from
#' the Russell 3000 Index in the package.
#' 
#' @param filepath Specifies the filepath of the text file containing the company names and 
#' tickers of interest. May be either absolute or relative to working directory.
#' 
#' @examples
#' ## The function by default uses a text file created from the Russell 3000 Index.
#' 
#' get_companies()
#' 
#' ## If you wish to create your own text file of companies for get_companies to process,
#' ## create a text file containing each company on a separate line. Every word and ticker
#' ## must be capitalized, and the ticker must be the last word, separated by a space, on
#' ## each line.
#' ## As an example valid txt file, create "companies.txt" in our current working directory
#' ## containing the following two lines:
#' ## APPLE AAPL
#' ## GOOGLE GOOG
#' 
#' get_companies("companies.txt")
#' 
#' @importFrom dplyr arrange
#' @export

get_companies <- function(filepath = system.file("extdata/companies.txt", package = "qmj")) {
  ## TODO. INITIALLY READ IN THE DATA AND APPLY SOME REGEX TO REMOVE CHARACTERS FROM
  ## "As of" ONWARDS ON EACH LINE. THEN DO FOLLOWING CODE.
  
  ## Create two copies of the data to separately parse out names and tickers. 
  names <- readLines(filepath)
  tickers <- readLines(filepath)
  
  ## From the raw component list, company names and tickers
  ## should be differentiated by being entirely capitalized.
  ## These are the only lines we care about.
  names <- names[which(toupper(names) == names)]
  tickers <- tickers[which(toupper(tickers) == tickers)]
  
  #' @describeIn The function splits by space and grabs everything before 
  #' the last word as the name and has the last word as the ticker.
  #' Which chunk it returns is determined by the is_name variable.
  splitter <- function(s, is_name) {
    split_s <- unlist(strsplit(s, " "))
    if(is_name) {
      paste(split_s[1:length(split_s)-1], collapse = " ")
    } else {
      split_s[length(split_s)]
    }
  }
  
  names <- sapply(names, splitter, TRUE)
  tickers <- sapply(tickers, splitter, FALSE)
  
  ## Create a data frame with appropriate column names
  ## and remove companies without a name. 
  companies <- data.frame(name = names, ticker = tickers, stringsAsFactors = FALSE)
  companies <- companies[companies$name != "",]
  
  ## Want the row names to just be numeric indices. 
  row.names(companies) <- NULL
  
  ## Arrange alphabetically by ticker. 
  companies <- dplyr::arrange(companies, ticker)
  companies
}