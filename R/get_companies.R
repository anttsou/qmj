#' Gets the company names and tickers from the most recent 
#' Russell 3000 Index.
#' 
#' Reads in the contents of a text file created from the 
#' pdf of company names and tickers given by the Russell 
#' 3000 Index. Because there is not a consistent way to read
#' pdfs into text files using R across different operating
#' systems, the user must copy and paste the contents of the 
#' Russell 3000 Index into a text file for this function to
#' process the data correctly. This can be done simply by 
#' selecting all on the Russell 3000 Index and pasting into
#' an empty document with the .txt extension. The contents
#' of the Russell 3000 Index can be found 
#' \href{https://www.russell.com/documents/indexes/membership/membership-russell-3000.pdf}{here}.
#' 
#' If you wish to create your own text files of companies for get_companies to process,
#' please be sure that each letter of all company names and tickers is capitalized and 
#' each word is separated by a single space, with each line starting with the company name
#' followed by the company ticker. For example, if you want to put two companies in your text 
#' file with the first having the name Apple and ticker AAPL and the second having the name
#' Google and ticker GOOG, please format the text file as follows: 
#' 
#' APPLE AAPL \cr
#' GOOGLE GOOG
#' 
#' Once you have your text file, you must pass the filepath as an argument to get_companies. 
#' For example, if your text file is called "companies.txt", you would use 
#' \code{get_companies("companies.txt")}. get_companies by default uses a text file 
#' in the package, so \code{get_companies()} will produce the data frame of names and tickers
#' for the most recent Russell 3000 Index already.
#' 
#' @examples
#' get_companies()
#' 
#' @importFrom dplyr arrange
#' @export

get_companies <- function(x = system.file("extdata/companies.txt", package = "qmj")) {
  
  ## Allows the user to specify a file path. Defaults to the txt file
  ## found in the extdata directory of the package. 
  
  filepath <- x
  
  ## Separates into two copies, one for names and one for tickers. 
  
  names <- readLines(filepath)
  tickers <- readLines(filepath)
  
  ## Only care about the lines that actually have company name 
  ## and ticker info, which are entirely capitalized. 
  
  names <- names[which(toupper(names) == names)]
  tickers <- tickers[which(toupper(tickers) == tickers)]
  
  ## Creates a string without the ticker for the names vector 
  ## and isolates the ticker for the tickers vector. The function
  ## splits by space and grabs everything before the last word as 
  ## the name and has the last word as the ticker, which is the
  ## formatting used in the Russell 3000 Index. 
  
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
  
  companies <- data.frame(name = names, ticker = tickers)
  companies <- companies[companies$name != "",]
  
  ## Want the row names to just be numeric indices. 
  
  row.names(companies) <- NULL
  
  ## Arrange alphabetically by ticker. 
  
  companies <- dplyr::arrange(companies, ticker)
  companies
}