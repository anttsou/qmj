tidy_helper <- function(cdata) {
  
  ## Extract the ticker from the first column. 
  ## Remove the unecessary digits to grab only
  ## the ticker letters.
  
  symbol <- gsub('[0-9 ]', '', colnames(cdata))[1] 
  
  ## Extract the years these financial statements were filed. 
  ## Remove the uncessary letters to grab only the year digits.
  
  years <- gsub('[A-Z]', '', colnames(cdata)) 
  
  ## Takes the transpose to make the rows become columns and appends
  ## the ticker and year columns. 
  
  temp <- cbind(data.frame(ticker = symbol, year = as.numeric(years), 
                           stringsAsFactors = FALSE), 
                data.frame(t(cdata)))
}