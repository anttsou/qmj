#' Makes prices data usable and readable.
#'
#' Tidies raw prices and returns a tidied, usable data frame. Raw data should be structured identically to that produced
#' by get_prices(), as this function depends on that structure.
#' 
#' \code{tidy_prices} produces a data frame that is "tidy" or more readily readable by a user and usable by other functions
#' within this package.
#' @param x Raw daily data, as produced by get_prices()
#' @return Returns a data set that's been "tidied" up for use by other functions in this package.
#' @seealso \code{\link{get_prices}}
#' @examples
#' \dontrun{
#' companies <- data(companies)
#' raw_price_data <- get_prices(companies)
#' prices <- tidy_prices(raw_price_data)
#' }
#' @export

tidy_prices <- function(x){
  numCompanies <- (length(names(x))/2) #Every company which has data adds two columns to the raw data output.
  numDaysInTwoYears <- 732 #We expect at most this many days of data from a company.
  tidymatrix <- matrix(nrow=(numDaysInTwoYears * numCompanies), ncol=4) #Output matrix is pre-allocated for speed.
  colnames(tidymatrix) <- c("ticker", "date", "pret", "close")
  dates <- rownames(x)
  for(i in 1:numCompanies){
    print(paste(i/numCompanies, " complete", sep=''))
    ticker <- gsub(".Close","",names(x)[2*i - 1]) #Parses the ticker out of one of the columns contributed by the company.
    ticker <- gsub(".Adjusted", "", ticker) #Correctly retrieves ticker from the special case of GSPC.Adjusted (S&P 500)
    col1 <- x[,(2*i)-1]
    col2 <- x[,(2*i)]
    
    #Select only data rows which are not na.
    nas <- which(!is.na(col1))
    cDates <- dates[nas]
    col1 <- col1[nas]
    col2 <- col2[nas]
    
    for(k in 1:length(col1)){
      #Loop places raw company data into expected positions in pre-allocated tidymatrix.
      tidymatrix[k + ((i-1) * numDaysInTwoYears), 1] <- ticker
      tidymatrix[k + ((i-1) * numDaysInTwoYears), 2] <- cDates[k]
      tidymatrix[k + ((i-1) * numDaysInTwoYears), 4] <- col1[k]
      tidymatrix[k + ((i-1) * numDaysInTwoYears), 3] <- col2[k]
    }
  }
  tidyresult <- as.data.frame(tidymatrix, stringsAsFactors=FALSE)
  tidyresult <- tidyresult[which(!is.na(tidyresult$ticker)), ] #Remove rows that are NA's, which should constitute a large percentage of the data.
  tidyresult$pret <- as.numeric(tidyresult$pret)
  tidyresult$close <- as.numeric(tidyresult$close)
}