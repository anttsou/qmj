#' Tidies raw daily data set
#'
#' Tidies raw daily data and returns a tidied data frame. Companies which lack daily data are given a placeholder row containing
#' only their ticker. Raw data set should be xts insofar as row.names are a series of dates, every column must have a
#' ticker in their name, and the columns should be ordered "PRET" "CLOSE" "PRET" "CLOSE" ...
#' @param x Raw daily data, as produced by get_dailydata()
#' @export

tidy_dailydata <- function(x){
  numCompanies <- (length(names(x))/2)
  numDaysInTwoYears <- 732
  tidymatrix <- matrix(nrow=(numDaysInTwoYears * numCompanies), ncol=4)
  colnames(tidymatrix) <- c("ticker", "date", "pret", "close")
  dates <- rownames(x)
  for(i in 1:numCompanies){
    print(paste(i/numCompanies, " complete", sep=''))
    ticker <- gsub(".Close","",names(x)[2*i - 1])
    ticker <- gsub(".Adjusted", "", ticker)
    col1 <- x[,(2*i)-1]
    col2 <- x[,(2*i)]
    nas <- which(!is.na(col1))
    cDates <- dates[nas]
    col1 <- col1[nas]
    col2 <- col2[nas]
    for(k in 1:length(col1)){
      tidymatrix[k + ((i-1) * numDaysInTwoYears), 1] <- ticker
      tidymatrix[k + ((i-1) * numDaysInTwoYears), 2] <- cDates[k]
      tidymatrix[k + ((i-1) * numDaysInTwoYears), 4] <- col1[k]
      tidymatrix[k + ((i-1) * numDaysInTwoYears), 3] <- col2[k]
    }
  }
  tidyresult <- as.data.frame(tidymatrix, stringsAsFactors=FALSE)
  tidyresult <- tidyresult[which(!is.na(tidyresult$ticker)), ]
}