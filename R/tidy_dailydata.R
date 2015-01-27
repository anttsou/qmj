#' Tidies raw daily data set
#'
#' Tidies raw daily data and returns a tidied data frame. Companies which lack daily data are given a placeholder row containing
#' only their ticker. Raw data set should be xts insofar as row.names are a series of dates, every column must have a
#' ticker in their name, and the columns should be ordered "PRET" "CLOSE" "PRET" "CLOSE" ...
#' @param x Raw daily data, as produced by get_dailydata()
#' @export

tidy_dailydata <- function(x){
  numCompanies <- (length(names(x))/2)
  numDaysInFiveYears <- 1827
  tidymatrix <- matrix(nrow=(numDaysInFiveYears * numCompanies), ncol=4)
  colnames(tidymatrix) <- c("ticker", "date", "pret", "close")
  dates <- rownames(dailydata)
  for(i in 1:numCompanies){
    print(paste(i/numCompanies, "% complete", sep=''))
    ticker <- gsub("\\..*","",names(x)[2*i])
    col1 <- x[,(2*i)-1]
    col2 <- x[,(2*i)]
    nas <- which(!is.na(col1))
    cDates <- dates[nas]
    col1 <- col1[nas]
    col2 <- col2[nas]
    for(k in 1:length(col1)){
      tidymatrix[k + ((i-1) * numDaysInFiveYears), 1] <- ticker
      tidymatrix[k + ((i-1) * numDaysInFiveYears), 2] <- cDates[k]
      tidymatrix[k + ((i-1) * numDaysInFiveYears), 3] <- col1[k]
      tidymatrix[k + ((i-1) * numDaysInFiveYears), 4] <- col2[k]
    }
  }
  tidymatrix <- tidymatrix[which(is.na(tidymatrix$ticker)), ]
  as.data.frame(tidymatrix)
}