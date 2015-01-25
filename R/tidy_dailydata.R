#' tidy_dailydata
#'
#' Given the data frame organized similarly to that produced by getdailydata()
#'  in the qmj library, tidies the data and returns the resultant data frame.
#' @param x The resultant data frame from calling getdailydata(x).
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
  tidymatrix <- tidymatrix[which(is.na(tidymatrix$ticker), ]
  as.data.frame(tidymatrix)
}