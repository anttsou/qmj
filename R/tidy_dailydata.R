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
    ticker <- gsub("\\..*","",names(x)[2*i])
    col1 <- x[,(2*i)-1]
    col2 <- x[,(2*i)]
    nas <- which(!is.na(col1))
    cDates <- dates[nas]
    col1 <- col1[nas]
    col2 <- col2[nas]
    print(i/numCompanies)
#Attempt 3: Pre-allocated matrix.
    for(k in 1:length(col1)){
      tidymatrix[k + ((i-1) * numDaysInFiveYears), 1] <- ticker
      tidymatrix[k + ((i-1) * numDaysInFiveYears), 2] <- cDates[k]
      tidymatrix[k + ((i-1) * numDaysInFiveYears), 3] <- col1[k]
      tidymatrix[k + ((i-1) * numDaysInFiveYears), 4] <- col2[k]
    }
    
#Attempt 2: rbind individual rows to tidymatrix.
#     for(k in 1:length(col1)){
#       row <- c(ticker, cDates[k], col1[k], col2[k])
#       tidymatrix <- rbind(tidymatrix, row)
#     }
    
#Attempt 1: Make matrix of columns, combine with tidymatrix.
#     if(length(which(tidymatrix["ticker"] == ticker)) == 0 && length(col1) > 1000){
#       print(ticker)
#       print(i)
#       ticker <- rep(ticker, length(col1))
#       matr <- matrix(ticker)
#       matr <- cbind(matr, cDates)
#       matr <- cbind(matr, col1)
#       matr <- cbind(matr, col2)
#       colnames(matr) <- c("ticker", "date", "pret", "close")
#       tidymatrix <- merge(tidymatrix, matr, all=TRUE)
#     }
  }
  tidymatrix <- na.omit(tidymatrix)
  as.data.frame(tidymatrix)
}