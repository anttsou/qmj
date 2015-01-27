#' Summary of quality scores
#'
#' Returns, by default, the top five and the bottom five companies ordered by
#' calculated quality score. Also returns the standard deviation of quality scores
#' and the interquartile range.
#' 
#' @param top The number of top ranked companies to be returned.
#' @param bottom The number of bottom ranked companies to be returned.
#' @export

get_portfolio <- function(top=5, bottom=5) {
  filepath <- system.file(package="qmj")
  marketdata <- collect_market_data()
  if(length(marketdata$name) >= top + bottom) {
    cat("Top Companies by Measured Quality\n\n")
    cat(head(as.character(marketdata$tickers), n=top), sep="\n")
    cat("-----------------------------------\n")
    cat("Bottom Companies by Measured Quality\n")
    cat(tail(as.character(marketdata$tickers), n=bottom), sep="\n")
    cat("-----------------------------------\n")
    cat(paste("Standard Deviation of Quality Scores:", sd(marketdata$quality, na.rm=TRUE), "\n"))
    cat(paste("Interquartile Range of Quality Scores:", IQR(marketdata$quality, na.rm=TRUE), "\n"))
  } else {
    stop("Portfolio does not have enough companies given values for parameters \"top\" and \"bottom\"")
  }
}