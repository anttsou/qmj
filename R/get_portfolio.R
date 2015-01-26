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
  marketdata <- qmj::collect_market_data()
  if(length(marketdata$names) >= top + bottom) {
    cat("Top Companies by Measured Quality\n")
    cat(head(marketdata$names, n=top))
    cat("-----------------------------------")
    cat("Bottom Companies by Measured Quality\n")
    cat(tail(marketdata$names, n=bottom))
    cat("-----------------------------------")
    cat(paste("Standard Deviation of Quality Scores:", sd(marketdata$quality)))
    cat(paste("Interquartile Range of Quality Scores:", IQR(marketdata$quality)))
  } else {
    stop("Portfolio does not have enough companies given values for parameters \"top\" and \"bottom\"")
  }
  summarydata
}