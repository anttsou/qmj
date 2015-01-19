#' pricereturns
#'
#' Given an xts object with closing prices, returns an xts object with price returns.
#' @param x An xts object with closing prices.
#' @export

pricereturns <- function(x){
  closingprices <- as.numeric(x[,1])
  numEntries <- length(closingprices)
  pricereturns <- rep(0, numEntries)
  for(i in 2:numEntries){
    pricereturns[i] <- log(closingprices[i]) - log(closingprices[i-1])
  }
  pricereturns
}