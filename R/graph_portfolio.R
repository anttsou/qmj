#' Graphical summary of quality scores
#'
#' Returns a graphical summary of the portfolio
#' 
#' @param type 
#' @export

graph_portfolio <- function(type="summary"){
  if(type == "summary"){
    library(data.table)
    data(companies, package="qmj")
    data(tidybalance, package="qmj")
    data(tidycash, package="qmj")
    data(tidyincome, package="qmj")
    data(tidydaily, package="qmj")
    
    tidydaily <- data.table(tidydaily, key="ticker")
    tidydaily <- tidydaily[order(as.Date(tidydaily$date, format="%Y-%m-%d"), decreasing=TRUE),]
    setkey(tidydaily, "ticker")
    tidydaily <- tidydaily[J(unique(ticker)), mult="first"]
    
    closingframe <- data.frame(tidydaily$ticker, tidydaily$close)
    colnames(closingframe) <- c("ticker", "close")
    tempframe <- merge(companies, closingframe, by='ticker', all.x = TRUE)
    
    marketcap <- function(closingprice, tcso){
      closingprice * tcso
    }
    MC <- mapply(marketcap, as.numeric(as.character(tempframe$close)), as.numeric(as.character(tidybalance$TCSO)))
    quality <- 
  }
}