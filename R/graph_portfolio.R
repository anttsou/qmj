#' Graphical summary of quality scores
#'
#' Returns a graphical summary of the portfolio. Be aware, missing values, or insufficient information, will result in some values
#' becoming 0.
#' 
#' @param type Type that determines the graph created and the information presented.
#' @export

graph_portfolio <- function(type="marketcap"){
  if(type == "marketcap"){
    library(data.table)
    data(companies, package="qmj")
    data(tidybalance, package="qmj")
    data(tidycash, package="qmj")
    data(tidyincome, package="qmj")
    data(tidydaily, package="qmj")
    
    companies <- data.frame(companies$tickers)
    colnames(companies) <- "ticker"
    
    fin <- merge(BS, merge(CF, IS, by=c("ticker", "year")), by=c("ticker", "year"))
    fin <- fin[order(fin$year, decreasing=TRUE),]
    fin <- data.table(fin, key="ticker")
    fstyear <- fin[J(unique(ticker)), mult="first"]
    fstyear <- merge(companies, fstyear, by="ticker", all.x = TRUE)
    
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
    MC <- mapply(marketcap, as.numeric(as.character(tempframe$close)), as.numeric(as.character(fstyear$TCSO)))
    MC[is.na(MC)] <- 0
    data <- collectmarketdata()
    quality <- data$quality
    quality[is.na(quality)] <- 0
    ggplot2::qplot(MC, quality, colour=quality, xlab="Estimated Market Cap", ylab="Quality Score")
    
  } else if(type == "summary"){
    
  } else {
    stop("Incorrect type entered! Check spelling and capitalization.")
  }
}