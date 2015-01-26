#' Graphical summary of quality scores
#'
#' Returns a graphical summary of the portfolio. Be aware, missing values, or insufficient information, will result in some values
#' becoming 0.
#' 
#' Possible types:
#' \itemize{
#'    \item marketcap : Displays quality vs. estimated market cap, returns a data frame with tickers, market cap, and quality.
#'    \item summary : Displays information returned from get_portfolio, itself a numeric summary of the portfolio.
#'    \item quality : Displays quality vs. index. Returns data frame with ticker and quality scores.
#'    \item profitability : Displays profitability vs. index. Returns data frame with ticker and profitability scores.
#'    \item safety : Displays safety vs. index. Returns data frame with ticker and safety scores.
#'    \item growth : Displays growth vs. index. Returns data frame with ticker and growth scores.
#'    \item payouts : Displays payouts vs. index. Returns data frame with ticker and payouts scores.
#'  }
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
    data <- qmj::collectmarketdata()
    quality <- data$quality
    quality[is.na(quality)] <- 0
    ggplot2::qplot(MC, quality, colour=quality, xlab="Estimated Market Cap", ylab="Quality Score")
    data.frame(companies$ticker, MC, quality)
  } else if(type == "summary"){
    summarydata <- qmj::get_portfolio()
    qdata <- qmj::collectmarketdata()
    ticker <- c(summarydata$Top.Companies.by.Measured.Quality, summarydata$Bottom.Companies.by.Measured.Quality)
    indices <- mapply(match, ticker, quality$tickers)
    qdata <- qdata[indices,]
    qdata <- qdata$quality
    ggplot2::qplot(length(qdata), qdata, colour=qdata, xlab="Graph Index", ylab="Quality Score")
    res <- data.frame(ticker, qdata)
    colnames(res) <- c("ticker", "quality")
    res
  } else if(type == "quality"){
    data <- qmj::collectmarketdata()
    ggplot2::qplot(length(data$tickers), data$quality, colour=data$quality, xlab="Graph Index", ylab="Quality Score")
    res <- data.frame(data$tickers, data$quality)
    colnames(res) <- c("ticker", "quality")
    res
  } else if(type == "profitability"){
    data <- qmj::collectmarketdata()
    ggplot2::qplot(length(data$tickers), data$profitability, colour=data$profitability, xlab="Graph Index", ylab="Profitability Score")
    res <- data.frame(data$tickers, data$profitability)
    colnames(res) <- c("ticker", "profitability")
    res
  } else if(type == "safety"){
    data <- qmj::collectmarketdata()
    ggplot2::qplot(length(data$tickers), data$safety, colour=data$safety, xlab="Graph Index", ylab="Safety Score")
    res <- data.frame(data$tickers, data$safety)
    colnames(res) <- c("ticker", "safety")
    res    
  } else if(type == "growth"){
    data <- qmj::collectmarketdata()
    ggplot2::qplot(length(data$tickers), data$growth, colour=data$growth, xlab="Graph Index", ylab="Growth Score")
    res <- data.frame(data$tickers, data$growth)
    colnames(res) <- c("ticker", "growth")
    res
  } else if(type == "payouts"){
    data <- qmj::collectmarketdata()
    ggplot2::qplot(length(data$tickers), data$payouts, colour=data$payouts, xlab="Graph Index", ylab="Payouts Score")
    res <- data.frame(data$tickers, data$payouts)
    colnames(res) <- c("ticker", "payouts")
    res
  } else {
    stop("Incorrect type entered! Check spelling and capitalization.")
  }
}