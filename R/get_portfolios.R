#' Returns a specific Company object (Company Portfolio)
#'
#' Given a data frame of companies, a ticker, a data frame with financial statements, and a data frame of prices, creates
#' a particular company portfolio and returns the result.
#' @param tickers A single ticker or vector of tickers that the user desires portfolios for.
#' @param portfolios A list of company portfolios, i.e., Company objects.
#' @export

get_portfolios <- function(tickers, portfolios){
  get_ticker <- function(portfolio){
    view_ticker(portfolio)
  }
  
  portfolio_tickers <- sapply(portfolios, get_ticker)
  indices <- match(tickers, portfolio_tickers)
  portfolios[indices]
}