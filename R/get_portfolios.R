#' Returns a specific Company object (Company Portfolio)
#'
#' Given a list of tickers and a data frame of portfolios, returns
#' particular company portfolios based on inputted tickers.
#' @param tickers A single ticker or vector of tickers that the user desires portfolios for.
#' @param portfolios A list of company portfolios, i.e. Company Portfolio objects.
#' @examples
#' tickers <- c("AAPL","GOOG")
#' data(portfolios)
#' get_portfolios(tickers,portfolios)
#' @export

get_portfolios <- function(tickers, portfolios){
  get_ticker <- function(portfolio){
    view_ticker(portfolio)
  }
  
  portfolio_tickers <- sapply(portfolios, get_ticker)
  indices <- match(tickers, portfolio_tickers)
  portfolios[indices]
}