#' Returns specific qmj objects from a list of qmj objects.
#'
#' Given a vector of tickers and a list of qmjs, returns
#' particular company qmjs based on inputted tickers.
#' @param tickers A single ticker or vector of tickers that the user desires qmjs for.
#' @param qmjs A list of qmjs, i.e. qmj objects.
#' @examples
#' tickers <- c("AAPL","GOOG")
#' data(qmjs)
#' get_qmjs(tickers,qmjs)
#' @export

get_qmjs <- function(tickers, qmjs){
  get_ticker <- function(qmj){
    view_ticker(qmj)
  }
  
  # subset the qmjs dataset to include the companies
  # with the input tickers
  qmj_tickers <- sapply(qmjs, get_ticker)
  indices <- match(tickers, qmj_tickers)
  qmjs[indices]
}