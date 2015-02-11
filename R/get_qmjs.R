#' Returns specific qmj objects in the existing data set of qmj.
#'
#' Given a vector of tickers and a data frame of qmjs, returns
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
  
  qmj_tickers <- sapply(qmjs, get_ticker)
  indices <- match(tickers, qmj_tickers)
  qmjs[indices]
}