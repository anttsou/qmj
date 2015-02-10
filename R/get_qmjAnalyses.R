#' Returns specific qmjAnalysis objects in the existing data set of qmjAnalyses.
#'
#' Given a vector of tickers and a data frame of qmjAnalyses, returns
#' particular company qmjAnalyses based on inputted tickers.
#' @param tickers A single ticker or vector of tickers that the user desires qmjAnalyses for.
#' @param qmjAnalyses A list of qmjAnalyses, i.e. qmjAnalysis objects.
#' @examples
#' tickers <- c("AAPL","GOOG")
#' data(qmjAnalyses)
#' get_qmjAnalyses(tickers,qmjAnalyses)
#' @export

get_qmjAnalyses <- function(tickers, qmjAnalyses){
  get_ticker <- function(qmjAnalysis){
    view_ticker(qmjAnalysis)
  }
  
  qmj_tickers <- sapply(qmjAnalyses, get_ticker)
  indices <- match(tickers, qmj_tickers)
  qmjAnalyses[indices]
}