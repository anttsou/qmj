#' Removes all downloaded temporary files for a given data frame of companies.
#' 
#' This function works to remove the files that get_info and get_prices
#' temporarily store in order to conserve progress. 
#' Because this data may become irrelvant over time,
#' clean_downloads removes these files so that get_info and
#' get_prices may download fresh sets of data for a given
#' data frame of companies.
#' 
#' The clean_downloads() function will also automatically remove any
#' temporarily stored data for the S&P 500, with the stock ticker ^GSPC.
#' 
#' Returns a logical vector Where the ith and (i+1)th correspond to
#' whether or not a temporary financial and/or price file, respectively, 
#' was found and removed for the ith company provided. 
#' The last two indices refer to the S&P 500 temporary data.
#'
#' @param x A data frame of companies. Must have a ticker column.
#' @seealso \code{\link{get_prices}}
#' @seealso \code{\link{get_info}}
#' @examples
#' sub_comps <- qmjdata::companies[1:2,]
#' clean_downloads(sub_comps)
#' @export

clean_downloads <- function(x = qmjdata::companies) {
  tickers = x$ticker
  if(length(tickers) == 0) {
    stop("parameter requires a ticker column.")
  }
  
  filepath <- Sys.getenv("temp")
  
  ## Every company will have at most two associated temporary files.
  ## The extra +1 is to account for temporary ^GSPC files.
  listfiles <- rep("", 2 * (length(x$ticker)+1))
  
  for(i in 1:length(tickers)) {
    ## File paths for both financial data, and price data, respectively.
    listfiles[(2*i)-1] <- paste0(filepath, "/", tickers[i], "-fin.RData")
    listfiles[(2*i)] <- paste0(filepath, "/", tickers[i], ".RData")
  }
  
  ## Add temporary S&P 500 data files at the end of the list.
  listfiles[(length(tickers)*2)+1] <- paste0(filepath, "/GSPC-fin.RData")
  listfiles[(length(tickers)*2)+2] <- paste0(filepath, "/GSPC.RData")
  
  file.remove(listfiles)
}