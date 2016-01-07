#' Removes downloaded temporary files.
#' 
#' \code{clean_downloads} removes files that get_info and get_prices
#' temporarily store when progress is interrupted while updating. 
#' Because the temporarily stored data may become irrelvant over time,
#' clean_downloads removes these files so that get_info and
#' get_prices will download completely fresh sets of data for a given
#' data frame of companies.
#' 
#' The clean_downloads() function will also automatically remove any
#' temporarily stored data for the S&P 500, with the stock ticker ^GSPC.
#' 
#' @return
#' A logical vector Where the (2i-1)th and (2i)th element corresponds to
#' whether or not a temporary financial and/or price file, respectively, 
#' was found and removed for the ith company provided. 
#' 
#' For example, if AAPL was our 3rd company, for which we had not 
#' partially downloaded financial data, but did have temporary 
#' price data, the 5th and 6th elements of the logical vector 
#' would be FALSE and TRUE, respectively.
#' 
#' The last two indices refer to the S&P 500 temporary data.
#'
#' @param x A data frame of companies. Must have a ticker column.
#' @seealso \code{\link{get_prices}}
#' @seealso \code{\link{get_info}}
#' @examples
#' \dontrun{
#' ## Without a specified data frame, 
#' ## clean_downloads defaults to the package
#' ## provided data frame of comapnies.
#' 
#' clean_downloads()
#' 
#' ## If we wanted to remove temporarily 
#' ## downloaded files for only some
#' ## subset of our companies, or if we 
#' ## wanted to specify a modified data frame
#' ## of companies.
#' 
#' clean_downloads(sub_comps)
#' 
#' ## Fetch fresh data after removing old 
#' ## temporary files.
#' 
#' get_prices(sub_comps)
#' get_info(sub_comps)
#' }
#' @export

clean_downloads <- function(x = qmjdata::companies) {
  tickers = x$ticker
  if (length(tickers) == 0) {
    stop("parameter requires a ticker column.")
  }
  
  ## In order to comply with CRAN Standards, files must have been downloaded to the user's specified temporary directory.
  filepath <- Sys.getenv("temp")
  
  ## Every company will have at most two associated temporary files.  The extra +1 is to account for any temporary ^GSPC files.
  listfiles <- rep("", 2 * (length(x$ticker) + 1))
  
  for (i in 1:length(tickers)) {
    ## File paths for both financial data, and price data, respectively.
    listfiles[(2 * i) - 1] <- paste0(filepath, "/", tickers[i], "-fin.RData")
    listfiles[(2 * i)] <- paste0(filepath, "/", tickers[i], ".RData")
  }
  
  ## Add temporary S&P 500 data files at the end of the list.
  listfiles[(length(tickers) * 2) + 1] <- paste0(filepath, "/GSPC-fin.RData")
  listfiles[(length(tickers) * 2) + 2] <- paste0(filepath, "/GSPC.RData")
  
  file.remove(listfiles)
} 
