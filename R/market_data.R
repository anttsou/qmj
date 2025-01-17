#' Produces component and quality scores.
#'
#' Calculates market growth, payouts, safety, and 
#' profitability of our list of companies for later 
#' processing.
#' 
#' All parameters default to package data sets and must
#' be formatted similarly to a data frame produced by
#' \code{\link{tidy_prices}} and \code{\link{tidyinfo}}.
#' 
#' @return A data frame containing company names, tickers, 
#' profitability z-scores, growth z-scores, safety z-scores,
#' payout z-scores, and quality z-scores. Organized by
#' quality in descending order.
#' 
#' @param companies A data frame of company names and 
#' tickers. 
#' @param financials A data frame containing financial 
#' information for the given companies.
#' @param prices A data frame containing the daily 
#' market closing prices and returns. 
#' 
#' @seealso \code{\link{market_profitability}}
#' @seealso \code{\link{market_growth}}
#' @seealso \code{\link{market_safety}}
#' @seealso \code{\link{market_payouts}}
#' 
#' @examples
#' \donttest{
#' # Takes more than 10 secs
#' market_data(companies_r3k16[companies_r3k16$ticker %in% c("AAPL"), ])
#' }
#' 
#' @importFrom dplyr arrange desc %>%
#' @importFrom rlang .data
#' 
#' @return data.frame of all market data
#' 
#' @export

market_data <- function(companies = qmj::companies_r3k16, financials = qmj::financials_r3k16, prices = qmj::prices_r3k16) {
  if (length(companies$ticker) == 0) {
    stop("first parameter requires a ticker column.")
  }
  if (length(which(financials$TCSO < 0))) {
    stop("Negative TCSO exists.")
  }
  
  ## First Filter: All companies must have an annual financial statement posted two years ago,
  ## we'll call this the target-year. Since some companies may produce an 10-K filing early
  ## the next year, we'll also allow any company which produced a filing the following year
  ## through this filter.
  # target_year <- as.numeric(format(Sys.Date(), "%Y")) - 2
  target_year <- max(financials$year) - 2
  leeway_year <- target_year + 1
  
  valid_tickers <- dplyr::filter(financials, .data[["year"]]==target_year | .data[["year"]]==leeway_year) %>%
                   dplyr::select(ticker) %>%
                   dplyr::distinct()
  
  ## Second Filter: All companies must have 3-4 years of contiguous financial data including
  ## the target year.
  
  ## Second Filter: Keeps only those companies which have 3-4 years of contiguous
  ## financial data including the target year (or leeway year).
  second_filter <- function(selected_ticker, fin, target_year, leeway_year) {
    selected_rows <- dplyr::filter(fin, .data[["ticker"]]==selected_ticker)
    
    ## Check to ensure that 3-4 years of financial data exist.
    if(nrow(selected_rows) >= 3) {
      
      ## Check to ensure that the target year, or the leeway year, is contained in the data.
      if(target_year %in% selected_rows$year | leeway_year %in% selected_rows$year){
        
        ## Check to ensure that years are contiguous. We'll allow some flexibility on this,
        ## due to the possibility of a company filing an annual report early the next calendar year,
        ## and then filing said report on an annual basis thereafter.
        ## As some companies may also produce two filings within the same calendar year
        ## (for example, at the beginning of January and then again late in December),
        ## we're interested primarily in just ensuring that the summed differences of
        ## the years of each filing is within a certain bound.
        ## Consequently, we'll test to see if the sum of the differences between adjacent
        ## row years is <= 4.
        if(sum(diff(selected_rows$year)) <= 4)
          return(selected_ticker)
      }
    }
    
    ## Return a predictable failure flag.
    return("")
  }
  
  valid_tickers <- sapply(valid_tickers$ticker, second_filter, financials, target_year, leeway_year)
  valid_tickers <- valid_tickers[valid_tickers != ""]
  
  ## Price Filter: Remove companies from consideration which do not have a significant
  ## amount of price data.
  expected_rows <- length(prices$ticker[prices$ticker == 'GSPC'])
  passing_companies <- table(prices$ticker[!is.na(prices$pret)])
  
  ## Say we want each company to have at least 80% of our maximal data company, GSPC.
  passing_companies <- passing_companies[passing_companies >= (expected_rows * 4/5)]
  passing_companies <- rownames(passing_companies)
  
  valid_tickers <- passing_companies[passing_companies %in% valid_tickers]
  
  ## Single out those companies that have passed our filters.
  companies <- companies[companies$ticker %in% valid_tickers,]
  
  ## Calculate component scores.
  profitability <- market_profitability(companies, financials)$profitability
  growth <- market_growth(companies, financials)$growth
  safety <- market_safety(companies, financials, prices)$safety
  payouts <- market_payouts(companies, financials)$payouts
  
  ## Calculate quality scores and get z-scores.
  quality <- profitability + growth + safety + payouts
  quality <- scale(quality)
  
  name <- companies$name
  ticker <- companies$ticker
  marketdata <- data.frame(name = name, ticker = ticker, profitability = profitability, growth = growth, safety = safety, payouts = payouts, quality = quality)
  
  ## Arrange data by
  marketdata <- dplyr::arrange(marketdata, desc(quality))
  marketdata
} 
