#' Tidies raw financial data.
#'
#' Reads in raw financial data in the /extdata folder of the qmj package,
#' tidies the data found, and then saves that data in the /data folder.
#' @param x A list of lists of financial statements. Generated from get_info(companies).
#' @export

tidyinfo <- function(x){
  tidybalance <- tidy_balancesheets(x[[3]])
  tidycash <- tidy_cashflows(x[[1]])
  tidyincome <- tidy_incomestatements(x[[2]])
  #   save(tidyincome, file=destpathi)

  financials <- merge(tidybalance, merge(tidycash, tidyincome, by=c("ticker", "year")), by=c("ticker", "year"))
  financials <- unique(financials)
  keep <- c("ticker","year","AM","CWC","CX","DIVC",
            "DO","DP.DPL","GPROF","IAT","IBT","NI",
              "NINT","NRPS","RPS","TA","TCA","TCL",
                     "TCSO","TD","TL","TLSE","TREV")
  financials[keep]
}