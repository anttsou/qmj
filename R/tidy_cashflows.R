#' tidy_cashflows
#'
#' Tidies given cashflow data, formatted in typical style of quantmods viewFinancials function.
#' 
#' @export

tidy_cashflows <- function(x) {
  numCompanies <- length(x)
  cashflows <- data.frame(matrix(nrow=1, ncol=22))
  
  colnames(cashflows) <- c("name", "ticker", "year", "NI", "DP", "AM", "DT", "NCI", "CWC", "COA", "CX", "OICF", "CIA", "FCFI", "TCDP", "ISN", "IDN", "CFA", "FEE", "NCC", "CIP", "CTP")
  for(i in 1:numCompanies){
    cdata <- x[[i]]
    ticker <- gsub('[0-9 ]', '', colnames(cdata))[1]
    years <- gsub('[ABCDEFGHIJKLMNOPQRSTUVWXYZ ]', '', colnames(cdata))
    for(k in 1:length(colnames(cdata))){
      row <- c("", ticker, years[k], as.numeric(cdata[,k]))
      cashflows <- rbind(cashflows, setNames(row, names(cashflows)))
    }
  }
  cashflows[-1,]
}