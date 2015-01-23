#' tidy_cashflows
#'
#' Tidies given cashflow data, formatted in typical style of quantmods viewFinancials function.
#' 
#' @export

tidy_cashflows <- function(x) {
  numCompanies <- length(x)
  cashflows <- matrix(nrow=numCompanies*4, ncol=21)
  
  colnames(cashflows) <- c("ticker", "year", "NI", "DP", "AM", "DT", "NCI", "CWC", "COA", "CX", "OICF", "CIA", "FCFI", "TCDP", "ISN", "IDN", "CFA", "FEE", "NCC", "CIP", "CTP")
  for(i in 1:numCompanies){
    cdata <- x[[i]]
    ticker <- gsub('[0-9 ]', '', colnames(cdata))[1]
    years <- gsub('[ABCDEFGHIJKLMNOPQRSTUVWXYZ ]', '', colnames(cdata))
    for(k in 1:length(colnames(cdata))){
      cashflows[k + (i-1)*4,1] <- ticker
      cashflows[k + (i-1)*4,2] <- years[k]
      cashflows[k + (i-1)*4,3] <- cdata[1,k]
      cashflows[k + (i-1)*4,4] <- cdata[2,k]
      cashflows[k + (i-1)*4,5] <- cdata[3,k]
      cashflows[k + (i-1)*4,6] <- cdata[4,k]
      cashflows[k + (i-1)*4,7] <- cdata[5,k]
      cashflows[k + (i-1)*4,8] <- cdata[6,k]
      cashflows[k + (i-1)*4,9] <- cdata[7,k]
      cashflows[k + (i-1)*4,10] <- cdata[8,k]
      cashflows[k + (i-1)*4,11] <- cdata[9,k]
      cashflows[k + (i-1)*4,12] <- cdata[10,k]
      cashflows[k + (i-1)*4,13] <- cdata[11,k]
      cashflows[k + (i-1)*4,14] <- cdata[12,k]
      cashflows[k + (i-1)*4,15] <- cdata[13,k]
      cashflows[k + (i-1)*4,16] <- cdata[14,k]
      cashflows[k + (i-1)*4,17] <- cdata[15,k]
      cashflows[k + (i-1)*4,18] <- cdata[16,k]
      cashflows[k + (i-1)*4,19] <- cdata[17,k]
      cashflows[k + (i-1)*4,20] <- cdata[18,k]
      cashflows[k + (i-1)*4,21] <- cdata[19,k]

    }
  }
  cashflows <- cashflows[rowSums(!is.na(cashflows)) >= 1,]
  data.frame(cashflows)
}