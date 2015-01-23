#' tidy_incomestatements
#'
#' Tidies given income statement data, formatted in typical style of quantmods viewFinancials function.
#' 
#' @export

tidy_incomestatements <- function(x) {
  numCompanies <- length(x)
  incomestatements <- data.frame(matrix(nrow=1, ncol=51))
  
  colnames(incomestatements) <- c("ticker", "year", "REV", "OREV", "TREV", "CREV", "GPROF", "SGAE", "RD", "DP", "NINT", "UI", "OOE", "TOE", "OI", "INT", "GSA", "OTH", "IBT", "IAT", "MI", "EIA", "NIBEI", "AC", "DO", "EI", "NI", "PD", "IACEEI", "IACIEI", "BWAS", "BEPSEEI", "BEPSIEI", "DILADJ", "DILWAS", "DILEPSEEI", "DILEPSIEI", "DIVC", "GDIV", "NIASBCE", "BEPSSBCE", "DEPSSBCE", "DPSUP", "TSI", "NIBT", "ESIIT", "ITISI", "NIAT", "NIAC", "BNEPS", "DNEPS")
  for(i in 1:numCompanies){
    print(paste(i/numCompanies, "% complete", sep=''))
    cdata <- x[[i]]
    ticker <- gsub('[0-9 ]', '', colnames(cdata))[1]
    years <- gsub('[ABCDEFGHIJKLMNOPQRSTUVWXYZ ]', '', colnames(cdata))
    for(k in 1:length(colnames(cdata))){
      row <- c(ticker, years[k], as.numeric(cdata[,k]))
      incomestatements <- rbind(incomestatements, setNames(row, names(incomestatements)))
    }
  }
  incomestatements[-1,]
}