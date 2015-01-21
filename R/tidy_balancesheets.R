#' tidy_balancesheets
#'
#' Tidies given balancesheet data, formatted in typical style of quantmods viewFinancials function.
#' 
#' @export

tidy_balancesheets <- function(x) {
  numCompanies <- length(x)
  balancesheets <- data.frame(matrix(nrow=1, ncol=45))
  
  colnames(balancesheets) <- c("name", "ticker", "year", "CE", "STI", "CSTI", "AR", "RE", "TR", "TI", "PE", "OCA", "TCA", "PPE", "AD", "GDW", "INT", "LTI", "OLTA", "TA", "AP", "AE", "STD", "CL", "OCL", "TCL", "LTD", "CLO", "TLTD", "TD", "DIT", "MI", "OL", "TL", "RPS", "NRPS", "CS", "APIC", "RE", "TS", "OE", "TE", "TLSE", "SO", "TCSO")
  for(i in 1:numCompanies){
    cdata <- x[[i]]
    ticker <- gsub('[0-9 ]', '', colnames(cdata))[1]
    years <- gsub('[ABCDEFGHIJKLMNOPQRSTUVWXYZ ]', '', colnames(cdata))
    for(k in 1:length(colnames(cdata))){
      row <- c("", ticker, years[k], as.numeric(cdata[,k]))
      balancesheets <- rbind(balancesheets, setNames(row, names(balancesheets)))
    }
  }
  balancesheets[-1,]
}