#' getbalancesheets
#'
#' Retrieves data from companies.csv and writes associated balanceflow statements to 'balancesheets.csv'.
#' @export

getbalancesheets <- function() {
  filepath <- system.file("data", package="qmj")
  filepath1 <- paste(filepath, "/companies.RData", sep='')
  load(filepath1)
  tickers <- as.character(companies$tickers)
  balancesheets <- list()
  n <- 1
  for(i in tickers) {
    prospective <- tryCatch(getFinancials(i,auto.assign = FALSE),
                  error=function(e) {
                  e
                  })
    matr <- matrix()
    if(!inherits(prospective,"error") && nrow(matr <- viewFinancials(prospective,type = 'BS',period = 'A'))) {
        a <- 1
        while(a <= length(colnames(matr))) {
          colnames(matr)[a] <- sub("[-][0-9]*[-][0-9]*","",paste(i,colnames(matr)[a]))
          a = a + 1
        }
        balancesheets[[n]] <- matr
        n = n+1
    } else {
      balancesheets[[n]] <- matrix(dat=NA, ncol=4, nrow=42)
      n = n+1
    }
  }
  filepath2 <- paste(filepath, "/balancesheets.RData", sep='')
  #save(balancesheets, file="balancesheets.RData")
  balancesheets
}