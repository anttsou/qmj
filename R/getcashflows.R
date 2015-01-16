getcashflows <- function() {
  companies <- read.csv("companies.csv")
  tickers <- as.character(companies$tickers)
  vect <- list()
  n <- 1
  for(i in tickers) {
    prospective <- tryCatch(getFinancials(i,auto.assign = FALSE),
                            error=function(e) {
                              e
                            })
    matr <- matrix()
    if(!inherits(prospective,"error") && nrow(matr <- viewFinancials(prospective,type = 'CF',period = 'A'))) {
      a <- 1
      while(a <= length(colnames(matr))) {
        colnames(matr)[a] <- sub("[-][0-9]*[-][0-9]*","",paste(i,colnames(matr)[a]))
        a = a + 1
      }
      vect[[n]] <- matr
      n = n+1
    } else {
      vect[[n]] <- matrix(dat=NA, ncol=4, nrow=19)
      n = n+1
    }
  }
  write.csv(vect,file="cashflows.csv")
}