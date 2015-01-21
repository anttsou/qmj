#' getbetas
#'
#' Retrieves the beta of companies based on
#' yahoo finance's benchmark.
#' @examples
#' \donttest{
#' getbetas()
#' }
#' @export

getbetas <- function() {
  filepath <- system.file("data", package="qmj")
  filepath1 <- paste(filepath, "/companies.RData", sep='')
  load(filepath1)
  tickers <- as.character(companies$tickers)
  start <- "http://finance.yahoo.com/q?s="
  betas <- numeric()
  for(i in tickers) {
    newUrl <- paste(start,i,sep="")
    con <- url(newUrl)
    htmlCode <- readLines(con)
    close(con)
    betas <- c(betas,as.numeric(sub("</td.*","",sub(".*Beta:</th><td class=\"yfnc_tabledata1\">","",htmlCode[191]))))
  }
  betaframe <- data.frame(tickers = tickers, betas = betas)
  #filepath2 <- paste(filepath, "/betas.RData", sep='')
  save(betaframe, file="~/econ20/R Paper/qmj/data/betas.RData")
}