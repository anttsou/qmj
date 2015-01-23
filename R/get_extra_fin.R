#' get_extra_fin
#'
#' Retrieves the beta of companies based on
#' yahoo finance's benchmark.
#' @examples
#' \donttest{
#' get_extra_fin()
#' }
#' @export

get_extra_fin <- function() {
  filepath <- system.file("data", package="qmj")
  filepath1 <- paste(filepath, "/companies.RData", sep='')
  load(filepath1)
  tickers <- as.character(companies$tickers)
  start <- "http://finance.yahoo.com/q?s="
  start2 <- "http://finance.yahoo.com/q/ks?s="
  betas <- numeric()
  ebitdas <- character()
  splitfactors <- character()
  splitdates <- character()
  for(i in tickers) {
    newUrl <- paste(start,i,sep="")
    con <- url(newUrl)
    htmlCode <- readLines(con)
    close(con)
    betas <- c(betas,as.numeric(sub("</td.*","",sub(".*Beta:</th><td class=\"yfnc_tabledata1\">","",htmlCode[191]))))
    newUrl2 <- paste(start2,i,sep="")
    con2 <- url(newUrl2)
    htmlCode2 <- readLines(con2)
    close(con2)
    n <- 100
    while(n <= length(htmlCode2)) {
      if(grepl(">EBITDA",htmlCode2[n])) {
        break
      }
      n <- n + 1
    }
    ebitdas <- c(ebitdas,sub(".*yfnc_tabledata1\">",
                             "",
                             sub("</td></tr>.*",
                                 "",
                                 sub(".*>EBITDA","",htmlCode2[n]))))
  }
  extrafin <- data.frame(tickers = tickers, 
                            betas = betas, 
                            ebitdas = ebitdas)
  filepath2 <- paste(filepath, "/extrafin.RData", sep='')
  save(extrafin,file="~/econ20/R Paper/qmj/data/extrafin.RData")
  save(extrafin, file=filepath2)
}