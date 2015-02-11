#' Gets raw company balance sheets, income statements, and cash flows from Google Finance.
#'
#' Retrieves data for the data frame of companies and generates a list with three elements. Each element
#' is a large list containing all the balance sheets, income statements, or cash flow statements for all companies.
#' @param companies A data frame of companies. Must have a ticker column.
#' @seealso \code{\link{get_prices}}
#' @examples
#' \dontrun{
#' data(companies)
#' get_info(companies)
#' }
#' @export

get_info <- function(x) {
  if(length(x$ticker) == 0) {
    stop("parameter requires a ticker column.")
  }  
  tickers <- as.character(x$ticker)
  
  filepath <- system.file("extdata", package="qmj")
  listfiles <- rep("", length(x$ticker))
  filesInDest <- list.files(path=filepath)
  for(i in tickers) {
    file <- paste(i, "-fin", ".RData", sep='')
    fileName <- paste(filepath, "/", i, "-fin.RData", sep='')
    if(is.element(file, filesInDest)){
      print(paste(i, "information found in extdata. Resuming Download.", sep=' '))
      listfiles[i] <- fileName
    } else{
      prospective <- tryCatch(quantmod::getFinancials(i,auto.assign = FALSE),
                              error=function(e) {
                                e
                              })
      matr <- matrix()
      if(!inherits(prospective,"error")) {
        if(nrow(matr <- quantmod::viewFinancials(prospective,type = 'CF',period = 'A'))) {
          a <- 1
          while(a <= length(colnames(matr))) {
            colnames(matr)[a] <- sub("[-][0-9]*[-][0-9]*","",paste(i,colnames(matr)[a]))
            a <- a + 1
          }
          cashflow <- matr
        }
        if(nrow(matr <- quantmod::viewFinancials(prospective,type = 'IS',period = 'A'))) {
          a <- 1
          while(a <= length(colnames(matr))) {
            colnames(matr)[a] <- sub("[-][0-9]*[-][0-9]*","",paste(i,colnames(matr)[a]))
            a <- a + 1
          }
          incomestatement <- matr
        }
        if(nrow(matr <- quantmod::viewFinancials(prospective,type = 'BS',period = 'A'))) {
          a <- 1
          while(a <= length(colnames(matr))) {
            colnames(matr)[a] <- sub("[-][0-9]*[-][0-9]*","",paste(i,colnames(matr)[a]))
            a <- a + 1
          }
          balancesheet <- matr
        }
        
        clist <- list(cashflow, incomestatement, balancesheet)
        listfiles[i] <- fileName
        save(clist, file=fileName)
      } else {
        print(paste("Error retrieving data for ", i, sep=''))
        warning(paste("No daily data for ", i,sep=""))
      }
    }
  }
  listfiles <- listfiles[listfiles != ""]
  cashflows <- list()
  incomestatements <- list()
  balancesheets <- list()
  if(length(listfiles) > 1){
    for(i in 1:(length(listfiles))){
      load(listfiles[i])
      cashflows <- c(cashflows, clist[1])
      incomestatements <- c(incomestatements, clist[2])
      balancesheets <- c(balancesheets, clist[3])
    }
  }
  resultlist <- list(cashflows, incomestatements, balancesheets)
  file.remove(listfiles)
  resultlist
}