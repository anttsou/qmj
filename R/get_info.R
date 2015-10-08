#' Gets raw company balance sheets, income statements, and 
#' cash flows from Google Finance.
#'
#' Retrieves data for the data frame of companies and generates 
#' a list with three elements. Each element is a large list 
#' containing all the balance sheets, income statements, or cash
#' flow statements for all companies. Also writes .RData files 
#' for every company in the /extdata folder in the package folder. 
#' If canceled partway through, function is able to find and 
#' re-read this data, allowing resumption of progress.
#' @param x A data frame of companies. Must have a ticker column.
#' @seealso \code{\link{get_prices}}
#' @examples
#' sub_comps <- qmjdata::companies[1:2,]
#' get_info(sub_comps)
#' @importFrom quantmod getFinancials viewFinancials
#' @export

get_info <- function(x = qmjdata::companies) {
  tickers = x$ticker
  if(length(tickers) == 0) {
    stop("parameter requires a ticker column.")
  }  
  
  filepath <- system.file("extdata", package="qmj")
  listfiles <- rep("", length(x$ticker))
  filesInDest <- list.files(path=filepath)
  for(i in tickers) {
    file <- paste0(i, "-fin", ".RData")
    fileName <- paste0(filepath, "/", i, "-fin.RData")
    if(is.element(file, filesInDest)){
      print(paste(i, "information found in extdata. Resuming Download.", sep=' '))
      listfiles[i] <- fileName
    } else {
      prospective <- tryCatch(quantmod::getFinancials(i,auto.assign = FALSE),
                              error=function(e) e)
      matr <- matrix()
      if(!inherits(prospective,"error")) {
        
        ## grab cash flows from Google Finance
        
        if(nrow(matr <- viewFinancials(prospective,type = 'CF',period = 'A'))) {
          
          ## rename columns to include the ticker and the year
          
          colnames(matr) <- sub("[-][0-9]*[-][0-9]*", "", paste(i,colnames(matr)))
          
          ## add company cash flows to building list
          
          cashflow <- matr
        }
        
        ## grab income statements from Google Finance
        
        if(nrow(matr <- viewFinancials(prospective, type = 'IS', period = 'A'))) {
        
          colnames(matr) <- sub("[-][0-9]*[-][0-9]*", "", paste(i,colnames(matr)))

          ## add company income statements to building list
          
          incomestatement <- matr
        }
        
        ## grab balance sheets from Google Finance
        
        if(nrow(matr <- viewFinancials(prospective, type = 'BS', period = 'A'))) {
      
          colnames(matr) <- sub("[-][0-9]*[-][0-9]*", "",paste(i,colnames(matr)))
                
          ## add company balance sheets to building list
          
          balancesheet <- matr
        }
        
        clist <- list(cashflow, incomestatement, balancesheet)
        listfiles[i] <- fileName
        save(clist, file=fileName)
      } else {
        print(paste0("Error retrieving data for ", i))
        warning(paste0("No financials for ", i))
      }
    }
  }
  
  ## extract information from files to compile cash flows, income statements, and balance sheets
  
  listfiles <- listfiles[listfiles != ""]
  cashflows <- list()
  incomestatements <- list()
  balancesheets <- list()
  if(length(listfiles) >= 1){
    for(i in 1:(length(listfiles))) {
      load(listfiles[i])
      cashflows <- c(cashflows, clist[1])
      incomestatements <- c(incomestatements, clist[2])
      balancesheets <- c(balancesheets, clist[3])
    }
  }
  
  resultlist <- as.list(cashflows, incomestatements, balancesheets)
  file.remove(listfiles)
  resultlist
}