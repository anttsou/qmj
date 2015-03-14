#' Makes raw incomestatement data usable and readable.
#'
#' Tidies raw income statement data produced from quantmod and returns the tidied data frame. Raw income statement data
#' must be formatted in a list such that every element is a data frame or matrix containing quantmod data.
#' 
#' \code{tidy_incomestatements} produces a data frame that is "tidy" or more readily readable by a user and usable by other functions
#' within this package.
#' @param x A list of raw incomestatement file data produced from quantmod
#' @return Returns a data set that's been "tidied" up for use by other functions in this package.
#' @seealso \code{\link{get_info}}
#' @seealso \code{\link{tidy_prices}}
#' @seealso \code{\link{tidy_cashflows}}
#' @seealso \code{\link{tidy_balancesheets}}
#' @examples
#' \dontrun{
#' companies <- data(companies)
#' raw_data <- get_info(companies)
#' tidyincome <- tidy_incomestatements(x[[2]])
#' }
#' @export

tidy_incomestatements <- function(x) {
  numCompanies <- length(x)
  incomestatements <- matrix(nrow=numCompanies*4, ncol=51) #Pre-allocates matrix for speed considerations.
    
  for(i in 1:numCompanies){
    cdata <- x[[2]][[i]] #Extracts this specific company's information from the raw data. Format is matrix.
    ticker <- gsub('[0-9 ]', '', colnames(cdata))[1] #Extract ticker from the first column of the data.
    years <- gsub('[ABCDEFGHIJKLMNOPQRSTUVWXYZ .]', '', colnames(cdata)) #Extracts the years each annual statements is dated.
    if(length(unique(years)) < length(years)){
      #If more than one annual statement is posted in a single year by the company, assign a suffix to deal with duplicates
      #during merging.
      for(j in 1:length(years)){
        years[j] <- paste(years[j], ".", j, sep='')
      }
    }
    for(k in 1:length(colnames(cdata))){
      incomestatements[k + (i-1)*4,1] <- ticker
      incomestatements[k + (i-1)*4,2] <- years[k]
      incomestatements[k + (i-1)*4,3] <- cdata[1,k]
      incomestatements[k + (i-1)*4,4] <- cdata[2,k]
      incomestatements[k + (i-1)*4,5] <- cdata[3,k]
      incomestatements[k + (i-1)*4,6] <- cdata[4,k]
      incomestatements[k + (i-1)*4,7] <- cdata[5,k]
      incomestatements[k + (i-1)*4,8] <- cdata[6,k]
      incomestatements[k + (i-1)*4,9] <- cdata[7,k]
      incomestatements[k + (i-1)*4,10] <- cdata[8,k]
      incomestatements[k + (i-1)*4,11] <- cdata[9,k]
      incomestatements[k + (i-1)*4,12] <- cdata[10,k]
      incomestatements[k + (i-1)*4,13] <- cdata[11,k]
      incomestatements[k + (i-1)*4,14] <- cdata[12,k]
      incomestatements[k + (i-1)*4,15] <- cdata[13,k]
      incomestatements[k + (i-1)*4,16] <- cdata[14,k]
      incomestatements[k + (i-1)*4,17] <- cdata[15,k]
      incomestatements[k + (i-1)*4,18] <- cdata[16,k]
      incomestatements[k + (i-1)*4,19] <- cdata[17,k]
      incomestatements[k + (i-1)*4,20] <- cdata[18,k]
      incomestatements[k + (i-1)*4,21] <- cdata[19,k]
      incomestatements[k + (i-1)*4,22] <- cdata[20,k]
      incomestatements[k + (i-1)*4,23] <- cdata[21,k]
      incomestatements[k + (i-1)*4,24] <- cdata[22,k]
      incomestatements[k + (i-1)*4,25] <- cdata[23,k]
      incomestatements[k + (i-1)*4,26] <- cdata[24,k]
      incomestatements[k + (i-1)*4,27] <- cdata[25,k]
      incomestatements[k + (i-1)*4,28] <- cdata[26,k]
      incomestatements[k + (i-1)*4,29] <- cdata[27,k]
      incomestatements[k + (i-1)*4,30] <- cdata[28,k]
      incomestatements[k + (i-1)*4,31] <- cdata[29,k]
      incomestatements[k + (i-1)*4,32] <- cdata[30,k]
      incomestatements[k + (i-1)*4,33] <- cdata[31,k]
      incomestatements[k + (i-1)*4,34] <- cdata[32,k]
      incomestatements[k + (i-1)*4,35] <- cdata[33,k]
      incomestatements[k + (i-1)*4,36] <- cdata[34,k]
      incomestatements[k + (i-1)*4,37] <- cdata[35,k]
      incomestatements[k + (i-1)*4,38] <- cdata[36,k]
      incomestatements[k + (i-1)*4,39] <- cdata[37,k]
      incomestatements[k + (i-1)*4,40] <- cdata[38,k]
      incomestatements[k + (i-1)*4,41] <- cdata[39,k]
      incomestatements[k + (i-1)*4,42] <- cdata[40,k]
      incomestatements[k + (i-1)*4,43] <- cdata[41,k]
      incomestatements[k + (i-1)*4,44] <- cdata[42,k]
      incomestatements[k + (i-1)*4,45] <- cdata[43,k]
      incomestatements[k + (i-1)*4,46] <- cdata[44,k]
      incomestatements[k + (i-1)*4,47] <- cdata[45,k]
      incomestatements[k + (i-1)*4,48] <- cdata[46,k]
      incomestatements[k + (i-1)*4,49] <- cdata[47,k]
      incomestatements[k + (i-1)*4,50] <- cdata[48,k]
      incomestatements[k + (i-1)*4,51] <- cdata[49,k]
    }
  }
  incomestatements <- incomestatements[rowSums(!is.na(incomestatements)) >= 1,] #Remove rows which are entirely NA from the pre-allocated matrix.
  
  incomeframe <- data.frame(incomestatements,stringsAsFactors=FALSE)
  # These are the categories we expect from the raw data.
  colnames(incomeframe) <- c("ticker", "year", "REV", "OREV", "TREV", "CREV", "GPROF", 
                                  "SGAE", "RD", "DP.AM", "NINT", "UI", "OOE", "TOE", "OI", "INT", "GSA", "OTH", "IBT", 
                                  "IAT", "MI", "EIA", "NIBEI", "AC", "DO", "EI", "NI", "PD", "IACEEI", "IACIEI", 
                                  "BWAS", "BEPSEEI", "BEPSIEI", "DILADJ", "DILWAS", "DILEPSEEI", "DILEPSIEI", 
                                  "DIVC", "GDIV", "NIASBCE", "BEPSSBCE", "DEPSSBCE", "DPSUP", "TSI", "NIBT", "ESIIT", 
                                  "ITISI", "NIAT", "NIAC", "BNEPS", "DNEPS")
  incomeframe[,2:51] = sapply(incomeframe[,2:51],as.numeric)
  incomeframe
}