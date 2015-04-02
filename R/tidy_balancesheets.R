#' Makes raw balancesheet data usable and readable.
#'
#' Processes raw balance sheet data produced from quantmod into a tidy data frame. Raw balance sheet data
#' must be formatted in a list such that every element is a data frame or matrix containing quantmod data.
#' 
#' \code{tidy_balancesheets} produces a data frame that is "tidy" or more readily readable by a user and usable by other functions
#' within this package.
#' @param x A list of raw cash flow data produced from quantmod
#' @return Returns a data set that's been "tidied" up for use by other functions in this package.
#' @seealso \code{\link{get_info}}
#' @seealso \code{\link{tidy_prices}}
#' @seealso \code{\link{tidy_cashflows}}
#' @seealso \code{\link{tidy_incomestatements}}
#' @examples
#' data(companies)
#' sub_comps <- companies[1:2,]
#' raw_data <- get_info(sub_comps)
#' tidybalance <- tidy_balancesheets(raw_data[[3]])
#' @export

tidy_balancesheets <- function(x) {
  numCompanies <- length(x)
  balancesheets <- matrix(nrow=numCompanies * 4, ncol=44) #Pre-allocate space for matrix for speed reasons.
  
  for(i in 1:numCompanies){
    cdata <- x[[i]] #Extract this specific company's data from the raw list.
    ticker <- gsub('[0-9 ]', '', colnames(cdata))[1] #Extract the ticker from the first column.
    years <- gsub('[ABCDEFGHIJKLMNOPQRSTUVWXYZ .]', '', colnames(cdata)) #Extract the years these financial statements were filed.
    if(length(unique(years)) < length(years)){
      #If more than one financial statement is filed in a single year, assign a suffix to all years in order to preserve order
      #and avoid duplicates.
      for(j in 1:length(years)){
        years[j] <- paste(years[j], ".", j, sep='')
      }
    }
    for(k in 1:length(colnames(cdata))){
      balancesheets[k + (i-1)*4,1] <- ticker
      balancesheets[k + (i-1)*4,2] <- years[k]
      balancesheets[k + (i-1)*4,3] <- cdata[1,k]
      balancesheets[k + (i-1)*4,4] <- cdata[2,k]
      balancesheets[k + (i-1)*4,5] <- cdata[3,k]
      balancesheets[k + (i-1)*4,6] <- cdata[4,k]
      balancesheets[k + (i-1)*4,7] <- cdata[5,k]
      balancesheets[k + (i-1)*4,8] <- cdata[6,k]
      balancesheets[k + (i-1)*4,9] <- cdata[7,k]
      balancesheets[k + (i-1)*4,10] <- cdata[8,k]
      balancesheets[k + (i-1)*4,11] <- cdata[9,k]
      balancesheets[k + (i-1)*4,12] <- cdata[10,k]
      balancesheets[k + (i-1)*4,13] <- cdata[11,k]
      balancesheets[k + (i-1)*4,14] <- cdata[12,k]
      balancesheets[k + (i-1)*4,15] <- cdata[13,k]
      balancesheets[k + (i-1)*4,16] <- cdata[14,k]
      balancesheets[k + (i-1)*4,17] <- cdata[15,k]
      balancesheets[k + (i-1)*4,18] <- cdata[16,k]
      balancesheets[k + (i-1)*4,19] <- cdata[17,k]
      balancesheets[k + (i-1)*4,20] <- cdata[18,k]
      balancesheets[k + (i-1)*4,21] <- cdata[19,k]
      balancesheets[k + (i-1)*4,22] <- cdata[20,k]
      balancesheets[k + (i-1)*4,23] <- cdata[21,k]
      balancesheets[k + (i-1)*4,24] <- cdata[22,k]
      balancesheets[k + (i-1)*4,25] <- cdata[23,k]
      balancesheets[k + (i-1)*4,26] <- cdata[24,k]
      balancesheets[k + (i-1)*4,27] <- cdata[25,k]
      balancesheets[k + (i-1)*4,28] <- cdata[26,k]
      balancesheets[k + (i-1)*4,29] <- cdata[27,k]
      balancesheets[k + (i-1)*4,30] <- cdata[28,k]
      balancesheets[k + (i-1)*4,31] <- cdata[29,k]
      balancesheets[k + (i-1)*4,32] <- cdata[30,k]
      balancesheets[k + (i-1)*4,33] <- cdata[31,k]
      balancesheets[k + (i-1)*4,34] <- cdata[32,k]
      balancesheets[k + (i-1)*4,35] <- cdata[33,k]
      balancesheets[k + (i-1)*4,36] <- cdata[34,k]
      balancesheets[k + (i-1)*4,37] <- cdata[35,k]
      balancesheets[k + (i-1)*4,38] <- cdata[36,k]
      balancesheets[k + (i-1)*4,39] <- cdata[37,k]
      balancesheets[k + (i-1)*4,40] <- cdata[38,k]
      balancesheets[k + (i-1)*4,41] <- cdata[39,k]
      balancesheets[k + (i-1)*4,42] <- cdata[40,k]
      balancesheets[k + (i-1)*4,43] <- cdata[41,k]
      balancesheets[k + (i-1)*4,44] <- cdata[42,k]
    }
  }
  balancesheets <- balancesheets[rowSums(!is.na(balancesheets)) >= 1,] #Remove all rows that are solely NAs.
  balanceframe <- data.frame(balancesheets, stringsAsFactors=FALSE)
  
  #These are the categories we expect from the raw data.
  colnames(balanceframe) <- c("ticker", "year", "CE", "STI", "CSTI", "AR", "RE", "TR", "TI", "PE", "OCA", "TCA", 
                               "PPE", "AD", "GDW", "INT", "LTI", "OLTA", "TA", "AP", "AE", "STD", "CL", "OCL", 
                               "TCL", "LTD", "CLO", "TLTD", "TD", "DIT", "MI", "OL", "TL", "RPS", "NRPS", 
                               "CS", "APIC", "RE", "TS", "OE", "TE", "TLSE", "SO", "TCSO")
  balanceframe[,2:44] = sapply(balanceframe[,2:44],as.numeric)
  balanceframe
}