#' collectmarketpayout
#'
#' Given a list of companies (names and tickers), a balance sheet, a cash flow statement,
#' and an income statement, calculates EISS, DISS, NPOP
#' and determines the z-score of overall payout based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A3-4.
#' @param x A dataframe of company names and tickers.
#' @param BS A dataframe containing balance sheet information for every company.
#' @param IS A dataframe containing income statement information for every company.
#' @export

collectmarketpayout <- function(x, BS, IS){
  # CollectMarketPayout collects data on overall payouts
  ## In the market for individual companies for later processing.
  ## x is the list of companies to be processed. BS, CF, IS are financial statements.
  numCompanies <- length(x$tickers)
  payouts <- rep(0, numCompanies)
  EISS <- rep(0, numCompanies)
  DISS <- rep(0, numCompanies)
  NPOP <- rep(0, numCompanies)
  BS[is.na(BS)] <- 0
  IS[is.na(IS)] <- 0
  
  #Function returns a structure that contains all elements in x.1 that are not in x.2
  modifiedsetdiff <- function(x.1,x.2,...){
    x.1p <- do.call("paste", x.1)
    x.2p <- do.call("paste", x.2)
    x.1[! x.1p %in% x.2p, ]
  }
  
  fin <- merge(BS, IS, by=c("ticker", "year"))
  fin <- fin[order(fin$year, decreasing=TRUE),]
  fin <- data.table(fin, key="ticker")
  fstyear <- fin[J(unique(ticker)), mult="first"]
  fin <- modifiedsetdiff(fin, fstyear)
  sndyear <- fin[J(unique(ticker)), mult="first"]
  
  eiss <- function(tcso1, tcso2){
    -log(tcso1/tcso2)
  }
  diss <- function(td1, td2){
    -log(td1/td2)
  }
  npop <- function(){
    
  }
  EISS <- mapply(eiss, as.numeric(as.character(fstyear$TCSO)), as.numeric(as.character(sndyear$TCSO)))
  DISS <- mapply(diss, as.numeric(as.character(fstyear$TD)), as.numeric(as.character(sndyear$TD)))
  
  for(i in 1:numCompanies) {
    cBS <- subset(BS,ticker == as.character(x$tickers[i]))
    cIS <- subset(IS,ticker == as.character(x$tickers[i]))
    
    if(nrow(cBS) > 1 && nrow(cIS) > 1) {
      #EISS
      # need to edit
      ##Total number of Shares - BS 43
      # Does not account for splits currently!!!
      EISS[i] <- -log(as.numeric(cBS$TCSO[1])/as.numeric(cBS$TCSO[2]))
      #DISS
      #Total debt - BS 28
      DISS[i] <- -log(as.numeric(cBS$TD[1])/as.numeric(cBS$TD[2]))
      
      #NPOP
      # (Net income - changes in book equity) / (total profits over the past 5 years)
      # Net income 
      #book equity = total liabilities and shareholders' equity - total liabilities - preferred stock
      # Gross profits - IS 6
      minimum <- min(c(length(cBS$TLSE), length(cIS$NI)))
      tempvect <- numeric()
      tempvect2 <- numeric()
      i <- 1
      while(i < minimum) {
        tempvect <- c(tempvect, as.numeric(cIS$NI[i+1]) - 
                     ((as.numeric(cBS$TLSE[i]) - 
                       as.numeric(cBS$TL[i]) - 
                      (as.numeric(cBS$RPS[i]) + as.numeric(cBS$NRPS[i]))) - 
                      (as.numeric(cBS$TLSE[i]) - 
                       as.numeric(cBS$TL[i+1]) - 
                      (as.numeric(cBS$RPS[i+1]) + as.numeric(cBS$NRPS[i+1])))))
        tempvect2 <- c(tempvect2, as.numeric(cIS$GPROF[i]), as.numeric(cIS$GPROF[i+1]))
        i <- i + 1
      }
      totalNetPayouts <- sum(tempvect)
      totalProfits <- sum(tempvect2)
      NPOP[i] <- totalNetPayouts/totalProfits
    }
  }
  
  EISS[is.infinite(EISS)] <- 0
  DISS[is.infinite(DISS)] <- 0
  NPOP[is.infinite(NPOP)] <- 0
  
  #Scale converts the individual scores for these values into z-scores.
  EISS <- scale(EISS)
  DISS <- scale(DISS)
  NPOP <- scale(NPOP)
  
  EISS[is.na(EISS)] <- 0
  DISS[is.na(DISS)] <- 0
  NPOP[is.na(NPOP)] <- 0
  
  payouts <- EISS + DISS + NPOP
  
#   for(i in 1:numCompanies){
#     payouts[i] <- EISS[i] + DISS[i] + NPOP[i]
#   }
  scale(payouts)
  res <- data.frame(fstyear$ticker, payouts)
  colnames(res) <- c("tickers", "payouts")
  originalorder <- data.frame(x$tickers)
  colnames(originalorder) <- "tickers"
  res <- merge(originalorder, res, by="tickers")
  #res$payouts
  res
}