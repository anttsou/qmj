#' Collects payout z-scores for companies
#'
#' Given a list of companies (names and tickers), a balance sheet, a cash flow statement,
#' and an income statement, calculates EISS, DISS, NPOP
#' and determines the z-score of overall payout based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A3-4.
#' @param x A dataframe of company names and tickers.
#' @param BS A dataframe containing balance sheet information for every company.
#' @param IS A dataframe containing income statement information for every company.
#' @examples
#' x <- data(companies)
#' BS <- data(tidybalance)
#' IS <- data(tidyincome)
#' collect_market_payout(x, BS, IS)
#' @export

collect_market_payout <- function(x, BS, IS){
  #Is there a better way to do this than calling "library(data.table)?"
  library(data.table)
  
  numCompanies <- length(x$tickers)
  payouts <- rep(0, numCompanies)
  
  BS[is.na(BS)] <- 0
  IS[is.na(IS)] <- 0
  
  #Function returns a structure that contains all elements in x.1 that are not in x.2
  modifiedsetdiff <- function(x.1,x.2,...){
    x.1p <- do.call("paste", x.1)
    x.2p <- do.call("paste", x.2)
    x.1[! x.1p %in% x.2p, ]
  }
  
  allcompanies <- data.frame(x$tickers)
  colnames(allcompanies) <- "ticker"

  fin <- merge(BS, IS, by=c("ticker", "year"))
  fin <- fin[order(fin$year, decreasing=TRUE),]
  fin <- data.table(fin, key="ticker")
  fstyear <- unique(fin)
  
  fin <- modifiedsetdiff(fin, fstyear)
  sndyear <- unique(fin)

  fin <- modifiedsetdiff(fin, sndyear)
  thdyear <- unique(fin)

  fthyear <- modifiedsetdiff(fin, thdyear)
  fthyear <- unique(fthyear)
  
  #Forces all data frames to have the same number of rows.
  fstyear <- merge(allcompanies, fstyear, by="ticker", all.x = TRUE)
  sndyear <- merge(allcompanies, sndyear, by="ticker", all.x = TRUE)
  thdyear <- merge(allcompanies, thdyear, by='ticker', all.x = TRUE)
  fthyear <- merge(allcompanies, fthyear, by='ticker', all.x = TRUE)
  
  eiss <- function(tcso1, tcso2){
    -log(tcso1/tcso2)
  }
  diss <- function(td1, td2){
    -log(td1/td2)
  }
  npop <- function(ni1, ni2, ni3, ni4, tlse1, tlse2, tlse3, tlse4,tl1, tl2, tl3, tl4, rps1, 
                   rps2, rps3, rps4, nrps1, nrps2, nrps3, nrps4, gprof1, gprof2, gprof3, gprof4){
    if(is.na(ni1)){
      ni1 <- 0
    }
    if(is.na(ni2)){
      ni2 <- 0
    }
    if(is.na(ni3)){
      ni3 <- 0
    }
    if(is.na(ni4)){
      ni4 <- 0
    }
    if(is.na(tlse1)){
      tlse1 <- 0
    }
    if(is.na(tlse2)){
      tlse2 <- 0
    }
    if(is.na(tlse3)){
      tlse3 <- 0
    }
    if(is.na(tlse4)){
      tlse4 <- 0
    }
    if(is.na(tl1)){
      tl1 <- 0
    }
    if(is.na(tl2)){
      tl2 <- 0
    }
    if(is.na(tl3)){
      tl3 <- 0
    }
    if(is.na(tl4)){
      tl4 <- 0
    }
    if(is.na(rps1)){
      rps1 <- 0
    }
    if(is.na(rps2)){
      rps2 <- 0
    }
    if(is.na(rps3)){
      rps3 <- 0
    }
    if(is.na(rps4)){
      rps4 <- 0
    }
    if(is.na(nrps1)){
      nrps1 <- 0
    }
    if(is.na(nrps2)){
      nrps2 <- 0
    }
    if(is.na(nrps3)){
      nrps3 <- 0
    }
    if(is.na(nrps4)){
      nrps4 <- 0
    }
    if(is.na(gprof1)){
      gprof1 <- 0
    }
    if(is.na(gprof2)){
      gprof2 <- 0
    }
    if(is.na(gprof3)){
      gprof3 <- 0
    }
    if(is.na(gprof4)){
      gprof4 <- 0
    }
    
    totalProfits <- gprof1 + gprof2 + gprof3 + gprof4
    sumNI <- ni1 + ni2 + ni3
    bookequity1 <- tlse1 - tl1 - (rps1 + nrps1)
    bookequity2 <- tlse2 - tl2 - (rps2 + nrps2)
    bookequity3 <- tlse3 - tl3 - (rps3 + nrps3)
    bookequity4 <- tlse4 - tl4 - (rps4 + nrps4)
    changebookequity1 <- bookequity2 - bookequity1
    changebookequity2 <- bookequity3 - bookequity2
    changebookequity3 <- bookequity4 - bookequity3
    sumChanges <- changebookequity1 + changebookequity2 + changebookequity3
    totalNetPayouts <- sumNI - sumChanges
    
    totalNetPayouts/totalProfits
  }
  
  EISS <- mapply(eiss, as.numeric(as.character(fstyear$TCSO)), as.numeric(as.character(sndyear$TCSO)))
  DISS <- mapply(diss, as.numeric(as.character(fstyear$TD)), as.numeric(as.character(sndyear$TD)))
  NPOP <- mapply(npop, as.numeric(as.character(fstyear$NI)), as.numeric(as.character(sndyear$NI)),
                 as.numeric(as.character(thdyear$NI)), as.numeric(as.character(fthyear$NI)), 
                 as.numeric(as.character(fstyear$TLSE)), as.numeric(as.character(sndyear$TLSE)),
                 as.numeric(as.character(thdyear$TLSE)), as.numeric(as.character(fthyear$TLSE)),
                 as.numeric(as.character(fstyear$TL)), as.numeric(as.character(sndyear$TL)),
                 as.numeric(as.character(thdyear$TL)), as.numeric(as.character(fthyear$TL)),
                 as.numeric(as.character(fstyear$RPS)), as.numeric(as.character(sndyear$RPS)),
                 as.numeric(as.character(thdyear$RPS)), as.numeric(as.character(fthyear$RPS)),
                 as.numeric(as.character(fstyear$NRPS)), as.numeric(as.character(sndyear$NRPS)),
                 as.numeric(as.character(thdyear$NRPS)), as.numeric(as.character(fthyear$NRPS)),
                 as.numeric(as.character(fstyear$GPROF)), as.numeric(as.character(sndyear$GPROF)),
                 as.numeric(as.character(thdyear$GPROF)), as.numeric(as.character(fthyear$GPROF)))

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
  
  payouts <- scale(payouts)
  data.frame(x$tickers, payouts, EISS, DISS, NPOP)
}