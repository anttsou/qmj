#' collectmarketsafety
#'
#' Given a list of companies (names and tickers), a balance sheet, a cash flow statement,
#' and an income statement, calculates BAB, IVOL, LEV, O, Z, and EVOL.
#' and determines the z-score of overall profitability based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param x A dataframe of company names and tickers.
#' @param BS A dataframe containing balance sheet information for every company.
#' @param CF A dataframe containing cash flow information for every company.
#' @param IS A dataframe containing income statement information for every company.
#' @param daily A dataframe containing the daily market closing prices and returns. 
#' @export

collectmarketsafety <- function(x, BS, CF, IS, extrafin, daily){
  #Is there a better way to do this than calling "library(data.table)?"
  library(data.table)
  
  filepath <- system.file("data", package="qmj")
  numCompanies <- length(x$tickers)
  safety <- rep(0, numCompanies)
  BAB <- rep(0, numCompanies)
  IVOL <- rep(0, numCompanies)
  LEV <- rep(0, numCompanies)
  O <- rep(0, numCompanies)
  Z <- rep(0, numCompanies)
  EVOL <- rep(0, numCompanies)
  BS[is.na(BS)] <- 0
  IS[is.na(IS)] <- 0
  CF[is.na(CF)] <- 0
  daily$date <- sub("-.*","",daily$date)
  daily <- data.table(daily, key="ticker")
  ordereddaily <- daily[order(fin$date, decreasing=TRUE),]
  splitindices <- split(seq(nrow(daily)), daily$ticker)  # Stores list of indices for a company ticker.
  companiesstored <- names(splitindices)
  yearlyprices <- ordereddaily[J(unique(ticker)), mult="first"]
  market <- daily[daily$ticker == "GSPC",]
  
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
  fstyear <- fin[J(unique(ticker)), mult="first"]
  
  fin <- modifiedsetdiff(fin, fstyear)
  sndyear <- fin[J(unique(ticker)), mult="first"]
  
  fin <- modifiedsetdiff(fin, sndyear)
  thdyear <- fin[J(unique(ticker)), mult="first"]
  
  fthyear <- modifiedsetdiff(fin, thdyear)
  fthyear <- unique(fthyear)
  
  #Forces all data frames to have the same number of rows.
  fstyear <- merge(allcompanies, fstyear, by="ticker", all.x = TRUE)
  sndyear <- merge(allcompanies, sndyear, by="ticker", all.x = TRUE)
  thdyear <- merge(allcompanies, thdyear, by='ticker', all.x = TRUE)
  fthyear <- merge(allcompanies, fthyear, by='ticker', all.x = TRUE)

  lev <- function(td, ta){
    -td/ta
  }
  exret <- function(subcomps, beta, marketclose){
    subcomps - (0.0002 + beta*(marketclose - 0.0002))
  }
  extrafinclean <- function(ebitdascol){
    if(grepl("B",as.character(ebitdascol))) {
      as.numeric(sub("B.*","",as.character(ebitdascol)))*1000
    } else if(grepl("M",as.character(ebitdascol))) {
      as.numeric(sub("M.*","",as.character(ebitdascol)))
    } else if(grepl("K",as.character(ebitdascol))) {
      as.numeric(sub("K.*","",as.character(ebitdascol)))/1000
    } else {
      as.numeric(as.character(ebitdascol))/1000000
    }
  }
  calcME <- fuction(indexlist, tcso){
    indexlist <- as.numeric(indexlist)
    mean(daily$close[indexlist])/tcso
  }
  evol <- function(ni1, ni2, ni3, ni4, tlse1, tlse2, tlse3, tlse4,tl1, tl2, tl3, tl4, rps1, 
                 rps2, rps3, rps4, nrps1, nrps2, nrps3, nrps4){
    val1 <- ni1/(tlse1 - tl1 - (rps1 + nrps1))
    val2 <- ni2/(tlse2 - tl2 - (rps2 + nrps2))
    val3 <- ni3/(tlse3 - tl3 - (rps3 + nrps3))
    val4 <- ni4/(tlse4 - tl4 - (rps4 + nrps4))
    sd(c(val1, val2, val3, val4), na.rm=TRUE)
  }

  ME <- mapply(calcME, splitindices, fstyear$TCSO)
  EBITDAS <- sapply(extrafin$ebitdas, extrafinclean)
  WC <- as.numeric(as.character(fstyear$TCA)) - as.numeric(as.character(fstyear$TCL))
  RE <- as.numeric(as.character(fstyear$NI)) - as.numeric(as.character(fstyear$DIVC))
  EBIT <- EBITDAS - as.numeric(as.character(fstyear$DP.DPL)) - as.numeric(as.character(fstyear$AM))
  SALE <- as.numeric(as.character(fstyear$TREV))

  #BAB scraped from web
  BAB <- extrafin$betas
  LEV <- mapply(lev, as.numeric(as.character(fstyear$TD)), as.numeric(as.character(fstyear$TA)))
  Z <- (1.2*WC + 1.4*RE + 3.3*EBIT + 0.6*ME + SALE)/(as.numeric(as.character(fstyear$TA)))

minimum <- min(c(length(cBS$TLSE), length(cIS$NI)))
tempvals <- numeric()
for(n in 1:minimum) {
  tempvals <- c(tempvals, as.numeric(cIS$NI[n])/(as.numeric(cBS$TLSE[n]) - 
                                                   as.numeric(cBS$TL[n]) - 
                                                   (as.numeric(cBS$RPS[n]) + as.numeric(cBS$NRPS[n]))))
}
  EVOL <- mapply(evol, fstyear)
= sd(tempvals)

  for(i in 1:numCompanies) {
    print(i/numCompanies)
    cBS <- BS[BS$ticker == x$tickers[i],]
    CIS <- IS[IS$ticker == x$tickers[i],]
    cCF <- CF[CF$ticker == x$tickers[i],]
#     cBS <- subset(BS,ticker == as.character(x$tickers[i]))
#     cIS <- subset(IS,ticker == as.character(x$tickers[i]))
#     cCF <- subset(CF,ticker == as.character(x$tickers[i]))
    
    if(nrow(cBS) > 0 && nrow(cIS) > 1 && nrow(cCF) > 0 && !is.na(extrafin$betas[i]) 
       && !is.na(extrafin$ebitdas[i]) && extrafin$ebitdas[i] != "N/A") {
      
      compdata <- daily[daily$ticker == as.character(x$tickers[i]),]
      #BAB[i] <- extrafin$betas[i]
    
      #IVOL 
      #sumvect <- numeric()
      market <- daily[daily$ticker == "GSPC",]
      market <- market[market$date == (as.character(cBS$year[1])),]
        #subset(subset(daily,ticker=="GSPC"),date == as.character(cBS$year[1]))
      subcomps <- compdata[compdata$date == (as.character(cBS$year[1])),]
      #subcomps <- subset(subset(daily,ticker==as.character(x$tickers[i])),date == as.character(cBS$year[1]))
      minsub <- min(c(length(market$ticker),length(subcomps$ticker)))
      # assume daily risk free return rate is 0.02 percent
      market <- market[1:minsub,]
      subcomps <- subcomps[1:minsub,]
      
      excess_return <- mapply(exret, as.numeric(as.character(subcomps$close)), BAB[i], market$close)
#       for(a in 1:minsub) {
#         excess_return <- as.numeric(as.character(subcomps$close[a])) - (0.0002 + extrafin$betas[i]*
#                                                                 (as.numeric(as.character(market$close[a])) -
#                                                                  0.0002))
#         sumvect <- c(sumvect,excess_return)
#       }
      #IVOL[i] <- sd(sumvect)
      IVOL[i] <- sd(excess_return)
      #LEV
      # -(total debt/total assets)
      #LEV[i] <- -(as.numeric(cBS$TD[1])/as.numeric(cBS$TA[1]))
    
      #O
      #ADJASSET = total assets + .1*(market equity - book equity)
      #CPI = 100
      #TLTA = (total debt - minority interest - preferred stock)/ADJASSET
      #WCTA = (current assets - current liabilities)/current assets
      #OENEG = 1 if total liabilities > total assets, 0 otherwise
      #NITA = net income/total assets
      #FUTL = income before tax/total liabilities
      #INTWO = 1 if neither the net income of the current year or 
      # nor the net income previous year is positive
      #CHIN = (net income current year - net income previous year)/
      # (|net income current year| + |net income previous year|)
#       sumvect2 <- numeric()
#       for(a in 1:length(subcomps)) {
#         sumvect2 <- c(sumvect2,as.numeric(as.character(daily$close[a])))
#       }
      # market equity
      # price of shares*number of shares
      
      #total assets + (.1 * (market equity - TLSE - TL + RPS + NRPS))
      #ME <- sumvect 2 <- all closing prices for that company for the most recent year.
      
      ADJASSET <- as.numeric(cBS$TA[1]) + 0.1*(ME - (as.numeric(cBS$TLSE[1]) - 
                                                   as.numeric(cBS$TL[1]) - 
                                                  (as.numeric(cBS$RPS[1]) + 
                                                   as.numeric(cBS$NRPS[1]))))
      TLTA <- (as.numeric(cBS$TD[1]) - as.numeric(cBS$MI[1])  - 
              (as.numeric(cBS$RPS[1]) + as.numeric(cBS$NRPS[1])))/
              ADJASSET
    
      WCTA <- (as.numeric(cBS$TCA[1]) - as.numeric(cBS$TCL[1]))/ADJASSET
      CLCA <- as.numeric(cBS$TCL[1])/as.numeric(cBS$TCA[1])
      OENEG <- as.numeric(as.numeric(cBS$TL[1]) > as.numeric(cBS$TA[1]))
      NITA <- as.numeric(cIS$NI[1])/as.numeric(cBS$TA[1])
      FUTL <- as.numeric(cIS$IBT[1])/as.numeric(cBS$TL[1])
      INTWO <- as.numeric(as.numeric(cIS$NI[1]) < 0 && as.numeric(cIS$NI[2]) < 0)
      CHIN <- (as.numeric(cIS$NI[1]) - as.numeric(cIS$NI[2]))/
              (abs(as.numeric(cIS$NI[1])) + abs(as.numeric(cIS$NI[2])))
      O[i] <- -(-1.32 - 0.407*log(ADJASSET/100) + 6.03*TLTA - 1.43*WCTA + 0.076*CLCA -
                 1.72*OENEG - 2.37*NITA - 1.83*FUTL + 0.285*INTWO - 0.521*CHIN)
      
      
    }
  }
  
  BAB[is.infinite(BAB)] <- 0
  IVOL[is.infinite(IVOL)] <- 0
  LEV[is.infinite(LEV)] <- 0
  O[is.infinite(O)] <- 0
  Z[is.infinite(Z)] <- 0
  EVOL[is.infinite(EVOL)] <- 0
  
  #Scale converts the individual scores for these values into z-scores.
  BAB <- scale(BAB)
  IVOL <- scale(IVOL)
  LEV <- scale(LEV)
  O <- scale(O)
  Z <- scale(Z)
  EVOL <- scale(EVOL)
  
  BAB[is.nan(BAB)] <- 0
  IVOL[is.nan(IVOL)] <- 0
  LEV[is.nan(LEV)] <- 0
  O[is.nan(O)] <- 0
  Z[is.nan(Z)] <- 0
  EVOL[is.nan(EVOL)] <- 0
  
  safety <- BAB + IVOL + LEV + O + Z + EVOL
  scale(safety)
}