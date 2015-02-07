#' Collects safety z-scores for companies
#'
#' Given a list of companies (names and tickers), a balance sheet, a cash flow statement,
#' and an income statement, calculates BAB, IVOL, LEV, O, Z, and EVOL.
#' and determines the z-score of overall profitability based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param x A dataframe of company names and tickers.
#' @param financials A dataframe containing financial statements for every company.
#' @param daily A dataframe containing the daily market closing prices and returns. 
#' @examples
#' data(companies)
#' data(financials)
#' data(extrafin)
#' data(tidydaily)
#' x <- companies
#' financials <- financials
#' daily <- tidydaily
#' market_safety(x, financials, daily)
#' @export

market_safety <- function(x, financials, daily){
  filepath <- system.file("data", package="qmj")
  numCompanies <- length(x$ticker)
  allcompanies <- data.frame(x$ticker)
  colnames(allcompanies) <- "ticker"
  financials[is.na(financials)] <- 0
  daily[is.na(daily)] <- 0
  daily$pret[is.nan(as.numeric(daily$pret))] <- 0
  daily$pret[is.infinite(as.numeric(daily$pret))] <- 0
  currentyear <- as.numeric(format(Sys.Date(), "%Y"))
  market <- filter(daily, ticker == "GSPC")
    #daily[daily$ticker == "GSPC",]
  nogspc <- filter(daily, ticker != "GSPC")
  year <- numeric()
  if(sum(market$date == currentyear) <= 150){
    year <- currentyear - 1
  } else{
    year <- currentyear
  }
  marketlistb <- market[grepl(year,market$date),]
  mergedail <- merge(marketlistb,nogspc,by="date")
  splitdail <- split(mergedail,mergedail$ticker.y)
  ordereddaily <- arrange(daily, desc(date))
    #daily[order(daily$date, decreasing=TRUE),]
  splitindices <- split(seq(nrow(daily)), daily$ticker)  # Stores list of indices for a company ticker.
  splitindices <- splitindices[-1]
  companiesstored <- names(splitindices)
  #setkey(ordereddaily, "ticker")
  yearlyprices <- distinct_(ordereddaily, "ticker")
    #unique(ordereddaily)
  
  modifiedsetdiff <- function(x.1,x.2,...){
    x.1p <- do.call("paste", x.1[,1:5])
    x.2p <- do.call("paste", x.2[,1:5])
    x.1[! x.1p %in% x.2p, ]
  }
  
  fin <- financials
  fin <- arrange(financials, desc(year))
  fstyear <- distinct_(fin, "ticker")
  
  fin <- modifiedsetdiff(fin, fstyear)
  sndyear <- distinct_(fin, "ticker")
  
  fin <- modifiedsetdiff(fin, sndyear)
  thdyear <- distinct_(fin, "ticker")
  
  fthyear <- modifiedsetdiff(fin, thdyear)
  fthyear <- distinct_(fthyear, "ticker")
  
  #Forces all data frames to have the same number of rows.
  fstyear <- merge(allcompanies, fstyear, by="ticker", all.x = TRUE)
  sndyear <- merge(allcompanies, sndyear, by="ticker", all.x = TRUE)
  thdyear <- merge(allcompanies, thdyear, by='ticker', all.x = TRUE)
  fthyear <- merge(allcompanies, fthyear, by='ticker', all.x = TRUE)
  

  merger <- function(company_ticker) {
    -(cov(as.numeric(as.character(splitdail[[company_ticker]]$pret.y)),
        as.numeric(as.character(splitdail[[company_ticker]]$pret.x)))/
      var(as.numeric(as.character(splitdail[[company_ticker]]$pret.x))))
  }
  calc_ivol <- function(company_ticker) {
    #print(length(splitdail[[company_ticker]]))
    if(length(splitdail[[company_ticker]]) > 0) {
      lmobj <- lm(as.numeric(as.character(splitdail[[company_ticker]]$pret.y))~
                    as.numeric(as.character(splitdail[[company_ticker]]$pret.x)))
      -(sd(residuals(lmobj)))
    } else {
      NA
    }
  }
  lev <- function(td, ta){
    -td/ta
  }
  exret <- function(subcomps, beta, marketclose){
    subcomps - (beta*marketclose)
  }
  calcmean <- function(indexlist){
    indexlist <- as.numeric(indexlist)
    closingprices <- as.numeric(as.character(daily$close[indexlist]))
    mean(closingprices)
  }
  marketequity <- function(closemeans, tcso){
    closemeans/tcso
  }
  evol <- function(ni1, ni2, ni3, ni4, tlse1, tlse2, tlse3, tlse4,tl1, tl2, tl3, tl4, rps1, 
                   rps2, rps3, rps4, nrps1, nrps2, nrps3, nrps4){
    val1 <- ni1/(tlse1 - tl1 - (rps1 + nrps1))
    val2 <- ni2/(tlse2 - tl2 - (rps2 + nrps2))
    val3 <- ni3/(tlse3 - tl3 - (rps3 + nrps3))
    val4 <- ni4/(tlse4 - tl4 - (rps4 + nrps4))
    sd(c(val1, val2, val3, val4), na.rm=TRUE)
  }

  intwo <- function(ni1, ni2){
    as.numeric(ni1 > 0 && ni2 > 0)
  }
  
  #BAB calculated in merger
  BAB <- sapply(x$ticker, merger)
  #BAB <- sapply(as.character(allcompanies$ticker), merger)
  
  IVOL <- sapply(x$ticker,calc_ivol)
  #IVOL <- sapply(as.character(allcompanies$ticker), calc_ivol)
  #   print(head(IVOL))
  LEV <- mapply(lev, as.numeric(as.character(fstyear$TD)), as.numeric(as.character(fstyear$TA)))
  
  closingmeans <- sapply(splitindices, calcmean)
  tempframe <- data.frame(companiesstored, closingmeans)
  colnames(tempframe) <- c("ticker", "close")
  tempframe <- merge(allcompanies, tempframe, by='ticker', all.x = TRUE)  
  
  ME <- mapply(marketequity, as.numeric(as.character(tempframe$close)), as.numeric(as.character(fstyear$TCSO)))
  #EBITDAS <- as.numeric(as.character(fstyear$NI)) - as.numeric(as.character(fstyear$))
  WC <- as.numeric(as.character(fstyear$TCA)) - as.numeric(as.character(fstyear$TCL))
  RE <- as.numeric(as.character(fstyear$NI)) - (as.numeric(as.character(fstyear$DIVC)) * as.numeric(as.character(fstyear$TCSO)))
  EBIT <- as.numeric(as.character(fstyear$NI)) - as.numeric(as.character(fstyear$DO)) + (as.numeric(as.character(fstyear$IBT)) - as.numeric(as.character(fstyear$IAT))) + as.numeric(as.character(fstyear$NINT))
  SALE <- as.numeric(as.character(fstyear$TREV))
  Z <- (1.2*WC + 1.4*RE + 3.3*EBIT + 0.6*ME + SALE)/(as.numeric(as.character(fstyear$TA)))
  
  EVOL <- mapply(evol, as.numeric(as.character(fstyear$NI)), as.numeric(as.character(sndyear$NI)),
                 as.numeric(as.character(thdyear$NI)), as.numeric(as.character(fthyear$NI)), 
                 as.numeric(as.character(fstyear$TLSE)), as.numeric(as.character(sndyear$TLSE)),
                 as.numeric(as.character(thdyear$TLSE)), as.numeric(as.character(fthyear$TLSE)),
                 as.numeric(as.character(fstyear$TL)), as.numeric(as.character(sndyear$TL)),
                 as.numeric(as.character(thdyear$TL)), as.numeric(as.character(fthyear$TL)),
                 as.numeric(as.character(fstyear$RPS)), as.numeric(as.character(sndyear$RPS)),
                 as.numeric(as.character(thdyear$RPS)), as.numeric(as.character(fthyear$RPS)),
                 as.numeric(as.character(fstyear$NRPS)), as.numeric(as.character(sndyear$NRPS)),
                 as.numeric(as.character(thdyear$NRPS)), as.numeric(as.character(fthyear$NRPS)))
  
  ADJASSET <- as.numeric(as.character(fstyear$TA)) + 0.1*(ME - (as.numeric(as.character(fstyear$TLSE))
                                                                - as.numeric(as.character(fstyear$TL))
                                                                - as.numeric(as.character(fstyear$RPS))
                                                                - as.numeric(as.character(fstyear$NRPS))))
  TLTA <- (as.numeric(as.character(fstyear$TD)) - as.numeric(as.character(fstyear$NI)) - 
             as.numeric(as.character(fstyear$RPS)) - as.numeric(as.character(fstyear$NRPS)))/ADJASSET
  WCTA <- (as.numeric(as.character(fstyear$TCA)) - as.numeric(as.character(fstyear$TCL)))/ADJASSET
  CLCA <- as.numeric(as.character(fstyear$TCL))/as.numeric(as.character(fstyear$TCA))
  OENEG <- as.numeric(as.character(fstyear$NI)) > as.numeric(as.character(fstyear$TA))
  NITA <- as.numeric(as.character(fstyear$NI))/as.numeric(as.character(fstyear$TA))
  FUTL <- as.numeric(as.character(fstyear$IBT))/as.numeric(as.character(fstyear$TL))
  INTWO <- mapply(intwo, as.numeric(as.character(fstyear$NI)), as.numeric(as.character(sndyear$NI)))
  CHIN <- (as.numeric(as.character(fstyear$NI)) - as.numeric(as.character(sndyear$NI)))/
    (abs(as.numeric(as.character(fstyear$NI))) + abs(as.numeric(as.character(sndyear$NI))))
  O <- -(-1.32 - 0.407*log(ADJASSET/100) + 6.03*TLTA - 1.43*WCTA + 0.076*CLCA -
           1.72*OENEG - 2.37*NITA - 1.83*FUTL + 0.285*INTWO - 0.521*CHIN)
  #length(BAB) <- numCompanies
  #length(IVOL) <- numCompanies
  #length(LEV) <- numCompanies
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
  
  BAB[is.na(BAB)] <- 0
  IVOL[is.nan(IVOL)] <- 0
  LEV[is.na(LEV)] <- 0
  O[is.na(O)] <- 0
  Z[is.na(Z)] <- 0
  EVOL[is.na(EVOL)] <- 0
  
  safety <- BAB + IVOL + LEV + O + Z + EVOL
  safety <- scale(safety)
  data.frame(ticker = x$ticker, 
             safety = safety, 
             BAB = BAB, 
             IVOL = IVOL,
             LEV = LEV, 
             O = O, 
             Z = Z, 
             EVOL = EVOL)
}