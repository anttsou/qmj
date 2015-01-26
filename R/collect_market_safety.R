#' Collects safety z-score for companies
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
#' @examples
#' x <- data(companies)
#' BS <- data(tidybalance)
#' CF <- data(tidycash)
#' IS <- data(tidyincome)
#' \dontrun{collect_market_safety(x, BS, CF, IS)}
#' @export

collect_market_safety <- function(x, BS, CF, IS, extrafin, daily){
  #Is there a better way to do this than calling "library(data.table)?"
  library(data.table)
  
  filepath <- system.file("data", package="qmj")
  numCompanies <- length(x$tickers)
  allcompanies <- data.frame(x$tickers)
  colnames(allcompanies) <- "ticker"
  
#   safety <- rep(0, numCompanies)
#   BAB <- rep(0, numCompanies)
#   IVOL <- rep(0, numCompanies)
#   LEV <- rep(0, numCompanies)
#   O <- rep(0, numCompanies)
#   Z <- rep(0, numCompanies)
#   EVOL <- rep(0, numCompanies)
  BS[is.na(BS)] <- 0
  IS[is.na(IS)] <- 0
  CF[is.na(CF)] <- 0
  currentyear <- as.numeric(format(Sys.Date(), "%Y"))
  daily$date <- sub("-.*","",daily$date)
  daily <- data.table(daily, key="ticker")
  ordereddaily <- daily[order(daily$date, decreasing=TRUE),]
  splitindices <- split(seq(nrow(daily)), daily$ticker)  # Stores list of indices for a company ticker.
  companiesstored <- names(splitindices)
  setkey(ordereddaily, "ticker")
  yearlyprices <- unique(ordereddaily)
  market <- daily[daily$ticker == "GSPC",]
  marketlist <- list(daily[daily$ticker == "GSPC",])
  
  modifiedsetdiff <- function(x.1,x.2,...){
    x.1p <- do.call("paste", x.1)
    x.2p <- do.call("paste", x.2)
    x.1[! x.1p %in% x.2p, ]
  }
  
  fin <- merge(BS, merge(CF, IS, by=c("ticker", "year")), by=c("ticker", "year"))
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

  lev <- function(td, ta){
    -td/ta
  }
  exret <- function(subcomps, beta, marketclose){
    subcomps - (beta*marketclose)
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
  calcmean <- function(indexlist){
    indexlist <- as.numeric(indexlist)
    mean(daily$close[indexlist])
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
  ivol <- function(refineddat, betas){
    market <- refineddat[[1]]
    market <- as.numeric(as.character(market$close))
    stock <- refineddat[[2]]
    stock <- as.numeric(as.character(stock$close))
    excess_return <- mapply(exret, stock, betas, market)
    sd(excess_return)
  }
  refine_ivol_data <- function(marketdat, stockindices){
    #stockindices <- as.numeric(stockindices)
    stockdat <- daily[stockindices,]
    if(sum(marketdat$date == currentyear) <= 150){
      year <- currentyear - 1
    } else{
      year <- currentyear
    }
    #marketdat <- marketdat[[1]]
    marketdat <- marketdat[marketdat$date == year,]
    stockdat <- stockdat[stockdat$date == year,]
    smallersize <- min(c(length(marketdat$date), length(stockdat$date)))
    marketdat <- marketdat[1:smallersize,]
    stockdat <- stockdat[1:smallersize,]
    list(marketdat, stockdat)
  }
  intwo <- function(ni1, ni2){
    as.numeric(ni1 > 0 && ni2 > 0)
  }

  #BAB scraped from web
  BAB <- extrafin$betas
  
#   refined_data <- mapply(refine_ivol_data, market, splitindices)
#   tempframe <- data.table(companiesstored, refined_data)
#   colnames(tempframe) <- c("ticker", "refined")
#   tempframe <- merge(allcompanies, tempframe, by='ticker', all.x = TRUE)
#   IVOL <- mapply(ivol, tempframe$refined, BAB)
#   print(head(IVOL))
  LEV <- mapply(lev, as.numeric(as.character(fstyear$TD)), as.numeric(as.character(fstyear$TA)))

  closingmeans <- sapply(splitindices, calcmean)
  tempframe <- data.frame(companiesstored, closingmeans)
  colnames(tempframe) <- c("ticker", "close")
  tempframe <- merge(allcompanies, tempframe, by='ticker', all.x = TRUE)  
  
  ME <- mapply(marketequity, as.numeric(as.character(tempframe$close)), as.numeric(as.character(fstyear$TCSO)))
  EBITDAS <- sapply(extrafin$ebitdas, extrafinclean)
  WC <- as.numeric(as.character(fstyear$TCA)) - as.numeric(as.character(fstyear$TCL))
  RE <- as.numeric(as.character(fstyear$NI)) - (as.numeric(as.character(fstyear$DIVC)) * as.numeric(as.character(fstyear$TCSO)))
  EBIT <- EBITDAS - as.numeric(as.character(fstyear$DP.DPL)) - as.numeric(as.character(fstyear$AM))
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
  
  BAB[is.infinite(BAB)] <- 0
  #IVOL[is.infinite(IVOL)] <- 0
  LEV[is.infinite(LEV)] <- 0
  O[is.infinite(O)] <- 0
  Z[is.infinite(Z)] <- 0
  EVOL[is.infinite(EVOL)] <- 0
  
  #Scale converts the individual scores for these values into z-scores.
  BAB <- scale(BAB)
  #IVOL <- scale(IVOL)
  LEV <- scale(LEV)
  O <- scale(O)
  Z <- scale(Z)
  EVOL <- scale(EVOL)
  
  BAB[is.na(BAB)] <- 0
  #IVOL[is.nan(IVOL)] <- 0
  LEV[is.na(LEV)] <- 0
  O[is.na(O)] <- 0
  Z[is.na(Z)] <- 0
  EVOL[is.na(EVOL)] <- 0
  
  #safety <- BAB + IVOL + LEV + O + Z + EVOL
  safety <- BAB + LEV + O + Z + EVOL
  safety <- scale(safety)
  data.frame(x$tickers, safety, BAB, LEV, O, Z, EVOL)
}