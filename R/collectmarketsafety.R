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
  # CollectMarketSafety collects data on overall safety
  ## In the market for individual companies for later processing.
  ## x is the list of companies to be processed. BS, CF, IS are financial statements.
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
  
  for(i in 1:numCompanies) {
    print(i/numCompanies)
    cBS <- subset(BS,ticker == as.character(x$tickers[i]))
    cIS <- subset(IS,ticker == as.character(x$tickers[i]))
    cCF <- subset(CF,ticker == as.character(x$tickers[i]))
    
    if(nrow(cBS) > 0 && nrow(cIS) > 1 && nrow(cCF) > 0 && !is.na(extrafin$betas[i]) 
       && !is.na(extrafin$ebitdas[i]) && extrafin$ebitdas[i] != "N/A") {
      #BAB scraped from web
      BAB[i] <- extrafin$betas[i]
    
      #IVOL 
      sumvect <- numeric()
      market <- subset(subset(daily,ticker=="GSPC"),date == as.character(cBS$year[1]))
      subcomps <- subset(subset(daily,ticker==as.character(x$tickers[i])),date == as.character(cBS$year[1]))
      minsub <- min(c(length(market$ticker),length(subcomps$ticker)))
      # assume daily risk free return rate is 0.02 percent
      for(a in 1:minsub) {
        excess_return <- as.numeric(as.character(subcomps$close[a])) - (0.0002 + extrafin$betas[i]*
                                                                (as.numeric(as.character(market$close[a])) -
                                                                 0.0002))
        sumvect <- c(sumvect,excess_return)
      }
      IVOL[i] <- sd(sumvect)
      #LEV
      # -(total debt/total assets)
      LEV[i] <- -(as.numeric(cBS$TD[1])/as.numeric(cBS$TA[1]))
    
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
      sumvect2 <- numeric()
      for(a in 1:length(subcomps)) {
        sumvect2 <- c(sumvect2,as.numeric(as.character(daily$close[a])))
      }
      # market equity
      # price of shares*number of shares
      ME <- mean(sumvect2)*as.numeric(cBS$TCSO[1])
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
      
      # working capital = current assets - current liabilities
      WC <- as.numeric(cBS$TCA[1]) - as.numeric(cBS$TCL[1])
      
      # retained earnings = beginning earnings + net income - dividends
      # dividends = dividends per share * total number of shares
      RE <- as.numeric(cIS$NI[1]) - as.numeric(cIS$DIVC[1])
      
      # earnings before interest and taxes = net income + interest + taxes
      ###taxes???
      value <- numeric()
      if(grepl("B",as.character(extrafin$ebitdas[i]))) {
        value <- as.numeric(sub("B.*","",as.character(extrafin$ebitdas[i])))*1000
      } else if(grepl("M",as.character(extrafin$ebitdas[i]))) {
        value <- as.numeric(sub("M.*","",as.character(extrafin$ebitdas[i])))
      } else if(grepl("K",as.character(extrafin$ebitdas[i]))) {
        value <- as.numeric(sub("K.*","",as.character(extrafin$ebitdas[i])))/1000
      } else {
        value <- as.numeric(as.character(extrafin$ebitdas[i]))/1000000
      }
      EBIT <- value - as.numeric(cCF$DP[1]) - as.numeric(cCF$AM[1])
      
      # total sales 
      SALE <- as.numeric(cIS$TREV[1])
      
      Z[i] <- (1.2*WC + 1.4*RE + 3.3*EBIT + 0.6*ME + SALE)/(as.numeric(cBS$TA[1]))
      
      minimum <- min(c(length(cBS$TLSE), length(cIS$NI)))
      tempvals <- numeric()
      for(n in 1:minimum) {
        tempvals <- c(tempvals, as.numeric(cIS$NI[n])/(as.numeric(cBS$TLSE[n]) - 
                                as.numeric(cBS$TL[n]) - 
                               (as.numeric(cBS$RPS[n]) + as.numeric(cBS$NRPS[n]))))
      }
      EVOL[i] = sd(tempvals)
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