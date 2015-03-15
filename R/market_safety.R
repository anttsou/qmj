#' Collects safety z-scores for companies
#'
#' Given a data frame of companies (names and tickers), a data frame of financial
#' statements, and a data frame of daily price data, calculates BAB, IVOL, LEV, O, Z, and EVOL
#' and determines the z-score of overall safety for each company based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param companies A data frame of company names and tickers.
#' @param financials A data frame containing financial statements for every company.
#' @param prices A data frame containing the daily market closing prices and returns. 
#' @seealso \code{\link{market_data}}
#' @seealso \code{\link{market_profitability}}
#' @seealso \code{\link{market_growth}}
#' @seealso \code{\link{market_payouts}}
#' @examples
#' data(companies)
#' data(financials)
#' data(prices)
#' market_safety(companies, financials, prices)
#' @importFrom dplyr filter distinct arrange
#' @export

market_safety <- function(x, financials, daily){
  if(length(x$ticker) == 0) {
    stop("first parameter requires a ticker column.")
  }
  if(length(which(financials$TCSO < 0))) {
    stop("Negative TCSO exists.")
  }
  filepath <- system.file("data", package="qmj")
  numCompanies <- length(x$ticker)
  allcompanies <- data.frame(x$ticker)
  colnames(allcompanies) <- "ticker"
  
  #set unavailable financial info to 0
  financials[is.na(financials)] <- 0
  
  #set unavailable price data to 0
  daily[is.na(daily)] <- 0
  
  daily$pret[is.nan(daily$pret)] <- 0
  daily$pret[is.infinite(daily$pret)] <- 0
  currentyear <- as.numeric(format(Sys.Date(), "%Y"))
  market <- dplyr::filter(daily, ticker == "GSPC")
  nogspc <- dplyr::filter(daily, ticker != "GSPC")
  year <- numeric()
  if(sum(as.numeric(sub("-.*","",market$date)) == currentyear) <= 150){
    year <- currentyear - 1
  } else{
    year <- currentyear
  }
  marketlistb <- market[grepl(year,market$date),]
  mergedail <- merge(marketlistb,nogspc,by="date")
  splitdail <- split(mergedail,mergedail$ticker.y)
  splitindices <- split(seq(nrow(daily)), daily$ticker)  # Stores list of indices for a company ticker.
  splitindices <- splitindices[-1]
  companiesstored <- names(splitindices)
  
  modifiedsetdiff <- function(x.1,x.2,...){
    x.1p <- do.call("paste", x.1[,1:5])
    x.2p <- do.call("paste", x.2[,1:5])
    x.1[! x.1p %in% x.2p, ]
  }
  
  fin <- financials
  fin <- dplyr::arrange(financials, desc(year))
  fstyear <- dplyr::distinct_(fin, "ticker")
  
  fin <- modifiedsetdiff(fin, fstyear)
  sndyear <- dplyr::distinct_(fin, "ticker")
  
  fin <- modifiedsetdiff(fin, sndyear)
  thdyear <- dplyr::distinct_(fin, "ticker")
  
  fthyear <- modifiedsetdiff(fin, thdyear)
  fthyear <- dplyr::distinct_(fthyear, "ticker")
  
  #Forces all data frames to have the same number of rows.
  fstyear <- merge(allcompanies, fstyear, by="ticker", all.x = TRUE)
  sndyear <- merge(allcompanies, sndyear, by="ticker", all.x = TRUE)
  thdyear <- merge(allcompanies, thdyear, by='ticker', all.x = TRUE)
  fthyear <- merge(allcompanies, fthyear, by='ticker', all.x = TRUE)
  
  #functions calculate individual components of safety
  merger <- function(company_ticker) {
    result <- -(cov(as.numeric(as.character(splitdail[[company_ticker]]$pret.y)),
                    as.numeric(as.character(splitdail[[company_ticker]]$pret.x)))/
                var(as.numeric(as.character(splitdail[[company_ticker]]$pret.x))))
    if(is.na(result)) {
      warning(paste(paste("BAB for",company_ticker,sep=" "),"generated NA",sep=" "))
    }
    result
  }
  calc_ivol <- function(company_ticker) {
    if(length(splitdail[[company_ticker]]) > 0) {
      lmobj <- lm(as.numeric(as.character(splitdail[[company_ticker]]$pret.y))~
                    as.numeric(as.character(splitdail[[company_ticker]]$pret.x)))
      -(sd(residuals(lmobj)))
    } else {
      warning(paste(paste("IVOL for",company_ticker,sep=" "),"generated NA",sep=" "))
      NA
    }
  }
  lev <- function(td, ta){
    -td/ta
  }

  calcmean <- function(indexlist){
    indexlist <- as.numeric(indexlist)
    closingprices <- daily$close[indexlist]
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

  #apply the calculation functions to all companies without needing a slow loop.
  BAB <- sapply(x$ticker, merger)
 
  IVOL <- sapply(x$ticker,calc_ivol)

  LEV <- mapply(lev, fstyear$TD, fstyear$TA)
  
  closingmeans <- sapply(splitindices, calcmean)
  tempframe <- data.frame(companiesstored, closingmeans)
  colnames(tempframe) <- c("ticker", "close")
  tempframe <- merge(allcompanies, tempframe, by='ticker', all.x = TRUE)  
  
  ME <- mapply(marketequity, tempframe$close, fstyear$TCSO)
  WC <- fstyear$TCA - fstyear$TCL
  RE <- fstyear$NI - (fstyear$DIVC * fstyear$TCSO)
  EBIT <- fstyear$NI - fstyear$DO + (fstyear$IBT - fstyear$IAT) + fstyear$NINT
  SALE <- fstyear$TREV
  Z <- (1.2*WC + 1.4*RE + 3.3*EBIT + 0.6*ME + SALE)/(fstyear$TA)
  
  EVOL <- mapply(evol, fstyear$NI, sndyear$NI,
                 thdyear$NI, fthyear$NI, 
                 fstyear$TLSE, sndyear$TLSE,
                 thdyear$TLSE, fthyear$TLSE,
                 fstyear$TL, sndyear$TL,
                 thdyear$TL, fthyear$TL,
                 fstyear$RPS, sndyear$RPS,
                 thdyear$RPS, fthyear$RPS,
                 fstyear$NRPS, sndyear$NRPS,
                 thdyear$NRPS, fthyear$NRPS)
  
  ADJASSET <- fstyear$TA + (0.1*(ME - (fstyear$TLSE
                                    - fstyear$TL)
                                    - fstyear$RPS
                                    - fstyear$NRPS))
  TLTA <- (fstyear$TD - fstyear$NI - 
           fstyear$RPS - fstyear$NRPS)/ADJASSET
  WCTA <- (fstyear$TCA - fstyear$TCL)/ADJASSET
  CLCA <- fstyear$TCL/fstyear$TCA
  OENEG <- fstyear$NI > fstyear$TA
  NITA <- fstyear$NI/fstyear$TA
  FUTL <- fstyear$IBT/fstyear$TL
  INTWO <- mapply(intwo, fstyear$NI, sndyear$NI)
  CHIN <- (fstyear$NI - sndyear$NI)/
    (abs(fstyear$NI) + abs(sndyear$NI))
  O <- -(-1.32 - 0.407*log(ADJASSET/100) + 6.03*TLTA - 1.43*WCTA + 0.076*CLCA -
           1.72*OENEG - 2.37*NITA - 1.83*FUTL + 0.285*INTWO - 0.521*CHIN)

  #removes potential errors from Inf values
  BAB[is.infinite(BAB)] <- 0
  IVOL[is.infinite(IVOL)] <- 0
  LEV[is.infinite(LEV)] <- 0
  O[is.infinite(O)] <- 0
  Z[is.infinite(Z)] <- 0
  EVOL[is.infinite(EVOL)] <- 0
  
  #scale converts the individual scores for these values into z-scores.
  BAB <- scale(BAB)
  IVOL <- scale(IVOL)
  LEV <- scale(LEV)
  O <- scale(O)
  Z <- scale(Z)
  EVOL <- scale(EVOL)
  
  #removes potential errors from nan values
  BAB[is.nan(BAB)] <- 0
  IVOL[is.nan(IVOL)] <- 0
  LEV[is.nan(LEV)] <- 0
  O[is.nan(O)] <- 0
  Z[is.nan(Z)] <- 0
  EVOL[is.nan(EVOL)] <- 0
  
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