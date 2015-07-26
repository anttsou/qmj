#' Collects payout z-scores for companies
#'
#' Given a data frame of companies (names and tickers) and a data frame of financial statements, 
#' calculates EISS, DISS, NPOP and determines the z-score of overall payout for each company based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A3-4.
#' @param companies A data frame of company names and tickers.
#' @param financials A data frame containing financial statements for every company.
#' @seealso \code{\link{market_data}}
#' @seealso \code{\link{market_profitability}}
#' @seealso \code{\link{market_growth}}
#' @seealso \code{\link{market_safety}}
#' @examples
#' companies <- qmjdata::companies[50:51,]
#' market_payouts(companies, qmjdata::financials)
#' @importFrom dplyr distinct arrange
#' @export

market_payouts <- function(companies = qmjdata::companies, financials = qmjdata::financials){
  if(length(companies$ticker) == 0) {
    stop("first parameter requires a ticker column.")
  }
  if(length(which(financials$TCSO < 0))) {
    stop("Negative TCSO exists.")
  }
  numCompanies <- length(companies$ticker)
  
  #set unavailable financial info to 0
  financials[is.na(financials)] <- 0
  
  #Function returns a structure that contains all elements in x.1 that are not in x.2
  modifiedsetdiff <- function(x.1,x.2,...){
    x.1p <- do.call("paste", x.1)
    x.2p <- do.call("paste", x.2)
    x.1[! x.1p %in% x.2p, ]
  }
  
  allcompanies <- data.frame(companies$ticker)
  colnames(allcompanies) <- "ticker"

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
  
  #functions calculate individual components of payouts
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
  
  #apply the calculation functions to all companies without needing a slow loop.
  EISS <- mapply(eiss, fstyear$TCSO, sndyear$TCSO)
  DISS <- mapply(diss, fstyear$TD, sndyear$TD)
  NPOP <- mapply(npop, fstyear$NI, sndyear$NI,
                 thdyear$NI, fthyear$NI, 
                 fstyear$TLSE, sndyear$TLSE,
                 thdyear$TLSE, fthyear$TLSE,
                 fstyear$TL, sndyear$TL,
                 thdyear$TL, fthyear$TL,
                 fstyear$RPS, sndyear$RPS,
                 thdyear$RPS, fthyear$RPS,
                 fstyear$NRPS, sndyear$NRPS,
                 thdyear$NRPS, fthyear$NRPS,
                 fstyear$GPROF, sndyear$GPROF,
                 thdyear$GPROF, fthyear$GPROF)

  #removes potential errors from Inf values
  EISS[is.infinite(EISS)] <- 0
  DISS[is.infinite(DISS)] <- 0
  NPOP[is.infinite(NPOP)] <- 0
  
  #scale converts the individual scores for these values into z-scores.
  EISS <- scale(EISS)
  DISS <- scale(DISS)
  NPOP <- scale(NPOP)
  
  #removes potential errors from nan values
  EISS[is.nan(EISS)] <- 0
  DISS[is.nan(DISS)] <- 0
  NPOP[is.nan(NPOP)] <- 0
  
  payouts <- EISS + DISS + NPOP
  
  payouts <- scale(payouts)
  data.frame(ticker = companies$ticker, 
             payouts = payouts, 
             EISS = EISS, 
             DISS = DISS,
             NPOP = NPOP)
}