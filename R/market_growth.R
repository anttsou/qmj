
#' Collects growth z-scores for companies
#'
#' Given a list of companies (names and tickers), a balance sheet, a cash flow statement,
#' and an income statement, calculates GPOA, ROE, ROA, CFOA, GMAR, ACC
#' and determines the z-score of overall growth based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param x A dataframe of company names and tickers.
#' @param financials A dataframe containing financial statements for every company.
#' @examples
#' data(companies)
#' data(financials)
#' market_growth(x, financials)
#' @export

#use sapply to make columns numeric
market_growth <- function(x, financials){
  allcompanies <- data.frame(x$ticker)
  colnames(allcompanies) <- "ticker"
  numCompanies <- length(x$tickers)
  
  financials[is.na(financials)] <- 0
  
  fin <- financials
  fin <- arrange(fin, desc(year))
  
  fstyear <- distinct_(fin, "ticker")
  fstyear <- merge(allcompanies, fstyear, by="ticker", all.x = TRUE)  

  fin <- arrange(fin, year)
  lstyear <- distinct_(fin, "ticker")
  lstyear <- merge(allcompanies, lstyear, by="ticker", all.x = TRUE)
  
  gpoa <- function(gprof1, gprof2, ta){
    (gprof1 - gprof2)/ta
  }
  roe <- function(ni1, ni2, tlse, tl, rps, nrps){
    (ni1 - ni2)/(tlse - tl - rps + nrps)
  }
  roa <- function(ni1, ni2, ta){
    (ni1 - ni2)/ta
  }
  cfoa <- function(ni1, dp1, cwc1, cx1, ni2, dp2, cwc2, cx2, ta){
    changeCF1 <- ni1 + dp1 - cwc1 - cx1
    changeCF2 <- ni2 + dp2 - cwc2 - cx2
    (changeCF1 - changeCF2)/ta
  }
  gmar <- function(gprof1, gprof2, trev){
    (gprof1 - gprof2)/trev
  }
  acc <- function(dp1, cwc1, dp2, cwc2, ta){
    accrual1 <- dp1 - cwc1
    accrual2 <- dp2 - cwc2
    (accrual1 - accrual2)/ta 
  }
  GPOA <- mapply(gpoa, as.numeric(as.character(fstyear$GPROF)), as.numeric(as.character(lstyear$GPROF)), 
                 as.numeric(as.character(lstyear$TA)))
  #Note: ROE is almost entirely NA's.
  ROE <- mapply(roe, as.numeric(as.character(fstyear$NI)), as.numeric(as.character(fstyear$NI)), 
                as.numeric(as.character(lstyear$TLSE)), as.numeric(as.character(lstyear$TL)), 
                as.numeric(as.character(lstyear$RPS)), as.numeric(as.character(lstyear$NRPS)))
  ROA <- mapply(roa, as.numeric(as.character(fstyear$NI)), as.numeric(as.character(lstyear$NI)), 
                as.numeric(as.character(lstyear$TA)))
  CFOA <- mapply(cfoa, as.numeric(as.character(fstyear$NI)), as.numeric(as.character(fstyear$DP.DPL)), 
                 as.numeric(as.character(fstyear$CWC)), as.numeric(as.character(fstyear$CX)),
                 as.numeric(as.character(lstyear$NI)), as.numeric(as.character(lstyear$DP.DPL)),
                 as.numeric(as.character(lstyear$CWC)), as.numeric(as.character(lstyear$CX)),
                 as.numeric(as.character(lstyear$TA)))
  GMAR <- mapply(gmar, as.numeric(as.character(fstyear$GPROF)), as.numeric(as.character(lstyear$GPROF)),
                 as.numeric(as.character(lstyear$TREV)))
  ACC <- mapply(acc, as.numeric(as.character(fstyear$DP.DPL)), as.numeric(as.character(fstyear$CWC)),
                as.numeric(as.character(lstyear$DP.DPL)), as.numeric(as.character(lstyear$CWC)),
                as.numeric(as.character(lstyear$TA)))
  
  GPOA[is.infinite(GPOA)] <- 0
  ROE[is.infinite(ROE)] <- 0
  ROA[is.infinite(ROA)] <- 0
  CFOA[is.infinite(CFOA)] <- 0
  GMAR[is.infinite(GMAR)] <- 0
  ACC[is.infinite(ACC)] <- 0
  
  #Scale converts the individual scores for these values into z-scores.
  GPOA <- scale(GPOA)
  ROE <- scale(ROE)
  ROA <- scale(ROA)
  CFOA <- scale(CFOA)
  GMAR <- scale(GMAR)
  ACC <- scale(ACC)

  GPOA[is.nan(GPOA)] <- 0
  ROE[is.nan(ROE)] <- 0
  ROA[is.nan(ROA)] <- 0
  CFOA[is.nan(CFOA)] <- 0
  GMAR[is.nan(GMAR)] <- 0
  ACC[is.nan(ACC)] <- 0

  growth <- GPOA + ROE + ROA + CFOA + GMAR + ACC
  growth <- scale(growth)
  data.frame(ticker = x$ticker, 
             growth = growth, 
             GPOA = GPOA, 
             ROE = ROA,
             CFOA = CFOA, 
             GMAR = GMAR,
             ACC = ACC)
}