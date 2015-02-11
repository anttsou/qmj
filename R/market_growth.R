
#' Collects growth z-scores for companies
#'
#' Given a data frame of companies (names and tickers) and a data frame of financial statements, 
#' calculates GPOA, ROE, ROA, CFOA, GMAR, ACC over a four-year time span
#' and determines the z-score of overall growth for each company based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param companies A data frame of company names and tickers.
#' @param financials A data frame containing financial statements for every company.
#' @seealso \code{\link{market_data}}
#' @seealso \code{\link{market_profitability}}
#' @seealso \code{\link{market_safety}}
#' @seealso \code{\link{market_payout}}
#' @examples
#' data(companies)
#' data(financials)
#' market_growth(companies, financials)
#' @export

market_growth <- function(x, financials){
  if(length(x$ticker) == 0) {
    stop("first parameter requires a ticker column.")
  }
  if(length(which(financials$TCSO < 0))) {
    stop("Negative TCSO exists.")
  }
  allcompanies <- data.frame(x$ticker)
  colnames(allcompanies) <- "ticker"
  numCompanies <- length(x$tickers)
  
  #set unavailable financial info to 0
  financials[is.na(financials)] <- 0
  
  fin <- financials
  fin <- arrange(fin, desc(year))
  
  fstyear <- distinct_(fin, "ticker")
  fstyear <- merge(allcompanies, fstyear, by="ticker", all.x = TRUE)  

  fin <- arrange(fin, year)
  lstyear <- distinct_(fin, "ticker")
  lstyear <- merge(allcompanies, lstyear, by="ticker", all.x = TRUE)
  
  #functions calculate individual components of growth
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
  
  #apply the calculation functions to all companies without using a slow loop.
  GPOA <- mapply(gpoa, as.numeric(as.character(fstyear$GPROF)), as.numeric(as.character(lstyear$GPROF)), 
                 as.numeric(as.character(lstyear$TA)))

  ROE <- mapply(roe, as.numeric(as.character(fstyear$NI)), as.numeric(as.character(lstyear$NI)), 
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
  
  #remove potential errors from Inf values
  GPOA[is.infinite(GPOA)] <- 0
  ROE[is.infinite(ROE)] <- 0
  ROA[is.infinite(ROA)] <- 0
  CFOA[is.infinite(CFOA)] <- 0
  GMAR[is.infinite(GMAR)] <- 0
  ACC[is.infinite(ACC)] <- 0
  
  #scale converts the individual scores for these values into z-scores.
  GPOA <- scale(GPOA)
  ROE <- scale(ROE)
  ROA <- scale(ROA)
  CFOA <- scale(CFOA)
  GMAR <- scale(GMAR)
  ACC <- scale(ACC)
  
  #remove potential errors from nan values
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
             ROE = ROE,
             ROA = ROA,
             CFOA = CFOA, 
             GMAR = GMAR,
             ACC = ACC)
}