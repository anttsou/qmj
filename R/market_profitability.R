#' Collects profitability z-scores for companies
#'
#' Given a data frame of companies (names and tickers) and a data frame of financial statements, 
#' calculates GPOA, ROE, ROA, CFOA, GMAR, ACC and determines the z-score of overall profitability 
#' for each company based on the paper Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param companies A data frame of company names and tickers.
#' @param financials A data frame containing financial statements for every company.
#' @examples
#' data(companies)
#' data(financials)
#' market_profitability(companies, financials)
#' @export

#use sapply to make columns numeric
market_profitability <- function(x, financials){ 
  if(length(x$ticker) == 0) {
    stop("first parameter requires a ticker column.")
  }
  if(length(which(financials$TCSO < 0))) {
    stop("Negative TCSO exists.")
  }
  numCompanies <- length(x$ticker)
  
  financials[is.na(financials)] <- 0
  
  allcompanies <- data.frame(x$ticker)
  colnames(allcompanies) <- "ticker"
  fin <- financials
  fin <- arrange(fin, desc(year))
  fin <- distinct_(fin, "ticker")
  fin <- merge(allcompanies, fin, by="ticker", all.x = TRUE)
  
  gpoa <- function(gprof, ta){
    gprof/ta
  }
  roe <- function(ni, tlse, tl, rps, nrps){
    ni/(tlse - tl - (rps + nrps))
  }
  roa <- function(ni, ta){
    ni/ta
  }
  cfoa <- function(ni, dp, cwc, cx, ta){
    (ni + dp - cwc - cx)/ta
  }
  gmar <- function(gprof, trev){
    gprof/trev
  }
  acc <- function(dp, cwc, ta){
    (dp - cwc)/ta
  }
  
  GPOA <- mapply(gpoa, as.numeric(as.character(fin$GPROF)), as.numeric(as.character(fin$TA)))
  ROE <- mapply(roe, as.numeric(as.character(fin$NI)), as.numeric(as.character(fin$TLSE)), 
                as.numeric(as.character(fin$TL)), as.numeric(as.character(fin$RPS)), as.numeric(as.character(fin$NRPS)))
  ROA <- mapply(roa, as.numeric(as.character(fin$NI)), as.numeric(as.character(fin$TA)))
  CFOA <- mapply(cfoa, as.numeric(as.character(fin$NI)), as.numeric(as.character(fin$DP.DPL)), 
                 as.numeric(as.character(fin$CWC)), as.numeric(as.character(fin$CX)), as.numeric(as.character(fin$TA)))
  GMAR <- mapply(gmar, as.numeric(as.character(fin$GPROF)), as.numeric(as.character(fin$TREV)))
  ACC <- mapply(acc, as.numeric(as.character(fin$DP.DPL)), as.numeric(as.character(fin$CWC)), as.numeric(as.character(fin$TA)))
  
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
  
  profitability <- GPOA + ROE + ROA + CFOA + GMAR + ACC
  profitability <- scale(profitability)
  data.frame(ticker = x$ticker, 
             profitability = profitability, 
             GPOA = GPOA, 
             ROE = ROE,
             ROA = ROA,
             CFOA = CFOA, 
             GMAR = GMAR,
             ACC = ACC)
}