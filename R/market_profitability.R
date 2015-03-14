#' Collects profitability z-scores for companies
#'
#' Given a data frame of companies (names and tickers) and a data frame of financial statements, 
#' calculates GPOA, ROE, ROA, CFOA, GMAR, ACC and determines the z-score of overall profitability 
#' for each company based on the paper Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param companies A data frame of company names and tickers.
#' @param financials A data frame containing financial statements for every company.
#' @seealso \code{\link{market_data}}
#' @seealso \code{\link{market_growth}}
#' @seealso \code{\link{market_safety}}
#' @seealso \code{\link{market_payouts}}
#' @examples
#' data(companies)
#' data(financials)
#' market_profitability(companies, financials)
#' @export

market_profitability <- function(x, financials){ 
  if(length(x$ticker) == 0) {
    stop("first parameter requires a ticker column.")
  }
  if(length(which(financials$TCSO < 0))) {
    stop("Negative TCSO exists.")
  }
  numCompanies <- length(x$ticker)
  
  #set unavailable financial info to 0
  financials[is.na(financials)] <- 0
  
  allcompanies <- data.frame(x$ticker)
  colnames(allcompanies) <- "ticker"
  fin <- financials
  fin <- dplyr::arrange(fin, desc(year))
  fin <- dplyr::distinct_(fin, "ticker")
  fin <- merge(allcompanies, fin, by="ticker", all.x = TRUE)
  
  #functions calculate individual components of profitability
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
  
  GPOA <- mapply(gpoa, fin$GPROF, fin$TA)
  ROE <- mapply(roe, fin$NI, fin$TLSE, 
                fin$TL, fin$RPS, fin$NRPS)
  ROA <- mapply(roa, fin$NI, fin$TA)
  CFOA <- mapply(cfoa, fin$NI, fin$DP.DPL, 
                 fin$CWC, fin$CX, fin$TA)
  GMAR <- mapply(gmar, fin$GPROF, fin$TREV)
  ACC <- mapply(acc, fin$DP.DPL, fin$CWC, fin$TA)
  
  #removes potential errors from Inf values
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

  #removes potential errors in nan values
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