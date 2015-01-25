#' collectmarketgrowth
#'
#' Given a list of companies (names and tickers), a balance sheet, a cash flow statement,
#' and an income statement, calculates GPOA, ROE, ROA, CFOA, GMAR, ACC
#' and determines the z-score of overall growth based on the paper
#' Quality Minus Junk (Asness et al.) in Appendix page A2.
#' @param x A dataframe of company names and tickers.
#' @param BS A dataframe containing balance sheet information for every company.
#' @param CF A dataframe containing cash flow information for every company.
#' @param IS A dataframe containing income statement information for every company.
#' @export

#use sapply to make columns numeric
collectmarketgrowth <- function(x, BS, CF, IS){
  numCompanies <- length(x$tickers)
#   growth <- rep(0, numCompanies)
#   GPOA <- rep(0, numCompanies)
#   ROE <- rep(0, numCompanies)
#   ROA <- rep(0, numCompanies)
#   CFOA <- rep(0, numCompanies)
#   GMAR <- rep(0, numCompanies)
#   ACC <- rep(0, numCompanies)
  BS[is.na(BS)] <- 0
  IS[is.na(IS)] <- 0
  CF[is.na(CF)] <- 0
  
  fin <- merge(BS, merge(CF, IS, by=c("ticker", "year")), by=c("ticker", "year"))
  fin <- fin[order(fin$year, decreasing=TRUE),]
  fin <- data.table(fin, key="ticker")
  fstyear <- fin[J(unique(ticker)), mult="first"]
  fin <- fin[order(fin$year, decreasing=FALSE),]
  setkey(fin, "ticker")
  lstyear <- fin[J(unique(ticker)), mult="first"]
  
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
  
#   for(i in 1:numCompanies){
#     cBS <- subset(BS,ticker == as.character(x$tickers[i]))
#     cIS <- subset(IS,ticker == as.character(x$tickers[i]))
#     cCF <- subset(CF,ticker == as.character(x$tickers[i]))
#     
#     if(nrow(cBS) > 3 && nrow(cIS) > 3 && nrow(cCF) > 3) {
#       ###GROWTH
#       #GPOA
#       #(5 year change in gross profits)/Total assets
#       #GP - IS 6
#       #Total assets - BS 18
#       GPOA[i] <- (as.numeric(cIS$GPROF[1]) - as.numeric(cIS$GPROF[length(cIS$GPROF)]))/
#                  as.numeric(cBS$TA[length(cBS$TA)])
#     
#       #(5 year change in Net income)/book equity
#       #Net income - CF 2
#       # Book equity 
#       ROE[i] <- (as.numeric(cCF$NI[1]) - as.numeric(cCF$NI[length(cCF$NI)]))/
#                 (as.numeric(cBS$TLSE[length(cBS$TLSE)]) - as.numeric(cBS$TL[length(cBS$TL)]) - 
#                  as.numeric(cBS$RPS[length(cBS$RPS)]) + as.numeric(cBS$NRPS[length(cBS$NRPS)]))
#       
#       #(5 year change in net income)/total assets
#       # Net income - CF 2
#       # Total assets - BS 18
#       ROA[i] <- (as.numeric(cCF$NI[1]) - as.numeric(cCF$NI[length(cCF$NI)]))/
#                  as.numeric(cBS$TA[length(cBS$TA)])
#       #(5 year change in cash flow over assets)
#       #Change in cash flow = net income + depreciation - change in working capital - capital expenditure
#       # IB (Net income) - CF 2
#       # Depreciation - CF 3
#       # Change in working capital - CF 7
#       # Capital expenditure - CF 9
#       #Total assets - BS 18
#       changeCF1 <- as.numeric(cCF$NI[1]) + as.numeric(cCF$DP[1]) - as.numeric(cCF$CWC[1]) - 
#                    as.numeric(cCF$CX[1])
#       changeCF2 <- as.numeric(cCF$NI[length(cCF$NI)]) + as.numeric(cCF$DP[length(cCF$DP)]) - 
#                    as.numeric(cCF$CWC[length(cCF$CWC)]) - as.numeric(cCF$CX[length(cCF$CX)])
#       CFOA[i] <- (changeCF1 - changeCF2)/as.numeric(cBS$TA[length(cBS$TA)])
#       #(5 year change in gross profit)/(total sales)
#       # GP - IS 6
#       # Total sales (total revenues) - IS 4
#       GMAR[i] <- as.numeric(cIS$GPROF[1]) - as.numeric(cIS$GPROF[length(cIS$GPROF)])/
#                  as.numeric(cIS$TREV[length(cIS$TREV)])
#       
#       #(5 year change in (low) accruals)/total assets
#       # Low accruals = DP - (change in WC)
#       # DP - CF 3
#       #Change in working capital - CF 7
#       #Total assets - BS 18
#       accrual1 <- as.numeric(cCF$DP[1]) - as.numeric(cCF$CWC[1])
#       accrual2 <- as.numeric(cCF$DP[length(cCF$DP)]) - as.numeric(cCF$CWC[length(cCF$CWC)])
#       ACC[i] <- (accrual1 - accrual2)/as.numeric(cBS$TA[length(cBS$TA)])
#     }
#   }
  
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

#   for(i in 1:numCompanies){
#     growth[i] <- GPOA[i] + ROE[i] + ROA[i] + CFOA[i] + GMAR[i] + ACC[i]
#   }
  scale(growth)
  res <- data.frame(fstyear$ticker, growth)
  colnames(res) <- c("tickers", "growth")
  originalorder <- data.frame(x$tickers)
  colnames(originalorder) <- "tickers"
  res <- merge(originalorder, res, by="tickers")
  res$growth
}