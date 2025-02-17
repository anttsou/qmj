% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/market_payouts.R
\name{market_payouts}
\alias{market_payouts}
\title{Collects payout z-scores for companies}
\usage{
market_payouts(
  companies = qmj::companies_r3k16,
  financials = qmj::financials_r3k16
)
}
\arguments{
\item{companies}{A data frame of company names and tickers. Requires
a 'ticker' column. Defaults to the provided companies data set.}

\item{financials}{A data frame containing financial statements for every
company. Defaults to the provided financials data set.}
}
\value{
data.frame of market payouts values
}
\description{
Given a data frame of companies (names and tickers) and a data frame of 
financial statements, calculates EISS, DISS, NPOP and determines the 
z-score of overall payout for each company based on the paper Quality 
Minus Junk (Asness et al.) in Appendix page A3-4.
}
\section{Functions}{
\itemize{
\item \code{market_payouts()}: Returns all rows in x that aren't in y,
where x and y are data frames.

}}
\examples{
market_payouts(companies_r3k16[1,], financials_r3k16)
}
\seealso{
\code{\link{market_data}}

\code{\link{market_profitability}}

\code{\link{market_growth}}

\code{\link{market_safety}}
}
