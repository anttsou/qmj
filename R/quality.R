#' A dataframe of quality scores for companies listed in the Russell 3000
#' Displays overall quality scores as well as the scores for profitability, growth,
#' safety, and payouts. Companies are sorted in order of quality score, with NAs stored
#' at the end of the data set. 
#' 
#' If partial information exists (i.e., a profitability score
#' was able to be calculated), then those scores are kept for that company, even if
#' insufficient information exists to produce a quality score. More details may be found
#' on the technical vignette.
#' 
#' Last updated: January 2015
#'
#' @format A data frame with 2999 rows and 7 variables
#' \itemize{
#'    \item quality
#'    \item profitability
#'    \item growth
#'    \item safety
#'    \item payouts
#'  }
#'
#' @name quality
#' @docType data
#' @keywords data
NULL