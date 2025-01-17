#' A dataframe of quality scores for companies listed in the Russell 3000
#' 
#' Displays overall quality scores as well as the scores for profitability, 
#' growth, safety, and payouts. Companies are sorted in order of quality 
#' score, with NAs stored at the end of the data set. For a description of 
#' the Russell 3000 index, as well as why it was used for this package, 
#' see \code{\link{companies_r3k16}}. Last updated 2016/01/06.
#' 
#' The quality data set stores quality and component scores for the 
#' various companies list in the \code{\link{companies_r3k16}} data set. For 
#' every ticker in companies, quality attempts to assign a profitability, 
#' growth, safety, and payouts score to each company using data from 
#' \code{\link{financials_r3k16}} and \code{\link{prices_r3k16}}, and then attempts 
#' to provide a quality score. It is possible that one or more companies 
#' may not have sufficient information to provide one or more component 
#' scores, in which case those companies can still be found at the end 
#' of the data set, with NA's making up any data that cannot be found.
#' 
#' If partial information exists (i.e., a profitability score was able to 
#' be calculated), then those scores are kept for that company, even if
#' insufficient information exists to produce a quality score. More 
#' details may be found on the technical vignette.
#'
#' @format A data frame with approximately 3000 rows and 7 variables
#' \itemize{
#'    \item quality = class \code{"numeric"}.
#'    \item profitability = class \code{"numeric"}.
#'    \item growth = class \code{"numeric"}.
#'    \item safety = class \code{"numeric"}.
#'    \item payouts = class \code{"numeric"}.
#'  }
#'
#' @name quality_r3k16
#' @docType data
#' @keywords data
NULL