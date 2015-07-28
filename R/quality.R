#' A dataframe of quality scores for companies listed in the Russell 3000
#' Displays overall quality scores as well as the scores for profitability, 
#' growth, safety, and payouts. Companies are sorted in order of quality 
#' score, with NAs stored at the end of the data set.
#' 
#' If partial information exists (i.e., a profitability score was able to 
#' be calculated), then those scores are kept for that company, even if
#' insufficient information exists to produce a quality score. More 
#' details may be found on the technical vignette.
#' 
#' The quality data set stores quality and component scores for the 
#' various companies list in the \code{\link{companies}} data set. For 
#' every ticker in companies, quality attempts to assign a profitability, 
#' growth, safety, and payouts score to each company using data from 
#' \code{\link{financials}} and \code{\link{prices}}, and then attempts 
#' to provide a quality score. It is possible that one or more companies 
#' may not have sufficient information to provide one or more component 
#' scores, in which case those companies can still be found at the end 
#' of the data set, with NA's making up any data that cannot be found.
#' 
#' The Russell 3000 Index is an equity index that tracks the performance 
#' of the "3000" (this number may actually vary from year to year, but 
#' is always in the neighborhood of 3000) largest US companies as measured 
#' by market cap. The component companies that make up this index are
#' reconstituted once a year, usually between May and June. At this 
#' reconstitution, all companies are reranked based on their market caps 
#' for the year, and any companies which become "ineligible" by, for 
#' example, going bankrupt, becoming acquired, or becoming private, 
#' are replaced at this time.
#' 
#' This Index was chosen because the majority of the information 
#' used in this package relies on data sources that are US-centric, 
#' in addition to giving reasonable output by using companies which
#' are at least of sufficient size to produce less erroneous items
#' (such as a tiny company doubling in profitability, though the 
#' actual change is very small in magnitude) as well as producing 
#' items which are more likely to interest the user.
#' 
#' Last updated: January 2015
#'
#' @format A data frame with 2999 rows and 7 variables
#' \itemize{
#'    \item quality = class \code{"numeric"}.
#'    \item profitability = class \code{"numeric"}.
#'    \item growth = class \code{"numeric"}.
#'    \item safety = class \code{"numeric"}.
#'    \item payouts = class \code{"numeric"}.
#'  }
#'
#' @name quality
#' @seealso \code{\link{filter_companies}}
#' @examples
#' 
#' head(qmjdata::quality,10)
#' 
#' temp <- qmjdata::quality
#' 
#' # remove NAs
#' temp <- temp[!is.na(temp$quality),]
#' tail(temp,10)
#' @docType data
#' @keywords data
NULL