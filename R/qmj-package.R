#' Quality Minus Junk
#'
#' Calculates and manipulates data on the quality of companies.
#' 
#' *This package is currently still in early development.* \cr
#'   RECOMMENDATION FOR EARLY USE: \cr
#'   \itemize{
#'   \item All functions and datasets are documented, and are freely available for use.
#'   \item getcompanies() is an alternative way of accessing the companies.RData data.
#'   \item collectmarketdata() provides an early look into *very rough* initial calculations
#'   for quality in the sample companies.
#'   \item *Warning*: Do not call getcashflows, getincomestatements, or getbalancesheets.
#'   While these functions correctly work, the functions do require several minutes to
#'   fully finish downloading the relevant financial documents from the web, returning
#'   a large matrix containing the desired documents.
#'   }
#' 
#' @docType package
#' @name qmj-package
NULL