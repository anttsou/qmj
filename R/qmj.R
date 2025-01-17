#' Exploring a quality minus junk approach to evaluating stocks
#'
#' The \strong{qmj} package calculates quality scores for the companies 
#' in the Russell 3000 Index based on the paper 
#' \emph{Quality Minus Junk} by Clifford Asness, Andrea Frazzini, 
#' and Lasse Pedersen.
#' 
#' Quality is a scaled measure of a company's profitability, growth, 
#' safety, and payouts. By using publicly available data for 
#' company balance sheets, income statements, and cash flows, \strong{qmj} 
#' calculates relative quality z-scores for companies. 
#' 
#' All functions and datasets are documented, and are freely 
#' available for use.
#' Index of datasets:
#' \itemize{
#'  \item companies_r3k16 - A data frame of publicly traded companies in the Russell 
#'  3000 Index.
#'  \item financials_r3k16 - Financial statements for companies in the 
#'  companies_r3k16 dataset.
#'  \item prices_r3k16 - Daily prices and price returns for the past two years for
#'  each company.
#'  \item quality_r3k16 - Measured quality z-scores and component scores
#' }
#' 
#' @references
#' Asness, Clifford S., Andrea Frazzini, and Lasse H. Pedersen. 'Quality Minus Junk.' AQR (2013)
#' 
#' @importFrom dplyr %>%
#' @importFrom reticulate use_virtualenv
#' 
#' @name qmj
"_PACKAGE"


.onLoad <- function(...) {
  use_virtualenv("r-qmj", required = FALSE)
}