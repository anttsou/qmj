.onLoad <- function(libname, pkgname) {
  if(!require(devtools)) install.packages("devtools")
  if(!require(qmjdata)) devtools::install_github("anttsou/qmjdata")
  utils::globalVariables(c("desc","year","ticker"))
}