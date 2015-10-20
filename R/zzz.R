.onLoad <- function(libname, pkgname) {
  if(!("devtools" %in% utils::installed.packages())) install.packages("devtools")
  if(!("qmjdata" %in% utils::installed.packages())) devtools::install_github("anttsou/qmjdata")
  utils::globalVariables(c("desc","year","ticker"))
}