.onLoad <- function(libname, pkgname) {
  if (system.file(package = "devtools") == "") 
    install.packages("devtools")
  if (system.file(package = "qmjdata") == "") 
    devtools::install_github("anttsou/qmjdata")
  utils::globalVariables(c("desc", "year", "ticker"))
} 
