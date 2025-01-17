#' Install yfinance and dependencies
#'
#' `install_yfinance()` installs just the yfinance python package and it's
#' direct dependencies. Users may be asked to install miniconda for the python installations. 
#' Even if you first decline it, you can later install miniconda by running
#' [`reticulate::install_miniconda()`]
#' 
#' @inheritParams reticulate::py_install
#' 
#' @param ... other arguments passed to [`reticulate::py_install()`]
#' @param new_env Delete `envname` if it already exists
#'   
#' @importFrom reticulate py_install virtualenv_exists virtualenv_remove
#' 
#' @return No return value, called for installing Python yfinance module
#' 
#' @export
#' 

install_yfinance <- function(..., envname = "r-qmj", new_env = identical(envname, "r-qmj")) {
  if(new_env && virtualenv_exists(envname))
    virtualenv_remove(envname)
    
  py_install(packages = "yfinance", envname = envname, ...)
}
