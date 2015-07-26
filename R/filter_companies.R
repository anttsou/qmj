#' Filters quality data to isolate or remove companies which have quality scores that are primarily "driven" by a single component.
#'
#' Taking in the quality data frame and a filter parameter, returns companies that are defined to be driven by that particular
#' factor. For a company to be driven by a particular component, at least 50% of its quality score must be due to that particular
#' component. Also allows for the user to trim "driven" companies, returning companies that have scores that are relatively well-distributed.
#' @param quality The quality data frame produced by market_data.
#' @param filter What driver to search for. Valid inputs are "profitability", "growth", "payouts", "safety", and "all", which filters companies that are driven by any component.
#' @param remove A logical value. If true, removes companies from data frame that are driven by the given filter. Mutually exclusive with the isolate parameter.
#' @param isolate A logical value. If true, returns companies that are driven by the given filter. Mutually exclusive with the remove parameter.
#' @examples
#' filter <- "all"
#' filter_companies(qmjdata::quality,filter)
#' @export

filter_companies <- function(quality = qmjdata::quality, 
                             filter = "all", 
                             remove = TRUE, 
                             isolate = FALSE) {
  if(remove == isolate) {
    stop("Both the remove and isolate parameters are set to either true or false in filter_companies. They must have different logical values.")
  }
  #Helper functions perform a simple arithmetic operation to determine if a given component is >= 50% of a given quality score.
  prof_filter <- function(profitability, data){
    if(is.na(profitability) || is.na(data)){
      FALSE
    }else{
      if(abs(profitability/data) >= .5){
        TRUE
      }else{
        FALSE
      }
    }
  }
  growth_filter <- function(growth, data){
    if(is.na(growth) || is.na(data)){
      FALSE
    } else{
      if(abs(growth/data) >= .5){
        TRUE
      } else{
        FALSE
      }
    }
  }
  safety_filter <- function(safety, data){
    if(is.na(safety) || is.na(data)){
      FALSE
    } else{
      if(abs(safety/data) >= .5){
        TRUE
      } else{
        FALSE
      }
    }
  }
  payouts_filter <- function(payouts, data){
    if(is.na(payouts) || is.na(data)){
      FALSE
    } else{
      if(abs(payouts/data) >= .5){
        TRUE
      } else{
        FALSE
      }
    }
  }
  
  #If statements below handle filter choices, as well as if a user chooses a non-valid filter choice.
  #In each case, drivenindices is a vector of logicals that determine which companies to remove or isolate, based on user choice.
  if(filter == "profitability"){
    drivenindices <- mapply(prof_filter, quality$profitability, quality$quality)
    if(remove){
      return(quality[!drivenindices,])
    } else if (isolate){
      return(quality[drivenindices,])
    }
  } else if(filter == "growth"){
    drivenindices <- mapply(growth_filter, quality$growth, quality$quality)
    if(remove){
      return(quality[!drivenindices,])
    } else if (isolate){
      return(quality[drivenindices,])
    }
    
  } else if(filter == "safety"){
    drivenindices <- mapply(safety_filter, quality$safety, quality$quality)
    if(remove){
      return(quality[!drivenindices,])
    } else if (isolate){
      return(quality[drivenindices,])
    }
    
  } else if(filter == "payouts"){
    drivenindices <- mapply(payouts_filter, quality$payouts, quality$quality)
    if(remove){
      return(quality[!drivenindices,])
    } else if (isolate){
      return(quality[drivenindices,])
    }
  } else if(filter == "all"){
    drivenindices1 <- mapply(prof_filter, quality$profitability, quality$quality)
    drivenindices2 <- mapply(growth_filter, quality$growth, quality$quality)
    drivenindices3 <- mapply(safety_filter, quality$safety, quality$quality)
    drivenindices4 <- mapply(payouts_filter, quality$payouts, quality$quality)
    
    drivenindices <- as.logical(drivenindices1 + drivenindices2 + drivenindices3 + drivenindices4)
    
    if(remove){
      return(quality[!drivenindices,])
    } else if (isolate){
      return(quality[drivenindices,])
    }
  } else{
    stop("Error with the filter parameter. Please make sure spelling is correct, and that the filter is lowercase.")
  }
}