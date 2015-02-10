#' Filters quality data to isolate companies which have quality scores that are primarily "driven" by a single component.
#'
#' Taking in the quality data frame and a filter parameter, returns companies that are defined to be driven by that particular
#' factor. For a company to be driven by a particular component, at least 50% of its quality score must be due to that particular
#' component. Also allows for the user to trim "driven" companies, returning companies that have scores that are relatively well-distributed.
#' @param data The qualiy data frame produced by market_data.
#' @param filter What driver to search for. Valid inputs are "profitability", "growth", "payouts", "safety", and "all", which filters companies that are driven by any component.
#' @param remove A logical value. If true, removes companies from data frame that are driven by the given filter. Mutually exclusive with the isolate parameter.
#' @param isolate A logical value. If true, returns companies that are driven by the given filter. Mutually exclusive with the remove parameter.
#' @export

filter_companies <- function(data, filter, remove=TRUE, isolate=FALSE){
  prof_filter <- function(profitability, quality){
    if(is.na(profitability) || is.na(quality)){
      FALSE
    }else{
      if(abs(profitability/quality) >= .5){
        TRUE
      }else{
        FALSE
      }
    }
  }
  growth_filter <- function(growth, quality){
    if(is.na(growth) || is.na(quality)){
      FALSE
    } else{
      if(abs(growth/quality) >= .5){
        TRUE
      } else{
        FALSE
      }
    }
  }
  safety_filter <- function(safety, quality){
    if(is.na(safety) || is.na(quality)){
      FALSE
    } else{
      if(abs(safety/quality) >= .5){
        TRUE
      } else{
        FALSE
      }
    }
  }
  payouts_filter <- function(payouts, quality){
    if(is.na(payouts) || is.na(quality)){
      FALSE
    } else{
      if(abs(payouts/quality) >= .5){
        TRUE
      } else{
        FALSE
      }
    }
  }
  
  if(filter == "profitability"){
    drivenindices <- mapply(prof_filter, data$profitability, data$quality)
    if(remove){
      return(data[!drivenindices,])
    } else if (isolate){
      return(data[drivenindices,])
    }
  } else if(filter == "growth"){
    drivenindices <- mapply(growth_filter, data$growth, data$quality)
    if(remove){
      return(data[!drivenindices,])
    } else if (isolate){
      return(data[drivenindices,])
    }
    
  } else if(filter == "safety"){
    drivenindices <- mapply(safety_filter, data$safety, data$quality)
    if(remove){
      return(data[!drivenindices,])
    } else if (isolate){
      return(data[drivenindices,])
    }
    
  } else if(filter == "payouts"){
    drivenindices <- mapply(payouts_filter, data$payouts, data$quality)
    if(remove){
      return(data[!drivenindices,])
    } else if (isolate){
      return(data[drivenindices,])
    }
  } else if(filter == "all"){
    drivenindices1 <- mapply(prof_filter, data$profitability, data$quality)
    drivenindices2 <- mapply(growth_filter, data$growth, data$quality)
    drivenindices3 <- mapply(safety_filter, data$safety, data$quality)
    drivenindices4 <- mapply(payouts_filter, data$payouts, data$quality)
    
    drivenindices <- as.logical(drivenindices1 + drivenindices2 + drivenindices3 + drivenindices4)
    
    if(remove){
      return(data[!drivenindices,])
    } else if (isolate){
      return(data[drivenindices,])
    }
  } else{
    stop("Error with the filter parameter. Please make sure spelling is correct, and that the filter is lowercase.")
  }
}