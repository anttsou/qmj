#' Filters quality data to isolate companies which have quality scores that are primarily "driven" by a single component.
#'
#' Taking in the quality data frame and a filter parameter, returns companies that are defined to be driven by that particular
#' factor. For a company to be driven by a particular component, at least 50% of its quality score must be due to that particular
#' component. Also allows for the user to trim "driven" companies, returning companies that have scores that are relatively well-distributed.
#' @param data The qualiy data frame produced by market_data.
#' @param filter What driver to search for. Valid inputs are "profitability", "growth", "payouts" and "safety".
#' @param remove A logical value. If true, removes companies from data frame that are driven by the given filter. Mutually exclusive with the isolate parameter.
#' @param isolate A logical value. If true, returns companies that are driven by the given filter. Mutually exclusive with the remove parameter.
#' @export

filter_companies <- function(data, filter, remove, isolate){
  if(filter == "profitability"){
    if(remove){
      
    } else if (isolate){
      
    }
  } else if(filter == "growth"){
    if(remove){
      
    } else if (isolate){
      
    }
    
  } else if(filter == "safety"){
    if(remove){
      
    } else if (isolate){
      
    }
    
  } else if(filter == "payouts"){
    if(remove){
      
    } else if (isolate){
      
    }
    
  }
}