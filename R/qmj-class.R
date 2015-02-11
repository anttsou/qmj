#' The qmj object allows for more in-depth analysis of a single company.
#'
#' The qmj object contains all details related to a company's quality score,
#' and also provides some tools for analyzing a specific company in more detail.
#'
#' @slot ticker Object of class \code{"character"}
#' @slot profitability Object of class \code{"numeric"}. The profitability score.
#' @slot pGPOA Object of class \code{"numeric"}. Gross profits over assets for profitability calculation.
#' @slot pROE Object of class \code{"numeric"}. Return on equity for profitability calculation.
#' @slot pROA Object of class \code{"numeric"}. Return on assets for profitability calculation.
#' @slot pCFOA Object of class \code{"numeric"}. Cash flow over assets for profitability calculation.
#' @slot pGMAR Object of class \code{"numeric"}. Gross margin for profitability calcuation.
#' @slot pACC Object of class \code{"numeric"}. Low accruals for profitability calculation.
#' @slot growth Object of class \code{"numeric"}. The growth score.
#' @slot gGPOA Object of class \code{"numeric"}. Change in gross profits for growth calculation.
#' @slot gROE Object of class \code{"numeric"}. Change in return on equity for growth calculation.
#' @slot gROA Object of class \code{"numeric"}. Change in return on assets for growth calculation.
#' @slot gCFOA Object of class \code{"numeric"}. Change in cash flow over assets for growth calculation.
#' @slot gGMAR Object of class \code{"numeric"}. Change in gross margin for growth calculation.
#' @slot gACC Object of class \code{"numeric"}. Change in low accruals for growth calculation.
#' @slot safety Object of class \code{"numeric"}. THe safety score.
#' @slot sBAB Object of class \code{"numeric"}. The calculated beta relative to the S&P 500.
#' @slot sIVOL Object of class \code{"numeric"}. The calculated IVOL relative to the S&P 500.
#' @slot sLEV Object of class \code{"numeric"}. The low leverage for safety calculation.
#' @slot sO Object of class \code{"numeric"}. The Ohlson's O score for safety calculation.
#' @slot sZ Object of class \code{"numeric"}. The Altman's Z score for safety calculation.
#' @slot sEVOL Object of class \code{"numeric"}. The low earnings volatility for safety calculation.
#' @slot payouts Object of class \code{"numeric"}. The payouts score.
#' @slot pEISS Object of class \code{"numeric"}. Net equity issuance for payouts calculation.
#' @slot pDISS Object of class \code{"numeric"}. Net debt issuance for payouts calculation.
#' @slot pNPOP Object of class \code{"numeric"}. Total net payouts over profits for payouts calculation.
#' @slot quality Object of class \code{"numeric"}. The quality score.
#' @name qmj-class
#' @rdname qmj-class
#' @exportClass qmj

qmj <- setClass(
  "qmj",
  slots = c(
    ticker = "character",
    profitability = "numeric",
    pGPOA = "numeric",
    pROE = "numeric",
    pROA = "numeric",
    pCFOA = "numeric",
    pGMAR = "numeric",
    pACC = "numeric",
    growth = "numeric",
    gGPOA = "numeric",
    gROE = "numeric",
    gROA = "numeric",
    gCFOA = "numeric",
    gGMAR = "numeric",
    gACC = "numeric",
    safety = "numeric",
    sBAB = "numeric",
    sIVOL = "numeric",
    sLEV = "numeric",
    sO = "numeric",
    sZ = "numeric",
    sEVOL = "numeric",
    payouts = "numeric",
    pEISS = "numeric",
    pDISS = "numeric",
    pNPOP = "numeric",
    quality = "numeric"    
  ),
  prototype = list(
    ticker = "NA",
    profitability = 0,
    pGPOA = 0,
    pROE = 0,
    pROA = 0,
    pCFOA = 0,
    pGMAR = 0,
    pACC = 0,
    growth = 0,
    gGPOA = 0,
    gROE = 0,
    gROA = 0,
    gCFOA = 0,
    gGMAR = 0,
    gACC = 0,
    safety = 0,
    sBAB = 0,
    sIVOL = 0,
    sLEV = 0,
    sO = 0,
    sZ = 0,
    sEVOL = 0,
    payouts = 0,
    pEISS = 0,
    pDISS = 0,
    pNPOP = 0,
    quality = 0
    ),
  validity = function(object)
  {
    if(object@ticker != toupper(object@ticker)) {
      return("An invalid ticker was given.")
    }
    return(TRUE)
  })

#' @title Return the ticker of the qmj object.
#' @return The ticker of the qmj object as a character.
#' 
#' @examples
#' \dontrun{
#' qmjs <- data(qmjs)
#' view_ticker(qmjs[[1]])
#' }
#' @rdname view_ticker
#' @exportMethod view_ticker
setGeneric(name="view_ticker",
           def=function(theObject)
           {
             standardGeneric("view_ticker")
           }
)

#' @rdname view_ticker
setMethod(f="view_ticker",
          signature="qmj",
          definition=function(theObject)
          {
            return(theObject@ticker)
          }
)

#' @title Returns the profitability of the qmj object.
#' @return A data frame with 1 row and 8 variables that describes profitability of the qmj object.
#' 
#' @examples
#' \dontrun{
#' qmjs <- data(qmjs)
#' view_profitability(qmjs[[1]])
#' }

#' @rdname view_profitability-methods
#' @exportMethod view_profitability
setGeneric(name="view_profitability",
                      def=function(theObject)
                      {
                        standardGeneric("view_profitability")
                      }
                      )

#' @rdname view_profitability-methods
#' @aliases view_profitability,qmj-method
setMethod(f="view_profitability",
                      signature="qmj",
                      definition=function(theObject)
                      {
                        tick <- theObject@ticker
                        prof <- theObject@profitability
                        gpoa <- theObject@pGPOA
                        roe <- theObject@pROE
                        roa <- theObject@pROA
                        cfoa <- theObject@pCFOA
                        gmar <- theObject@pGMAR
                        acc <- theObject@pACC
                        
                        return(data.frame(ticker = tick, 
                                   profitability = prof, 
                                   GPOA = gpoa, 
                                   ROE = roe,
                                   ROA = roa,
                                   CFOA = cfoa, 
                                   GMAR = gmar,
                                   ACC = acc))
                      }
                      )

#' @title Returns the growth of the qmj object.
#' @aliases view_growth,qmj-method
#' @return A data frame with 1 row and 8 variables that describes growth of the qmj object.
#' @export
#' @docType methods
#' @rdname view_growth-methods
#' 
#' @examples
#' \dontrun{
#' qmjs <- data(qmjs)
#' view_growth(qmjs[[1]])
#' }
setGeneric(name="view_growth",
                      def=function(theObject)
                      {
                        standardGeneric("view_growth")
                      }
                      )

setMethod(f="view_growth",
                      signature="qmj",
                      definition=function(theObject)
                      {
                        tick <- theObject@ticker
                        prof <- theObject@growth
                        gpoa <- theObject@gGPOA
                        roe <- theObject@gROE
                        roa <- theObject@gROA
                        cfoa <- theObject@gCFOA
                        gmar <- theObject@gGMAR
                        acc <- theObject@gACC
                        
                        return(data.frame(ticker = tick, 
                                          growth = prof, 
                                          GPOA = gpoa, 
                                          ROE = roe,
                                          ROA = roa,
                                          CFOA = cfoa, 
                                          GMAR = gmar,
                                          ACC = acc))
                      }
                      )

#' @title Returns the safety of the qmj object.
#' @aliases view_safety,qmj-method
#' @return A data frame with 1 row and 8 variables that describes the safety of the qmj object.
#' @export
#' @docType methods
#' @rdname view_safety-methods
#' 
#' @examples
#' \dontrun{
#' qmjs <- data(qmjs)
#' view_safety(qmjs[[1]])
#' }
setGeneric(name="view_safety",
                      def=function(theObject)
                      {
                        standardGeneric("view_safety")
                      }
                      )

setMethod(f="view_safety",
                      signature="qmj",
                      definition=function(theObject)
                      {
                        tick <- theObject@ticker
                        safety <- theObject@safety
                        bab <- theObject@sBAB
                        ivol <- theObject@sIVOL
                        lev <- theObject@sLEV
                        o <- theObject@sO
                        z <- theObject@sZ
                        evol <- theObject@sEVOL
                        return(data.frame(ticker = tick, 
                                   safety = safety, 
                                   BAB = bab, 
                                   IVOL = ivol,
                                   LEV = lev, 
                                   O = o, 
                                   Z = z, 
                                   EVOL = evol))
                      }
                      )

#' @title Returns the payouts of the qmj object.
#' @aliases view_payouts,qmj-method
#' @return A data frame with 1 row and 5 variables that describes the payouts of the qmj object.
#' @export
#' @docType methods
#' @rdname view_payouts-methods
#' 
#' @examples
#' \dontrun{
#' qmjs <- data(qmjs)
#' view_payouts(qmjs[[1]])
#' }
setGeneric(name="view_payouts",
                      def=function(theObject)
                      {
                        standardGeneric("view_payouts")
                      }
                      )

setMethod(f="view_payouts",
                      signature="qmj",
                      definition=function(theObject)
                      {
                        tick <- theObject@ticker
                        payouts <- theObject@payouts
                        eiss <- theObject@pEISS
                        diss <- theObject@pDISS
                        npop <- theObject@pNPOP
                        return(data.frame(ticker = tick, 
                                   payouts = payouts, 
                                   EISS = eiss, 
                                   DISS = diss,
                                   NPOP = npop))
                      }
                      )

#' @title Returns the quality of the qmj object.
#' @aliases view_quality,qmj-method
#' @return A data frame with 1 row and 6 variables that describes the quality score of the qmj object.
#' @export
#' @docType methods
#' @rdname view_quality-methods
#' 
#' @examples
#' \dontrun{
#' qmjs <- data(qmjs)
#' view_quality(qmjs[[1]])
#' }
setGeneric(name="view_quality",
                      def=function(theObject)
                      {
                        standardGeneric("view_quality")
                      }
                      ) 

setMethod(f="view_quality",
                      signature="qmj",
                      definition=function(theObject)
                      {
                        tick <- theObject@ticker
                        quality <- theObject@quality
                        profitability <- theObject@profitability
                        growth <- theObject@growth
                        safety <- theObject@safety
                        payouts <- theObject@payouts
                        
                        return(data.frame(ticker = tick,
                                          quality = quality,
                                          profitability = profitability,
                                          growth = growth,
                                          safety = safety,
                                          payouts = payouts))
                      }
                      )

#' @title Plots the relative quality of the qmj object.
#' @aliases plot_quality,qmj-method
#' @return Displays a quality histogram, highlighting the bin containing the chosen qmj object/company.
#' @export
#' @docType methods
#' @rdname plot_quality-methods
#' 
#' @examples
#' \dontrun{
#' qmjs <- data(qmjs)
#' quality <- data(quality)
#' plot_quality(qmjs[[2]], quality)
#' }
setGeneric(name="plot_quality",
                      def=function(theObject, quality_data_frame)
                      {
                        standardGeneric("plot_quality")
                      }
                      ) 

setMethod(f="plot_quality",
                      signature=c("qmj", "data.frame"),
                      definition=function(theObject, quality_data_frame)
                      {
                        quality <- theObject@quality
                        if(is.na(quality)){
                          print(paste('Error!', theObject@ticker, "has quality NA.", sep=' '))
                        } else{
                          market_quality <- quality_data_frame$quality
                          market_quality <- c(market_quality, quality)
                          market_quality <- market_quality[order(market_quality, na.last=NA, decreasing=TRUE)]
                          
                          market_range <- range(market_quality)
                          quality_range <- abs(market_range[2] - market_range[1])
                          pbinwidth <- (quality_range)/(length(market_quality) / 10)
                          
                          #Find bin whose max value is just greater than my quality. This should be the right bin.
                          min <- market_range[1]
                          numBins <- ceiling((quality - min)/pbinwidth)
                          maxVal <- (numBins * pbinwidth) + min
                          minVal <- ((numBins - 1) * pbinwidth) + min
                          df <- data.frame(quality = market_quality)
                          cond <- df$quality < maxVal & df$quality > minVal
                          
                          print("Selected object is in the yellow bin.")
                          ggplot2::ggplot(df, aes(x=quality)) +
                            ggtitle("Quality Histogram") + 
                            xlab("Quality Scores") +
                            ylab("Frequency") +
                            geom_histogram(data=subset(df, cond==FALSE), binwidth=pbinwidth, origin=min, fill="black") +
                            geom_histogram(data=subset(df, cond==TRUE), binwidth=pbinwidth, origin=min, fill="gold")
                        }
                      }
                      )

#' @title Plots the relative profitability of the qmj object.
#' @aliases plot_profitability,qmj-method
#' @return Displays a profitability histogram, highlighting the bin containing the chosen qmj object/company.
#' @export
#' @docType methods
#' @rdname plot_profitability-methods
#' 
#' @examples
#' \dontrun{
#' data(qmjs)
#' data(profitability)
#' plot_profitability(qmjs[[1]], profitability)
#' }
setGeneric(name="plot_profitability",
           def=function(theObject, profitability_data_frame)
           {
             standardGeneric("plot_profitability")
           }
) 

setMethod(f="plot_profitability",
          signature=c("qmj", "data.frame"),
          definition=function(theObject, profitability_data_frame)
          {
            profitability <- theObject@profitability
            if(is.na(profitability)){
              print(paste('Error!', theObject@ticker, "has profitability NA.", sep=' '))
            } else {
              market_profitability <- profitability_data_frame$profitability
              market_profitability <- c(market_profitability, profitability)
              market_profitability <- market_profitability[order(market_profitability, na.last=NA, decreasing=TRUE)]
              
              market_range <- range(market_profitability)
              profitability_range <- abs(market_range[2] - market_range[1])
              pbinwidth <- (profitability_range)/(length(market_profitability) / 10)
              
              #Find bin whose max value is just greater than my profitability. This should be the right bin.
              min <- market_range[1]
              numBins <- ceiling((profitability - min)/pbinwidth)
              maxVal <- (numBins * pbinwidth) + min
              minVal <- ((numBins - 1) * pbinwidth) + min
              df <- data.frame(profitability = market_profitability)
              cond <- df$profitability < maxVal & df$profitability > minVal
              
              print("Selected object is in the yellow bin.")
              ggplot2::ggplot(df, aes(x=profitability)) +
                ggtitle("Profitability Histogram") + 
                xlab("Profitability Scores") +
                ylab("Frequency") +
                geom_histogram(data=subset(df, cond==FALSE), binwidth=pbinwidth, origin=min, fill="black") +
                geom_histogram(data=subset(df, cond==TRUE), binwidth=pbinwidth, origin=min, fill="gold")
            }
          }
)

#' @title Plots the relative safety of the qmj object.
#' @aliases plot_safety,qmj-method
#' @return Displays a safety histogram, highlighting the bin containing the chosen qmj object/company.
#' @export
#' @docType methods
#' @rdname plot_safety-methods
#' 
#' @examples
#' \dontrun{
#' data(qmjs)
#' data(safety)
#' plot_safety(qmjs[[2]], safety)
#' }
setGeneric(name="plot_safety",
           def=function(theObject, safety_data_frame)
           {
             standardGeneric("plot_safety")
           }
) 

setMethod(f="plot_safety",
          signature=c("qmj", "data.frame"),
          definition=function(theObject, safety_data_frame)
          {
            safety <- theObject@safety
            if(is.na(safety)){
              print(paste('Error!', theObject@ticker, "has safety NA.", sep=' '))
            } else {
              market_safety <- safety_data_frame$safety
              market_safety <- c(market_safety, safety)
              market_safety <- market_safety[order(market_safety, na.last=NA, decreasing=TRUE)]
              
              market_range <- range(market_safety)
              safety_range <- abs(market_range[2] - market_range[1])
              pbinwidth <- (safety_range)/(length(market_safety) / 10)
              
              #Find bin whose max value is just greater than my safety. This should be the right bin.
              min <- market_range[1]
              numBins <- ceiling((safety - min)/pbinwidth)
              maxVal <- (numBins * pbinwidth) + min
              minVal <- ((numBins - 1) * pbinwidth) + min
              df <- data.frame(safety = market_safety)
              cond <- df$safety < maxVal & df$safety > minVal
              
              print("Selected object is in the yellow bin.")
              ggplot2::ggplot(df, aes(x=safety)) +
                ggtitle("Safety Histogram") + 
                xlab("Safety Scores") +
                ylab("Frequency") +
                geom_histogram(data=subset(df, cond==FALSE), binwidth=pbinwidth, origin=min, fill="black") +
                geom_histogram(data=subset(df, cond==TRUE), binwidth=pbinwidth, origin=min, fill="gold")
            }
          }
)

#' @title Plots the relative growth of the qmj object.
#' @aliases plot_growth,qmj-method
#' @return Displays a growth histogram, highlighting the bin containing the chosen qmj object/company.
#' @export
#' @docType methods
#' @rdname plot_growth-methods
#' 
#' @examples
#' \dontrun{
#' data(qmjs)
#' data(growth)
#' plot_growth(qmjs[[1]], growth)
#' }
setGeneric(name="plot_growth",
           def=function(theObject, growth_data_frame)
           {
             standardGeneric("plot_growth")
           }
) 

setMethod(f="plot_growth",
          signature=c("qmj", "data.frame"),
          definition=function(theObject, growth_data_frame)
          {
            growth <- theObject@growth
            if(is.na(growth)){
              print(paste('Error!', theObject@ticker, "has growth NA.", sep=' '))
            } else {
              market_growth <- growth_data_frame$growth
              market_growth <- c(market_growth, growth)
              market_growth <- market_growth[order(market_growth, na.last=NA, decreasing=TRUE)]
              
              market_range <- range(market_growth)
              growth_range <- abs(market_range[2] - market_range[1])
              pbinwidth <- (growth_range)/(length(market_growth) / 10)
              
              #Find bin whose max value is just greater than my growth. This should be the right bin.
              min <- market_range[1]
              numBins <- ceiling((growth - min)/pbinwidth)
              maxVal <- (numBins * pbinwidth) + min
              minVal <- ((numBins - 1) * pbinwidth) + min
              df <- data.frame(growth = market_growth)
              cond <- df$growth < maxVal & df$growth > minVal
              
              print("Selected object is in the yellow bin.")
              ggplot2::ggplot(df, aes(x=growth)) +
                ggtitle("Growth Histogram") + 
                xlab("Growth Scores") +
                ylab("Frequency") +
                geom_histogram(data=subset(df, cond==FALSE), binwidth=pbinwidth, origin=min, fill="black") +
                geom_histogram(data=subset(df, cond==TRUE), binwidth=pbinwidth, origin=min, fill="gold")
            }
          }
)

#' @title Plots the relative payouts of the qmj object.
#' @aliases plot_payouts,qmj-method
#' @return Displays a payouts histogram, highlighting the bin containing the chosen qmj object/company.
#' @export
#' @docType methods
#' @rdname plot_payouts-methods
#' 
#' @examples
#' \dontrun{
#' data(qmjs)
#' data(payouts)
#' plot_payouts(qmjs[[1]], payouts)
#' }
setGeneric(name="plot_payouts",
           def=function(theObject, payouts_data_frame)
           {
             standardGeneric("plot_payouts")
           }
) 

setMethod(f="plot_payouts",
          signature=c("qmj", "data.frame"),
          definition=function(theObject, payouts_data_frame)
          {
            payouts <- theObject@payouts
            if(is.na(payouts)){
              print(paste('Error!', theObject@ticker, "has payouts NA.", sep=' '))
            } else {
              market_payouts <- payouts_data_frame$payouts
              market_payouts <- c(market_payouts, payouts)
              market_payouts <- market_payouts[order(market_payouts, na.last=NA, decreasing=TRUE)]
              
              market_range <- range(market_payouts)
              payouts_range <- abs(market_range[2] - market_range[1])
              pbinwidth <- (payouts_range)/(length(market_payouts) / 10)
              
              #Find bin whose max value is just greater than my payouts. This should be the right bin.
              min <- market_range[1]
              numBins <- ceiling((payouts - min)/pbinwidth)
              maxVal <- (numBins * pbinwidth) + min
              minVal <- ((numBins - 1) * pbinwidth) + min
              df <- data.frame(payouts = market_payouts)
              cond <- df$payouts < maxVal & df$payouts > minVal
              
              print("Selected object is in the yellow bin.")
              ggplot2::ggplot(df, aes(x=payouts)) +
                ggtitle("Payouts Histogram") + 
                xlab("Payout Scores") +
                ylab("Frequency") +
                geom_histogram(data=subset(df, cond==FALSE), binwidth=pbinwidth, origin=min, fill="black") +
                geom_histogram(data=subset(df, cond==TRUE), binwidth=pbinwidth, origin=min, fill="gold")
            }
          }
)

#' @title Summarizes all information about the qmj object.
#' @aliases summarize,qmj-method
#' @return Prints all components and scores of the qmj object.
#' @export
#' @docType methods
#' @rdname summarize-methods
#' 
#' @examples
#' \dontrun{
#' qmjs <- data(qmjs)
#' summarize(qmjs[[1]])
#' }
setGeneric(name="summarize",
                      def=function(theObject)
                      {
                        standardGeneric("summarize")
                      }
                      ) 

setMethod(f="summarize",
                      signature=c("qmj"),
                      definition=function(theObject)
                      {
                        cat("Information for: ", theObject@ticker)
                        cat("\n_______________________________________\n")
                        cat("Quality Score: ", theObject@quality)
                        cat("\n_______________________________________\n")
                        profitability <- data.frame(profitability = theObject@profitability,
                                                    GPOA = theObject@pGPOA,
                                                    ROE = theObject@pROE,
                                                    ROA = theObject@pROA,
                                                    CFOA = theObject@pCFOA,
                                                    GMAR = theObject@pGMAR,
                                                    ACC = theObject@pACC)
                        print.data.frame(profitability)
                        cat("\n\n")
                        
                        growth <- data.frame(growth = theObject@growth,
                                             GPOA = theObject@gGPOA,
                                             ROE = theObject@gROE,
                                             ROA = theObject@gROA,
                                             CFOA = theObject@gCFOA,
                                             GMAR = theObject@gGMAR,
                                             ACC = theObject@gACC)
                        print.data.frame(growth)
                        cat("\n\n")
                        
                        safety <- data.frame(safety = theObject@safety,
                                             BAB = theObject@sBAB,
                                             IVOL = theObject@sIVOL,
                                             LEV = theObject@sLEV,
                                             OhlsonOScore = theObject@sO,
                                             AltmanZScore = theObject@sZ)
                        print.data.frame(safety)
                        cat("\n\n")
                        
                        payouts <- data.frame(payouts = theObject@payouts,
                                                    EISS = theObject@pEISS,
                                                    DISS = theObject@pDISS,
                                                    NPOP = theObject@pNPOP)
                        print.data.frame(payouts)
                        cat("\n_______________________________________\n")
                      }
                      )