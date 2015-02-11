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
#' @name qmj
#' @rdname qmj-class
#' @aliases qmj-class
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
#' @aliases view_ticker,qmj-method
#' @return The ticker of the qmj object as a character.
#' @export
#' @docType methods
#' @rdname view_ticker-methods
#' 
#' @examples
#' \dontrun{
#' qmjs <- data(qmjs)
#' view_ticker(qmjs[[1]])
#' }
setGeneric(name="view_ticker",
           def=function(theObject)
           {
             standardGeneric("view_ticker")
           }
)

setMethod(f="view_ticker",
          signature="qmj",
          definition=function(theObject)
          {
            return(theObject@ticker)
          }
)

#' @title Returns the profitability of the qmj object.
#' @aliases view_profitability,qmj-method
#' @return A data frame with 1 row and 8 variables that describes profitability of the qmj object.
#' @export
#' @docType methods
#' @rdname view_profitability-methods
#' 
#' @examples
#' \dontrun{
#' qmjs <- data(qmjs)
#' view_profitability(qmjs[[1]])
#' }
setGeneric(name="view_profitability",
                      def=function(theObject)
                      {
                        standardGeneric("view_profitability")
                      }
                      )

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
#' plot_quality(qmjs[[1]], quality)
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
                        ggplot2::ggplot(df, aes(x=quality)) +
                          geom_histogram(data=subset(df, cond==FALSE), binwidth=pbinwidth, origin=min, fill="black") +
                          geom_histogram(data=subset(df, cond==TRUE), binwidth=pbinwidth, origin=min, fill="gold")
                        
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