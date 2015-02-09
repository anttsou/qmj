Portfolio <- setClass(
  "Portfolio",
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

setGeneric(name="view_ticker",
           def=function(theObject)
           {
             standardGeneric("view_ticker")
           }
)

setMethod(f="view_ticker",
          signature="Portfolio",
          definition=function(theObject)
          {
            return(theObject@ticker)
          }
)

setGeneric(name="view_profitability",
                      def=function(theObject)
                      {
                        standardGeneric("view_profitability")
                      }
                      )

setMethod(f="view_profitability",
                      signature="Portfolio",
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

setGeneric(name="view_growth",
                      def=function(theObject)
                      {
                        standardGeneric("view_growth")
                      }
                      )

setMethod(f="view_growth",
                      signature="Portfolio",
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

setGeneric(name="view_safety",
                      def=function(theObject)
                      {
                        standardGeneric("view_safety")
                      }
                      )

setMethod(f="view_safety",
                      signature="Portfolio",
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

setGeneric(name="view_payouts",
                      def=function(theObject)
                      {
                        standardGeneric("view_payouts")
                      }
                      )

setMethod(f="view_payouts",
                      signature="Portfolio",
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

setGeneric(name="view_quality",
                      def=function(theObject)
                      {
                        standardGeneric("view_quality")
                      }
                      ) 

setMethod(f="view_quality",
                      signature="Portfolio",
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

setGeneric(name="plot_quality",
                      def=function(theObject, quality_data_frame)
                      {
                        standardGeneric("plot_quality")
                      }
                      ) 

setMethod(f="plot_quality",
                      signature=c("Portfolio", "data.frame"),
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

setGeneric(name="summarize",
                      def=function(theObject)
                      {
                        standardGeneric("summarize")
                      }
                      ) 

setMethod(f="summarize",
                      signature=c("Portfolio"),
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