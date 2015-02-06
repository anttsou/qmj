company <- setClass(
  "company",
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
    profitability = 0
    pGPOA = "0",
    pROE = "0",
    pROA = "0",
    pCFOA = "0",
    pGMAR = "0",
    pACC = "0",
    growth = "0",
    gGPOA = "0",
    gROE = "0",
    gROA = "0",
    gCFOA = "0",
    gGMAR = "0",
    gACC = "0",
    safety = "0",
    sBAB = "0",
    sIVOL = "0",
    sLEV = "0",
    sO = "0",
    sZ = "0",
    sEVOL = "0",
    payouts = "0",
    pEISS = "0",
    pDISS = "0",
    pNPOP = "0",
    quality = "0"
    ),
  validity = function(object)
  {
    if(!grepl("[A-Z][^a-z]", object@ticker)) {
      return("An invalid ticker was given.")
    }
    if(!identical(colnames(object@profitability),c("ticker","profitability","GPOA","ROE","ROA","CFOA","GMAR","ACC"))) {
      return("An invalid profitability data frame was given.")
    }
    if(!identical(colnames(object@growth),c("ticker","growth","GPOA","ROE","ROA","CFOA","GMAR","ACC"))) {
      return("An invalid growth data frame was given.")
    }
    if(!identical(colnames(object@safety), c("ticker","safety","BAB","IVOL","LEV","O","Z","EVOL"))) {
      return("An invalid profitability data frame was given.")
    }
    if(!identical(colnames(object@payouts), c("ticker","payouts","EISS","DISS","NPOP"))) {
      return("An invalid payouts data frame was given.")
    }
    if(!identical(colnames(object@quality), c("name","ticker","profitability","safety","payouts","quality"))) {
      return("An invalid quality data frame was given.")
    }
    return(TRUE)
  })

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
                        
                        return data.frame(ticker = tick, 
                                   profitability = prof, 
                                   GPOA = gpoa, 
                                   ROE = roe,
                                   ROA = roa,
                                   CFOA = cfoa, 
                                   GMAR = gmar,
                                   ACC = acc)
                      }
                      )

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
                        
                        return data.frame(ticker = tick, 
                                          growth = prof, 
                                          GPOA = gpoa, 
                                          ROE = roe,
                                          ROA = roa,
                                          CFOA = cfoa, 
                                          GMAR = gmar,
                                          ACC = acc)
                      }
                      )

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
                        return data.frame(ticker = tick, 
                                   safety = safety, 
                                   BAB = bab, 
                                   IVOL = ivol,
                                   LEV = lev, 
                                   O = o, 
                                   Z = z, 
                                   EVOL = evol)
                      }
                      )

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
                        return data.frame(ticker = tick, 
                                   payouts = payouts, 
                                   EISS = eiss, 
                                   DISS = diss,
                                   NPOP = npop)
                      }
                      )

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
                        
                        return data.frame(ticker = tick,
                                          quality = quality,
                                          profitability = profitability,
                                          growth = growth,
                                          safety = safety,
                                          payouts = payouts)
                      }
                      )

setGeneric(name="plot_quality",
                      def=function(theObject)
                      {
                        standardGeneric("plot_quality")
                      }
                      ) 

setMethod(f="plot_quality",
                      signature="qmj",
                      definition=function(theObject)
                      {
                        market_dat <- market_data()
                        quality <- market_dat$quality
                        theObject
                        
                      }
                      )

setGeneric(name="summarize",
                      def=function(theObject)
                      {
                        standardGeneric("summarize")
                      }
                      ) 

setMethod(f="summarize",
                      signature="qmj",
                      definition=function(theObject)
                      {
                        return
                      }
                      )