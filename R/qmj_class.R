qmj <- setClass(
  "qmj",
  slots = c(
    ticker = "character",
    profitability = "data.frame",
    growth = "data.frame",
    safety = "data.frame",
    payouts = "data.frame",
    quality = "data.frame"
  ),
  contains = "data.frame",
  prototype = list(
    ticker = "AAPL",
    profitability = function() {
      data(profitability)
      profitability
    }, 
    growth = function() {
      data(growth)
      growth
    },
    safety = function() {
      data(safety)
      safety
    },
    payouts = function() {
      data(payouts)
      payouts
    },
    quality = function() {
      data(quality)
      quality
    }),
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
                        temp.prof <- theObject@profitability
                        temp.tick <- theObject@ticker
                        return(temp.prof[temp.prof$ticker==temp.tick,])
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
                        temp.grow <- theObject@growth
                        temp.tick <- theObject@ticker
                        return(temp.grow[temp.grow$ticker==temp.tick,])
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
                        temp.safe <- theObject@safety
                        temp.tick <- theObject@ticker
                        return(temp.safe[temp.safe$ticker==temp.tick,])
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
                        temp.pay <- theObject@payouts
                        temp.tick <- theObject@ticker
                        return(temp.pay[temp.pay$ticker==temp.tick,])
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
                        temp.qual <- theObject@quality
                        temp.tick <- theObject@ticker
                        return(temp.qual[temp.qual$ticker==temp.tick,])
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
                        return
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


