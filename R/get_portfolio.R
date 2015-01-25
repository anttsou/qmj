get_portfolio <- function(top=5, bottom=5) {
  filepath <- system.file(package="qmj")
  data(marketdata, package="qmj")
  if(length(marketdata$names) >= top + bottom) {
    summarydata <- data.frame(Top.Companies.by.Measured.Quality = marketdata$names[1:top], 
                              Bottom.Companies.by.Measured.Quality = marketdata$names[length(marketdata$names) - bottom:
                                                                                      length(marketdata$names)], 
                              Standard.Deviation.of.Quality.Scores = sd(marketdata$quality),
                              Interquartile.Range.of.Quality.Scores = IQR(marketdata$quality))
  } else {
    stop("Portfolio does not have enough companies given values for parameters \"top\" and \"bottom\"")
  }
  summarydata
}