Quality Minus Junk
========================================================
author: Ryan Kwon and Anthoney Tsou
date: January 27, 2015

What is Quality Minus Junk?
========================================================

Quality Minus Junk, or QMJ, is a strategy that focuses on
the quality of stocks rather than prices. The price approach 
is the more conventional method of buying low and selling high. 
QMJ instead focuses on going long, or keeping, high-quality stocks 
and shorting, or selling, low-quality stocks.

The Purpose of qmj
========================================================
Our package, qmj, is an implementation of AQR's paper, 
$Quality \  Minus \ Junk$. However, AQR only looks into historical
portfolios, whereas qmj provides free software for measuring the 
quality of stocks in today's market, or as close to today as we 
can get given publicly available information, and can be easily
updated to have the most recent information.

Using qmj, Help Files
========================================================

```r
library(qmj)
help(package = "qmj")
```

Measuring Quality
========================================================

We will take a look at one of our vignettes.

Collecting Data
========================================================


```r
#get_info()
financials <- collect_market_data()
```

- get_info() will grab financial information from balance sheets, income statements, and balance sheets. 

- collect_market_data() calls the functions in qmj that separately calculate profitability, growth, safety, and payouts, organizing stocks by descending order of quality. 

Viewing Data
========================================================

```r
#head(financials,10)
```

Viewing Data Cont'd
========================================================















```
Error in merge(BS, merge(CF, IS, by = c("ticker", "year")), by = c("ticker",  : 
  object 'BS' not found
```
