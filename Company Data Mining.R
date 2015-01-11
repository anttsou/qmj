val <- "1956 235 949 1119 1804 277 1971 1886 165 1024 777 1801 889 1661 ## [15] 1465 1610 770 1359 8 1650 15 411 1793 1209 750 861 74 1920 ## [29] 851 1887 1748 1260 1910 1217 656 682 783 1540 77 1468 1327 336 ## [43] 511 1007 1321 1921 1484 1106 1658 370 529 1614 1350 469 84 274 ## [57] 421 931 384 1396 16 728 997 4 1126 306 695 1248 1499 1088 ## [71] 451 174 1991 588 1285 1 402 1794 1779 1410 640 988 1427 1187 ## [85] 1200 416 415 744 1802 1839 1413 1400 1022 5 1161 1594 1431 862 ## [99] 1019 1907"
val <- gsub("## ", "", val)
val <- gsub("\\[\\d\\d\\]", "", val)
val
library(stringr)
val <- str_split(val, " ")
val <- val[val != ""]
val
val <- sapply(val, as.numeric)
val
val <- val[!is.na(val)]
val
last50 <- val[51:100]
length(last50)

Site <- "http://www.forbes.com/global2000/list/#page:1_sort:0_direction:asc_search:_filter:All%20industries_filter:All%20countries_filter:All%20states"
last50
#BloomR Portable R for bloomberg

setwd("econ20/R Paper/Discrete")
getwd()
cLinks <- rep("", 50)
cNames <- rep("", 50)
cTicker <- rep("", 50)

cLinks[1] <- "http://www.bloomberg.com/quote/GMEXICOB:MM" 
cNames[1] <- ""
cTicker[1] <- "GMEXICOB:MM"
c1 <- "http://www.bloomberg.com/quote/GMEXICOB:MM"
c1Name <- "GMEXICOB:MM"
download.file(c1, destfile = c1Name)


cLinks[2] <- "http://www.marketwatch.com/investing/stock/eres" 
cNames[2] <- ""
cTicker[2] <- "ERES"

cLinks[3] <- "https://www.google.com/finance?q=OTCMKTS:ELUXY" 
cNames[3] <- "ElectroLux"
cTicker[3] <- "ELUXY"

last50[4]
cLinks[4] <- "http://www.bloomberg.com/quote/GFNORTEO:MM"
cNames[4] <- ""
cTicker[4] <- "GFNORTEO"

last50[5]
cLinks[5] <- "https://www.google.com/finance?cid=661224"
cNames[5] <- "Boeing"
cTicker[5] <- "BA"

last50[6]
cLinks[6] <- "http://www.bloomberg.com/quote/000001:CH"
cNames[6] <- "Ping An Bank Co Ltd"
cTicker[6] <- "000001:CH"

last50[7]
cLinks[7] <- "https://www.google.com/finance?cid=34649"
cNames[7] <- "Texas Instruments Inc"
cTicker[7] <- "TXN"

last50[8]
cLinks[8] <- "http://www.bloomberg.com/quote/BSFR:AB"
cNames[8] <- "Banque Saudi Fransi"
cTicker[8] <- "BSFR:AB"

last50[9]
cLinks[9] <- "https://www.google.com/finance?q=MCX:AFKS"
cNames[9] <- "Sistema"
cTicker[9] <- "MCX:AFKS"

last50[10]
cLinks[10] <- "https://www.google.com/finance?cid=690834"
cNames[10] <- "Sany Heavy Industry Co., LTD"
cTicker[10] <- "SHA:600031"

last50[11]
cLinks[11] <- "https://www.google.com/finance?q=NYSE:C"
cNames[11] <- "Citigroup Inc"
cTicker[11] <- "NYSE:C"

last50[12]
cLinks[12] <- "https://www.google.com/finance?cid=674894"
cNames[12] <- "Fujitsu Ltd"
cTicker[12] <- "TYO:6702"

last50[13]
cLinks[13] <- "https://www.google.com/finance?cid=705276"
cNames[13] <- "Continental Resources, Inc."
cTicker[13] <- "NYSE:CLR"

last50[14]
cLinks[14] <- "https://www.google.com/finance?cid=665639"
cNames[14] <- "JPMorgan Chase & Co."
cTicker[14] <- "NYSE:JPM"

last50[15]
cLinks[15] <- "https://www.google.com/finance?cid=214336"
cNames[15] <- "Fiserv Inc"
cTicker[15] <- "NASDAQ:FISV"

last50[16]
cLinks[16] <- "https://www.google.com/finance?cid=712515"
cNames[16] <- "China Communications Construction Co Lt"
cTicker[16] <- "HKG:1800"

last50[17]
cLinks[17] <- "http://www.bloomberg.com/quote/SCR:FP"
cNames[17] <- "Scor"
cTicker[17] <- "SCR:FP"

last50[18]
cLinks[18] <- "http://www.bloomberg.com/quote/NLMK:RM"
cNames[18] <- "Novoliptsk Steel OJSC"
cTicker[18] <- "NLMK:RM"

last50[19]
cLinks[19] <- "https://www.google.com/finance?q=NYSE:SNI"
cNames[19] <- "Scripps Networks Interactive, Inc."
cTicker[19] <- "NYSE:SNI"

last50[20]
cLinks[20] <- "http://www.bloomberg.com/quote/3698:HK"
cNames[20] <- "Huishang Bank"
cTicker[20] <- "3698:HK"

last50[21]
cLinks[21] <- "https://www.google.com/finance?q=NYSE:CHK"
cNames[21] <- "Chesapeake Energy Corporation"
cTicker[21] <- "NYSE:CHK"

last50[22]
cLinks[22] <- "https://www.google.com/finance?cid=656715"
cNames[22] <- "British American Tobacco PLC (ADR)"
cTicker[22] <- "NYSEMKT:BTI"

last50[23]
cLinks[23] <- "https://www.google.com/finance?cid=1123987"
cNames[23] <- "MEIJI Holdings Co Ltd"
cTicker[23] <- "TYO:2269"

last50[24]
cLinks[24] <- "http://www.bloomberg.com/quote/8830:JP"
cNames[24] <- "Sumitomo Realty & Development Co Ltd"
cTicker[24] <- "8830:JP"

last50[25]
cLinks[25] <- "https://www.google.com/finance?cid=674368"
cNames[25] <- "Taisei Corp"
cTicker[25] <- "TYO:1801"

dataF <- data.frame(cLinks, cNames, cTicker)
dataF <- dataF[1:26,]
dataF

write.table(dataF, file="Ryan25BigCompany.csv")
