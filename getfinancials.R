getfinancials <- function(period) {
  require(XBRL)
  source("xbrlSECdev01.R")
  source("pause.R")
  companies <- read.csv("testcsv2.csv") 
  start1 <- "http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK="
  start2 <- "&type=&dateb=&owner=include&count=40&start="
  start3 <- "http://www.sec.gov/Archives"
  i <- 1
  n <- 0
  counter <- 0
  namesvect <- character()
  namesvect2 <- character()
  maxrows <- 1
  k <- 1
  temp <- list()
  tempindex <- 1
  # loop through each of the company ciks
  while(i <= 8) { 
    indices <- numeric()
    hasNext <- 0
    # make sure the ciks exist
    if(as.character(companies$ciks[i]) != "N/A" && !is.na(companies$ciks[i])) {
      newUrl <- paste(paste(paste(start1,companies$ciks[i],sep=""),start2,sep=""),as.character(n),sep="")
      con <- url(newUrl)
      htmlCode <- readLines(con)
      close(con)
      #pause(5)
      index <- 1
      # if you want 10-Q data
      if(period == 'Q') {
        while(index <= length(htmlCode)) {
          if(grepl(">10-Q",htmlCode[index])) {
            indices <- c(indices,index+1)
          }
          if(grepl("Next 40",htmlCode[index])) {
            hasNext <- 1
          }
          index = index + 1
        }
        if(length(indices) > 0) {
          a <- 1
          while(a <= length(indices) && counter < 4) {
              newUrl2 <- paste(start3,sub("\\\".*","",sub(".*Archives","",htmlCode[indices[a]])),sep="")
              con2 <- url(newUrl2)
              htmlCode2 <- readLines(con2)
              close(con2)
              index2 <- 1
              found2 <- 0
              while(index2 <= length(htmlCode2) && !found2) {
                if(grepl(".*[0-9].xml",htmlCode2[index2],ignore.case=TRUE)) {
                  newUrl3 <- paste(start3,sub("\\\".*","",sub(".*Archives","",htmlCode2[index2])),sep="")
                  options(stringsAsFactors = FALSE)
                  xbrl.vars <- xbrlDoAll(newUrl3)
                  xbrl.sec <- xbrlSECdev01(xbrl.vars)
                  # hacking of showStatements goes here 
                  rawlist <- xbrl.sec$showStatements()
                  pos <- 1
                  done <- 0
                  while(pos <= length(rawlist) && !done) {
                    pos2 <- 1
                    while(pos2 <= length(rawlist[[pos]])) {
                      if(grepl("us-gaap", rawlist[[pos]][[pos2]][1],ignore.case=TRUE)) {
                        cur <- 1
                        labelfound <- 0
                        while(cur <= length(colnames(rawlist[[pos]][[pos2]])) && !labelfound) {
                          if(colnames(rawlist[[pos]][[pos2]])[cur] == "label") {
                            namesvect <- c(namesvect,"label")
                            namesvect2 <- c(namesvect2, paste(companies$tickers[i],"label",sep=" "))
                            labelfound <- 1
                          }  
                          cur = cur + 1
                        }
                        while(cur <= length(colnames(rawlist[[pos]][[pos2]]))) {
                          namesvect <- c(namesvect,colnames(rawlist[[pos]][[pos2]])[cur])
                          namesvect2 <- c(namesvect2, paste(companies$tickers[i],colnames(rawlist[[pos]][[pos2]])[cur],sep=" "))
                          cur = cur + 1
                        }
                        while(k <= length(namesvect)) {
                          if(length(temp) == 0) {
                            temp[[1]] <- rawlist[[pos]][[pos2]][,"label"]
                            maxrows <- length(rawlist[[pos]][[pos2]][,"label"])                        
                          } else {                        
                            temp[[tempindex]] = rawlist[[pos]][[pos2]][,namesvect[k]] 
                            if(length(rawlist[[pos]][[pos2]][,namesvect[k]]) > maxrows) {
                              maxrows <- length(rawlist[[pos]][[pos2]][,namesvect[k]])
                            }
                          }
                          tempindex = tempindex + 1
                          k = k+1
                        }
                      }
                      pos2 = pos2 + 1
                    }
                    pos = pos + 1
                  }
                  found2 <- 1
                  counter = counter + 1
                }
                index2 = index2 + 1
              }
              a = a + 1
          }
        }
        # if you want 10-K data
      } else if(period == 'A') {
        while(index <= length(htmlCode)) {
          if(htmlCode[index]=="<td nowrap=\"nowrap\">10-K</td>") {
            indices <- c(indices,index+1)
          }
          index = index + 1
        }
        a <- 1
        while(a <= length(indices) && counter < 4) {
            newUrl2 <- paste(start3,sub("\\\".*","",sub(".*Archives","",htmlCode[indices[a]])),sep="")
            con2 <- url(newUrl2)
            htmlCode2 <- readLines(con2)
            close(con2)
            index2 <- 1
            found2 <- 0
            while(index2 <= length(htmlCode2) && !found2) {
              if(htmlCode2[index2]=="            <td scope=\"row\">XBRL INSTANCE FILE</td>") {
                newUrl3 <- paste(start3,sub("\\\".*","",sub(".*Archives","",htmlCode2[index2+1])),sep="")
                options(stringsAsFactors = FALSE)
                xbrl.vars <- xbrlDoAll(newUrl3)
                xbrl.sec <- xbrlSECdev01(xbrl.vars)
                # hacking of showStatements goes here 
                rawlist <- xbrl.sec$showStatements()
                rawdata <- rawlist[[10]][[3]]
                if(counter == 1) {
                  q1 <- as.numeric(rawdata[,5])
                } else if(counter == 2) {
                  q2 <- as.numeric(rawdata[,5])
                } else if(counter == 3) {
                  q3 <- as.numeric(rawdata[,5])
                } else if(counter == 4) {
                  q4 <- as.numeric(rawdata[,5])
                }
                found2 <- 1
                counter = counter + 1
              }
              index2 = index2 + 1
            }
            a = a + 1
        }
          # invalid request
      } else {
        stop("Invalid period parameter. Must be 'Q' or 'A'.")
      }
    }
    if(counter == 4 | (length(indices) == 0 & counter == 0) | !hasNext) {
      i = i + 1
      counter <- 0
      n <- 0
    } else {
      n = n + 40
    }
    #pause(5)
  }
  tempi <- 1
  while(tempi <= length(temp)) {
    length(temp[[tempi]]) <- maxrows
    tempi = tempi + 1
  }
  
  matr <- matrix()
  tempcounter <- 1
  while(tempcounter <= length(temp)) {
    if(is.na(matr[1,1]))  {
      matr <- matrix(temp[[tempcounter]])
    } else {
      matr <- cbind(matr, temp[[tempcounter]])
    }
    tempcounter = tempcounter + 1
  }
  rawdata <- data.frame(matr)
  names(rawdata) <- namesvect2
  write.csv(rawdata, file = "income_statement_test2.csv")
}

