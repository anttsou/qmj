incomeStatement <- function( url1, url2, url3, url4, qkorder){
        ## load neccesary librarys
        library(XBRL)
        library(xlsx)
        
        ## obtain the raw data from url(s)
        raw1 <- xbrlDoAll(url1)
        raw2 <- xbrlDoAll(url2)
        raw3 <- xbrlDoAll(url3)
        raw4 <- xbrlDoAll(url4)
        
        ## turn them into sec
        rawsec1 <- xbrlSECdev01(raw1)
        rawsec2 <- xbrlSECdev01(raw2)
        rawsec3 <- xbrlSECdev01(raw3)
        rawsec4 <- xbrlSECdev01(raw4)
        
        ## obtain data from sec in list form
        rawlist1 <- rawsec1$showStatements()
        rawlist2 <- rawsec2$showStatements()
        rawlist3 <- rawsec3$showStatements()
        rawlist4 <- rawsec4$showStatements()
        
        ## read each rawlist into a dataframe differentiating between 10k and 10q
        ## begin with the first url
        if(qkorder[1] == "q"){
                rawdata1 <- data.frame(rawlist1[[10]][[3]])
        } else {
                rawdata1 <- data.frame(rawlist1[[14]][[3]])
        }
        
        ## now second url
        if(qkorder[2] == "q"){
                rawdata2 <- data.frame(rawlist2[[10]][[3]])
        } else {
                rawdata2 <- data.frame(rawlist2[[14]][[3]])
        }
        
        ## now third url
        if(qkorder[3] == "q"){
                rawdata3 <- data.frame(rawlist3[[10]][[3]])
        } else {
                rawdata3 <- data.frame(rawlist3[[14]][[3]])
        }
        
        ## now fourth url
        if(qkorder[4] == "q"){
                rawdata4 <- data.frame(rawlist4[[10]][[3]])
        } else {
                rawdata4 <- data.frame(rawlist4[[14]][[3]])
        }
        
        ## extract labels
        labels <- rawdata3[,2]
        
        ## extract quarterly data
        q1 <- as.numeric(rawdata1[,5])
        q2 <- as.numeric(rawdata2[,5])
        q3 <- as.numeric(rawdata3[,3])
        y1 <- as.numeric(rawdata4[,3])
        q4 = y1 - q1 - q2 - q3
        
        final <- data.frame(labels, y1, q4, q3, q2, q1)
        
        ## convert to excel document
        write.xlsx(final, file = "income_statement_test.xlsx", sheetName = "Sheet1", col.names = TRUE )
        
        
        
}