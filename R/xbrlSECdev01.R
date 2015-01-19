xbrlSECdev01 <- function (xbrl.vars) 
{
        self <- xbrl.vars
        self$presentation$preferredLabel[is.na(self$presentation$preferredLabel)] <- "http://www.xbrl.org/2003/role/label"
        self$merged <- merge(x = xbrl.vars$fact, y = xbrl.vars$context, 
                             by = "contextId", sort = FALSE)
        self$merged <- merge(x = self$merged, y = xbrl.vars$element, 
                             by = "elementId", sort = FALSE)
        showStatements <- function (){
                output <- list()
                for (rnt in order(as.numeric(self$role$order))) {
                        temp <- list()
                        this.role <- self$role[rnt, ]
                        if (this.role$type != "Statement") {
                                next
                        }
                        pre <- self$presentation[self$presentation$roleId == 
                                                         this.role$roleId, ]
                        pre <- pre[order(as.numeric(pre$order)), ]
                        find.top.hierarchy <- function(pre, toElementId) {
                                this.line <- pre[pre$toElementId == toElementId, 
                                                 ]
                                if (nrow(this.line) == 0) {
                                        return(toElementId)
                                }
                                return(find.top.hierarchy(pre, this.line$fromElementId[1]))
                        }
                        top.hierarchy <- find.top.hierarchy(pre, pre$toElementId[1])
                        common.ctx <- self$context$contextId
                        for (toElementId in pre$toElementId) {
                                if (nrow(pre[pre$fromElementId == toElementId, ]) == 
                                            0) {
                                        context <- self$merged$contextId[self$merged$elementId == 
                                                                                 toElementId]
                                        if (length(context) > 0) {
                                                common.ctx <- intersect(common.ctx, context)
                                        }
                                }
                        }
                        ctx <- self$context[self$context$contextId %in% common.ctx, 
                                            ]
                        ctx <- ctx[order(ctx$endDate, decreasing = TRUE), ]
                        process.hierarchy <- function(fromElementId, level) {
                                this.level <- pre[pre$fromElementId == fromElementId, 
                                                  ]
                                if (nrow(this.level) > 0) {
                                        for (i in 1:nrow(this.level)) {
                                                if (nrow(pre[pre$fromElementId == this.level$toElementId[i], 
                                                             ]) == 0) {
                                                        this.merged <- self$merged[self$merged$elementId == 
                                                                                           this.level$toElementId[i], ]
                                                        if (nrow(this.merged[this.merged$contextId %in% 
                                                                                     ctx$contextId, ]) > 0) {
                                                                fact <- NULL
                                                                for (this.contextId in ctx$contextId) {
                                                                        fact <- c(fact, this.merged$fact[this.merged$contextId == 
                                                                                                                 this.contextId])
                                                                }
                                                                out <<- rbind(out, c(this.level$toElementId[i], 
                                                                                     self$label$labelString[self$label$elementId == 
                                                                                                                    this.level$toElementId[i] & self$label$labelRole == 
                                                                                                                    this.level$preferredLabel[i]], fact))
                                                        }
                                                }
                                                process.hierarchy(this.level$toElementId[i], 
                                                                  level + 1)
                                        }
                                }
                        }
                        temp[[1]] <- top.hierarchy
                        out <- NULL
                        if (!is.na(top.hierarchy)) {
                                process.hierarchy(top.hierarchy, 0)
                        }
                        if (!is.null(out)) {
                                temp[[2]] <- this.role$description
                                colnames(out) <- c("elementId", "label", ifelse(is.na(ctx$startDate), 
                                                                                ctx$endDate, paste0(ctx$endDate, " (", round((as.Date(ctx$endDate) - 
                                                                                                                                      as.Date(ctx$startDate))/30, 0), " mo)")))
                                temp[[3]] <- out
                        }
                        output[[rnt]] <- temp
                }
                output 
        }
        showPresentationHierarchy <- function(showLabels = TRUE, 
                                              showFacts = FALSE, file = "") {
                if (file != "") {
                        cat("", sep = "", file = file)
                }
                for (rnt in order(as.numeric(self$role$order))) {
                        this.role <- self$role[rnt, ]
                        heading <- paste0("* ", this.role$type, " - ", this.role$description, 
                                          " *")
                        nchar.heading <- nchar(heading)
                        cat("\n\n", rep("*", nchar.heading), "\n", heading, 
                            "\n", rep("*", nchar.heading), "\n\n", sep = "", 
                            file = file, append = TRUE)
                        pre <- self$presentation[self$presentation$roleId == 
                                                         this.role$roleId, ]
                        pre <- pre[order(as.numeric(pre$order)), ]
                        find.top.hierarchy <- function(pre, toElementId) {
                                this.line <- pre[pre$toElementId == toElementId, 
                                                 ]
                                if (nrow(this.line) == 0) {
                                        return(toElementId)
                                }
                                return(find.top.hierarchy(pre, this.line$fromElementId[1]))
                        }
                        top.hierarchy <- find.top.hierarchy(pre, pre$toElementId[1])
                        cat("Id  : ", top.hierarchy, "\n", sep = "", file = file, 
                            append = TRUE)
                        if (showLabels) {
                                cat("Lab : ", self$label$labelString[self$label$elementId == 
                                                                             top.hierarchy & self$label$labelRole == "http://www.xbrl.org/2003/role/label"], 
                                    "\n", sep = "", file = file, append = TRUE)
                        }
                        cat("\n", sep = "", file = file, append = TRUE)
                        process.hierarchy <- function(fromElementId, level) {
                                this.level <- pre[pre$fromElementId == fromElementId, 
                                                  ]
                                if (nrow(this.level) > 0) {
                                        for (i in 1:nrow(this.level)) {
                                                cat(paste(rep("    ", level), collapse = ""), 
                                                    "Id  : ", this.level$toElementId[i], "\n", 
                                                    sep = "", file = file, append = TRUE)
                                                if (showLabels) {
                                                        cat(paste(rep("    ", level), collapse = ""), 
                                                            "Lab : ", self$label$labelString[self$label$elementId == 
                                                                                                     this.level$toElementId[i] & self$label$labelRole == 
                                                                                                     this.level$preferredLabel[i]], "\n", 
                                                            sep = "", file = file, append = TRUE)
                                                }
                                                this.merged <- self$merged[self$merged$elementId == 
                                                                                   this.level$toElementId[i], ]
                                                nrow.this.merged <- nrow(this.merged)
                                                if (showFacts && nrow(pre[pre$fromElementId == 
                                                                                  this.level$toElementId[i], ]) == 0 && nrow.this.merged > 
                                                            0) {
                                                        this.seq <- 1:nrow.this.merged
                                                        cat(paste(rep("    ", level), collapse = ""), 
                                                            "Fact: ", paste0("(", this.seq, ") ", 
                                                                             this.merged$fact, collapse = " | "), 
                                                            "\n", sep = "", file = file, append = TRUE)
                                                        cat(paste(rep("    ", level), collapse = ""), 
                                                            "End : ", paste0("(", this.seq, ") ", 
                                                                             this.merged$endDate, collapse = " | "), 
                                                            "\n", sep = "", file = file, append = TRUE)
                                                        cat(paste(rep("    ", level), collapse = ""), 
                                                            "Mo  : ", paste0("(", this.seq, ") ", 
                                                                             ifelse(is.na(this.merged$startDate), 
                                                                                    0, round((as.Date(this.merged$endDate) - 
                                                                                                      as.Date(this.merged$startDate))/30, 
                                                                                             0)), collapse = " | "), "\n", sep = "", 
                                                            file = file, append = TRUE)
                                                        cat(paste(rep("    ", level), collapse = ""), 
                                                            "Unit: ", paste0("(", this.seq, ") ", 
                                                                             this.merged$unitId, collapse = " | "), 
                                                            "\n", sep = "", file = file, append = TRUE)
                                                }
                                                cat("\n", sep = "", file = file, append = TRUE)
                                                process.hierarchy(this.level$toElementId[i], 
                                                                  level + 1)
                                        }
                                }
                        }
                        if (!is.na(top.hierarchy)) {
                                process.hierarchy(top.hierarchy, 1)
                        }
                }
        }
        list(showPresentationHierarchy = showPresentationHierarchy, 
             showStatements = showStatements)
}