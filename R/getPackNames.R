
getPackNames <- function (rpackDir, majv = 1) #, dest = tempfile()) 
{
#
# searches a given source directory for packages with major version > majv
#
    curd <- getwd()
    on.exit(setwd(curd))
    setwd(rpackDir)
    cands <- dir(patt = "DESCRIPTION", recurs = TRUE)
    lkdep <- strsplit(cands, "/")
    el2 <- sapply(lkdep, function(x) x[2])
    ok <- el2 == "DESCRIPTION"
    cands <- cands[ok]
    allde <- lapply(cands, readLines)
    sallde <- lapply(allde, function(x) {
        tmp <- strsplit(x, ": ")
        nms <- sapply(tmp, function(x) x[1])
        names(tmp) <- nms
        tmp
    })
    pt <- sapply(sallde, function(x) x$Title[2])
    pn <- sapply(sallde, function(x) x$Package[2])
    pv <- sapply(sallde, function(x) x$Version[2])
    ppv <- strsplit(pv, "\\.")
    mv <- sapply(ppv, function(x) as.numeric(x[1]))
    packNms <- pn[mv >= majv]
    packNms
#    packTits <- pt[mv >= majv]
#    writeLines(paste(packNms, packTits, sep = "+"), dest)
#    cat("package names+titles written to", dest)
#    invisible(dest)
}

