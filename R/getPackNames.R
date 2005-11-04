
#library(biocViews)
#gg <- getPacksAndViews("http://bioconductor.org/packages/bioc/1.8/src/contrib/")
#data(oct05)
#pp <- permulist(gg, oct05, "vocRoot")
#ct <- getCTVs(pp,oct05)
#sapply(ct, bcv2html)


.available.packages2 <- function (contriburl = contrib.url(getOption("repos")), method) 
{
    .checkRversion <- function(x) {
        if (is.na(xx <- x["Depends"])) 
            return(TRUE)
        xx <- tools:::.split_dependencies(xx)
        if (length(z <- xx[["R"]]) > 1) 
            eval(parse(text = paste("currentR", z$op, "z$version")))
        else TRUE
    }
    flds <- c("Package", "Version", "Priority", "Bundle", "Depends", 
        "Imports", "Suggests", "Contains", "biocViews")
    res <- matrix(as.character(NA), 0, length(flds) + 1)
    colnames(res) <- c(flds, "Repository")
    for (repos in contriburl) {
        localcran <- length(grep("^file:", repos)) > 0
        if (localcran) {
            tmpf <- paste(substring(repos, 6), "PACKAGES", sep = "/")
            tmpf <- sub("^//", "", tmpf)
            if (.Platform$OS.type == "windows") {
                if (length(grep("[A-Za-z]:", tmpf))) 
                  tmpf <- substring(tmpf, 2)
            }
            res0 <- read.dcf(file = tmpf, fields = flds)
            if (length(res0)) 
                rownames(res0) <- res0[, "Package"]
        }
        else {
            dest <- file.path(tempdir(), paste("repos_", URLencode(repos, 
                TRUE), ".rds", sep = ""))
            if (file.exists(dest)) {
                res0 <- .readRDS(dest)
            }
            else {
                tmpf <- tempfile()
                on.exit(unlink(tmpf))
                op <- options("warn")
                options(warn = -1)
                z <- try(download.file(url = paste(repos, "PACKAGES.gz", 
                  sep = "/"), destfile = tmpf, method = method, 
                  cacheOK = FALSE, quiet = TRUE, mode = "wb"), 
                  silent = TRUE)
                if (inherits(z, "try-error")) {
                  z <- try(download.file(url = paste(repos, "PACKAGES", 
                    sep = "/"), destfile = tmpf, method = method, 
                    cacheOK = FALSE, quiet = TRUE, mode = "wb"), 
                    silent = TRUE)
                }
                options(op)
                if (inherits(z, "try-error")) {
                  warning(gettextf("unable to access index for repository %s", 
                    repos), call. = FALSE, immediate. = TRUE, 
                    domain = NA)
                  next
                }
                res0 <- read.dcf(file = tmpf, fields = flds)
                if (length(res0)) 
                  rownames(res0) <- res0[, "Package"]
                .saveRDS(res0, dest, compress = TRUE)
                unlink(tmpf)
                on.exit()
            }
        }
        res0 <- cbind(res0, Repository = repos)
        res <- rbind(res, res0)
    }
    if (length(res)) {
        .checkRversion <- function(x) {
            if (is.na(xx <- x["Depends"])) 
                return(TRUE)
            xx <- tools:::.split_dependencies(xx)
            if (length(z <- xx[["R"]]) > 1) 
                eval(parse(text = paste("currentR", z$op, "z$version")))
            else TRUE
        }
        currentR <- getRversion()
        res <- res[apply(res, 1, .checkRversion), , drop = FALSE]
    }
    res
}


getPacksAndViews <- function(reposURL) {
 pstruc <- .available.packages2(reposURL)
 ns <- pstruc[,"Package"]
 bcv <- pstruc[,"biocViews"]
 bcv[is.na(bcv)] <- "NoViewProvided"
 bcv <- gsub("\\\n","",bcv)
 bcvl <- strsplit(bcv, ", *")
 names(bcvl) <- ns
# patch up some usages that do not capitalize first letter
 bcvl <- lapply(bcvl, function(x) gsub("\\b(\\w)", "\\U\\1", x, perl=TRUE))
 names(bcvl) <- ns
 bcvl
}


permulist <- function(allv, vocab, root, interp=TRUE) {
    lens <- sapply(allv, length)
    packnames <- names(allv)
    repp <- rep(packnames, lens)
    ans <- split(repp, unlist(allv))
    if (interp) 
        pump(ans, vocab, root)
    else ans
}

