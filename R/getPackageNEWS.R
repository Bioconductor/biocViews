.msg <-
    function(fmt, ..., width=getOption("width"))
    ## Use this helper to format all error / warning / message text
{
    strwrap(sprintf(fmt, ...), width=width, exdent=4)
}


## collate package NEWS files using starting version number in
## prevRepos, and membership in currRepos as references. Package
## source tree rooted at srcDir, possibiblly as tarred files
getPackageNEWS <-
    function(prevRepos="http://www.bioconductor.org/packages/2.10/bioc",
             currRepos="http://www.bioconductor.org/packages/2.11/bioc",
             srcDir)
{
    prev <- available.packages(contrib.url(prevRepos, "source"))
    curr <- available.packages(contrib.url(currRepos, "source"))
    prev <- prev[rownames(prev) %in% rownames(curr),]

    newpkgs <- setdiff(rownames(curr), rownames(prev))
    idx <- package_version(curr[newpkgs, "Version"]) >= "0.99.0"
    newpkgs <- newpkgs[idx]
    vers <- c(sub("\\.[[:digit:]]?$", ".0", prev[,"Version"]),
              setNames(rep("0.0", length(newpkgs)), newpkgs))

    getNews <- function(pkg, ver, srcdir) {
        newsloc <- file.path(srcdir, pkg, c("inst", "inst", "."),
                             c("NEWS.Rd", "NEWS", "NEWS"))
        news <- head(newsloc[file.exists(newsloc)], 1)
        if (0L == length(news))
            return(NULL)
        tryCatch({
            db <- 
                if (grepl("Rd$", news))
                    tools:::.build_news_db_from_package_NEWS_Rd(news)
                else
                    tools:::.news_reader_default(news)
            if (!is.null(db))
                utils::news(Version>ver, db=db)
            else NULL
        }, error=function(...) NULL)
    }

    ret <- Filter(function(x) !is.null(x) && 0L != nrow(x),
           Map(getNews, names(vers), vers, srcDir))
    nms <- names(ret)
    s <- sort(nms)
    newRet <- ret[s]
}


## based on tools:::.build_news_db()
getNEWSFromFile <- function (dir, destfile, format = NULL, reader = NULL,
    output=c("md", "text")) 
{
    mdIfy <- function(txt) 
    {
        lines <- strsplit(txt, "\n")
        segs <- lines[[1]]
        segs <- sub("^    o +", "- ", segs)
        segs <- sub("^\t", "  ", segs)
        return(paste(segs, collapse="\n"))
    }
    
    newsRdFile <- file.path(dir, "NEWS.Rd") ## should never be found
    newsRdFile2 <- file.path(dir, "inst", "NEWS.Rd") 
    
    if (!file_test("-f", newsRdFile) && !file_test("-f", newsRdFile2)) {
        nfile <- file.path(dir, "NEWS")
        nfile2 <- file.path(dir, "inst", "NEWS")

        if (!file_test("-f", nfile) && !file_test("-f", nfile2))
            return(invisible())

        nfile <- ifelse(file_test("-f", nfile), nfile, nfile2)

        if (!is.null(format)) 
            .NotYetUsed("format", FALSE)
        if (!is.null(reader)) 
            .NotYetUsed("reader", FALSE)

        file <- file(destfile, "w+")
        on.exit(close(file))
        news <- paste(readLines(nfile), collapse="\n")
        if ("md" == output)
            news = mdIfy(news)
        cat(news, file=file)
        return(invisible())
    }

    newsRdFile <- ifelse(file_test("-f", newsRdFile), newsRdFile, newsRdFile2)
    
    file <- file(destfile, "w+")
    on.exit(close(file))
    db <- tools:::.build_news_db_from_package_NEWS_Rd(newsRdFile)
    news <- NULL
    try(news <- capture.output(print(db)))
    if (is.null(news))
    {
        message(sprintf("Error building news database for %s/%s",
            dir, destfile))
        return(invisible())
    }
    news <- paste(news, collapse="\n")
    if ("md" == output)
        news <- mdIfy(news)
    cat(news, file=file)
    return(invisible())
}


printNEWS <- function(dbs, destfile, overwrite=FALSE, width=68,
                      output=c("md", "text"), ...)
{
    output <- match.arg(output)
    dbs <- lapply(dbs, function(db) {
        db[["Text"]] <- lapply(db[["Text"]], function(elt) {
            paste(strwrap(elt, width=options()[["width"]] - 10),
                  collapse="\n")
        })
        db
    })
    txt <- capture.output({
        for (i in seq_along(dbs)) {
            cat(sprintf("\n%s\n%s\n\n", names(dbs)[[i]],
                        paste(rep("-", nchar(names(dbs)[[i]])), collapse="")))
            tryCatch({
                print(dbs[[i]])
            }, error=function(err) {
                warning("print() failed for ", sQuote(names(dbs)[[i]]),
                        immediate.=TRUE, call.=FALSE)
            })
        }
    })
    if ("md" == output) {
        txt <- sub("^    o  ", "-", txt)
        txt <- sub("^\t", "  ", txt)
    }

    if (!is(destfile, "connection")) {
        if (file.exists(destfile) && !overwrite)
            stop(.msg("'%s' exists and overwrite=FALSE", destfile))
        file <- file(destfile, "w+")
        on.exit(close(file))
    } else file = destfile
    writeLines(txt, file)
}

getNewPackageTitles <-
    function(prevRepos="http://www.bioconductor.org/packages/2.8/bioc",
             currRepos="http://www.bioconductor.org/packages/2.9/bioc",
             srcDir)
{
    prev <- available.packages(contrib.url(prevRepos, "source"))
    curr <- available.packages(contrib.url(currRepos, "source"))

    new <- sort(setdiff(rownames(curr), rownames(prev)))
    sapply(new, function(elt) {
        read.dcf(file.path(srcDir, elt, "DESCRIPTION"), "Title")
    })
}

printNewPackageTitles <- function(titles, destfile, overwrite=FALSE)
{
    if (!is(destfile, "connection")) {
        if (file.exists(destfile) && !overwrite)
            stop(.msg("'%s' exists and overwrite=FALSE", destfile))
        file <- file(destfile, "w+")
        on.exit(close(file))
    } else file = destfile
    cat(strwrap(sprintf("\n- %s: %s", names(titles), titles),
                width=70, exdent=2),
        file=stdout(), sep="\n")
}
