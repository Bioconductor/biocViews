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
    function(prevRepos="http://www.bioconductor.org/packages/2.8/bioc",
             currRepos="http://www.bioconductor.org/packages/2.9/bioc",
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

    Filter(function(x) !is.null(x) && 0L != nrow(x),
           Map(getNews, names(vers), vers, srcDir))
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
            print(dbs[[i]])
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
