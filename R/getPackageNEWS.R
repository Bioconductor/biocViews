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
             srcDir, tarred=TRUE)
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
        tryCatch({
            db <- tools:::.build_news_db(pkg, dirname(srcdir), NULL, NULL)
            if (!is.null(db))
                utils::news(Version>ver, db=db)
            else NULL
        }, error=function(...) NULL)
    }

    getTarredNews <- function(pkg, ver, tarball) {
        fl <- grep("/NEWS(.Rd)?$", untar(tarball, list=TRUE), value=TRUE)
        if (length(fl)) {
            tryCatch({
                exdir <- tempfile()
                untar(tarball, exdir=exdir)
                getTarredNews(pkg, ver, exdir)
            } ,error=function(...) NULL)
        } else NULL
    }

    if (tarred) {
        FUN <- getTarredNews
        src <- file.path(srcDir, sprintf("%s_%s.tar.gz", names(vers), vers))
    } else {
        FUN <- getNews
        src <- file.path(srcDir, names(vers))
    }
    Filter(function(x) !is.null(x) && 0L != nrow(x),
           Map(FUN, names(vers), vers, src))
}

printNEWS <- function(dbs, destfile, overwrite=FALSE, width=68, ...)
{
    if (file.exists(destfile) && !overwrite)
        stop(.msg("'%s' exists and overwrite=FALSE", destfile))
    file <- file(destfile, "w+")
    on.exit(close(file))
    dbs <- lapply(dbs, function(db) {
        db[["Text"]] <- lapply(db[["Text"]], function(elt) {
            paste(strwrap(elt, width=options()[["width"]] - 10),
                  collapse="\n")
        })
        db
    })
    for (i in seq_along(dbs)) {
        cat(sprintf("\n* %s\n\n", names(dbs)[[i]]), file=file)
        capture.output(dbs[[i]], file=file)
    }
}
