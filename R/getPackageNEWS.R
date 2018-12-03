.msg <-
    function(fmt, ..., width=getOption("width"))
    ## Use this helper to format all error / warning / message text
{
    strwrap(sprintf(fmt, ...), width=width, exdent=4)
}


## collate package NEWS files using starting version number in
## prevRepos, and membership in currRepos as references. Package
## source tree rooted at srcDir, possibiblly as tarred files

# repo:  bioc data/experiment workflows
getPackageNEWS <- function(prevRepos="3.6",
                           currRepos="3.7",
                           repo=c("bioc", "data/experiment", "workflows")){

    repo <- match.arg(repo)
    URL_BASE <- "http://master.bioconductor.org/packages/"
    VIEWS <- "%s%s/%s/VIEWS"

    prevUrl <- url(sprintf(VIEWS, URL_BASE, prevRepos, repo))
    prev <- read.dcf(prevUrl, fields=c("Package", "Version"))
    rownames(prev) <- prev[,1]
    close(prevUrl)
    currUrl <- url(sprintf(VIEWS, URL_BASE, currRepos, repo))
    curr <- read.dcf(currUrl, fields=c("Package", "Version"))
    rownames(curr) <- curr[,1]
    close(currUrl)

    prev <- prev[rownames(prev) %in% rownames(curr),]
    newpkgs <- setdiff(rownames(curr), rownames(prev))

    idx <- package_version(curr[newpkgs, "Version"]) >= "0.99.0"
    newpkgs <- newpkgs[idx]
    vers <- c(sub("\\.[[:digit:]]?$", ".0", prev[,"Version"]),
              setNames(rep("0.0", length(newpkgs)), newpkgs))

    temp = tempdir()
    system(paste0("scp -r webadmin@master.bioconductor.org:/extra/www/bioc/packages/",
                  currRepos, "/", repo, "/news ", temp))


    getNews <- function(pkg, ver, srcdir) {
        newsloc <- file.path(srcdir, pkg, c("inst", "inst", "inst", ".","."),
                             c("NEWS.Rd", "NEWS", "NEWS.md", "NEWS.md", "NEWS"))
        news <- head(newsloc[file.exists(newsloc)], 1)
        if (0L == length(news))
            return(NULL)
        tryCatch({
            db <-
                if (grepl("Rd$", news)){
                    tools:::.build_news_db_from_package_NEWS_Rd(news)
                } else if (grepl("md$", news)){
                    tools:::.build_news_db_from_package_NEWS_md(news)
                } else {
                    tools:::.news_reader_default(news)
                }
            if (!is.null(db))
                utils::news(Version > ver, db=db)
            else NULL
        }, error=function(...) NULL)
    }

    ret <- Filter(function(x) !is.null(x) && 0L != nrow(x),
                  Map(getNews, names(vers), vers, paste0(temp, "/news")))
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


        newsMdFile <- file.path(dir, "NEWS.md")
        newsMdFile2 <- file.path(dir, "inst", "NEWS.md")

        if (!file_test("-f", newsMdFile) && !file_test("-f", newsMdFile2)) {


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

        newsMdFile <- ifelse(file_test("-f", newsMdFile), newsMdFile,
                             newsMdFile2)
        file <- file(destfile, "w+")
        on.exit(close(file))
        db <- tools:::.build_news_db_from_package_NEWS_md(newsMdFile)
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
        db[["Text"]] <- sapply(db[["Text"]], function(elt) {
            paste(strwrap(elt, width=options()[["width"]] - 10),
                  collapse="\n")
        })
        db
    })
    txt <- capture.output({
        for (i in seq_along(dbs)) {
            tryCatch({
                cat(sprintf(
                    "\n[%s](https://bioconductor.org/packages/%s)\n%s\n\n",
                    names(dbs)[[i]], names(dbs)[[i]],
                    paste(rep("-", nchar(names(dbs)[[i]])), collapse="")))
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

# manifest:  software.txt data-experiment.txt workflows.txt
# status:  new or removed
getPackageTitles <- function(prevBranch="RELEASE_3_6",
                             currBranch="master",
                             manifest=c("software.txt", "data-experiment.txt", "workflows.txt"),
                             status = c("new", "removed")){

   manifest <- match.arg(manifest)
   status <- match.arg(status)

   GIT_ARCHIVE <-
       "git archive --remote=ssh://git@git.bioconductor.org/admin/manifest %s %s | tar -xO"
   prevRepo <- system(sprintf(GIT_ARCHIVE, prevBranch, manifest), intern=TRUE)
   prevRepo <- trimws(gsub(pattern = "Package: ", replacement="",
                           prevRepo[-which(prevRepo=="")]))
   currRepo <- system(sprintf(GIT_ARCHIVE, currBranch, manifest), intern=TRUE)
   currRepo <- trimws(gsub(pattern = "Package: ", replacement="",
                           currRepo[-which(currRepo=="")]))

   # switch statement
   pkgs <- switch(status,
                  new = setdiff(currRepo, prevRepo),
                  removed = setdiff(prevRepo, currRepo)
                  )
   pkgs
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

getPackageDescriptions <- function(pkgs, outfile, output=c("md", "text"))
{

    output <- match.arg(output)
    if (output == "text")
        exdent = 4
    else
        exdent = 2
    plower <- tolower(pkgs)
    names(plower) <- pkgs
    pkgs <- names(sort(plower))

    file <- tempfile()
    DESC_FILE <-
        "git archive --remote=ssh://git@git.bioconductor.org/packages/%s master DESCRIPTION|tar -xO > %s"

    desc = lapply(pkgs, function(pkg) {
        system(sprintf(DESC_FILE, pkg, file))
        d = read.dcf(file)[,"Description"]
        paste(strwrap(sprintf("- [%s](https://bioconductor.org/packages/%s) %s",
                              pkg, pkg, d), width=70, exdent=exdent),
              collapse="\n")
    })
    cat(noquote(unlist(desc)), sep="\n\n", file=outfile)
    invisible(NULL)
}
