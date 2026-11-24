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
                           repo=c("bioc", "data/experiment", "workflows"),
                           srcdir=NULL){

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

    idx <- package_version(curr[newpkgs, "Version"], strict=FALSE) >= "0.99.0"
    newpkgs <- newpkgs[idx]
    vers <- c(sub("\\.[[:digit:]]?$", ".0", prev[,"Version"]),
              setNames(rep("0.0", length(newpkgs)), newpkgs))

    if (is.null(srcdir)){
        temp = tempdir()
        system(paste0("scp -r webadmin@master.bioconductor.org:/extra/www/bioc/packages/",
                      currRepos, "/", repo, "/news ", temp))
        srcdir <- paste0(temp, "/news")
    }

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
                  Map(getNews, names(vers), vers, srcdir))
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


## Keep the first (newest) whole NEWS items of a package's printed NEWS
## 'lines' so that at most 'maxLines' non-blank lines remain. Items are
## never split: a unit starts at a 'Changes in version' header, at a
## top-level bullet, or at an unindented line after a blank line (a
## category heading). The first unit is always kept, and headings left
## without items are dropped. Returns the kept lines, with attribute
## 'truncated'.
.truncateNEWSLines <- function(lines, maxLines)
{
    nonblank <- nzchar(trimws(lines))
    truncated <- is.finite(maxLines) && sum(nonblank) > maxLines
    if (!truncated)
        return(structure(lines, truncated=FALSE))

    indent <- nchar(sub("^(\\s*).*", "\\1", lines))
    version <- grepl("^\\s{5,}Changes in version", lines)
    bullet <- grepl("^\\s*[-o*+•]\\s+\\S", lines) & !version
    base <- if (any(bullet)) min(indent[bullet]) else 0L
    prevBlank <- c(TRUE, !nonblank[-length(lines)])
    fence <- cumsum(grepl("^\\s*```", lines)) %% 2L == 1L
    start <- nonblank & !fence &
        (version | (bullet & indent == base) |
         (indent <= base & prevBlank))
    start[which(nonblank)[1L]] <- TRUE
    unit <- cumsum(start)
    unitLines <- tabulate(unit[nonblank], nbins=max(unit))
    keep <- max(1L, sum(cumsum(unitLines) <= maxLines))
    ## drop trailing units that are only a heading
    isHeading <- function(u) {
        l <- lines[unit == u & nonblank]
        length(l) == 1L && !grepl("^\\s*[-o*+•]\\s", l)
    }
    while (keep > 1L && isHeading(keep))
        keep <- keep - 1L
    kept <- lines[unit <= keep]
    while (length(kept) && !nzchar(trimws(kept[length(kept)])))
        kept <- kept[-length(kept)]
    structure(kept, truncated=TRUE)
}

## markdown fix-ups of printed NEWS lines
.mdNEWS <- function(txt)
{
    txt <- sub("^    o  ", "-", txt)
    txt <- sub("^\t", "  ", txt)
    ## 'o' is a valid bullet for R's NEWS reader, but 'o' (or bullet
    ## character) lines kept inside an item's text are not markdown list
    ## markers, and kramdown merges them into the parent bullet
    txt <- sub("^(\\s+)[o•] ", "\\1- ", txt)
    ## a line starting with an HTML-like tag (e.g. '<save_path>') is read
    ## by kramdown as an unclosed HTML block that hides the rest of the
    ## page; autolinks such as <https://...> are left alone
    sub("^(\\s*)<(?![a-z]+://)(?=[A-Za-z/!])", "\\1&lt;", txt, perl=TRUE)
}

printNEWS <- function(dbs, destfile, overwrite=FALSE, width=68,
                      output=c("md", "text"), relativeLink=FALSE,
                      maxLines=50L,
                      repo=c("bioc", "data/experiment", "workflows"), ...)
{
    output <- match.arg(output)
    repo <- match.arg(repo)
    dbs <- lapply(dbs, function(db) {
         db[["Text"]] <- vapply(db[["Text"]], function(elt) {
             elt <- unlist(strsplit(elt, "\n"))
             elt <- paste(strwrap(elt, width=options()[["width"]] - 10),
                          collapse="\n")
             ## items from NEWS.Rd start with a newline, which printed
             ## as an empty bullet followed by the text
             trimws(elt)
         }, character(1L))
         db
     })
    urlBase <- ifelse(relativeLink, "/packages/","https://bioconductor.org/packages/")
    txt <- capture.output({
        for (i in seq_along(dbs)) {
            pkg <- names(dbs)[[i]]
            tryCatch({
                cat(sprintf(
                    "\n[%s](%s%s)\n%s\n\n",
                    pkg, urlBase, pkg,
                    paste(rep("-", nchar(pkg)), collapse="")))
                lines <- capture.output(print(dbs[[i]]))
                if ("md" == output)
                    lines <- .mdNEWS(lines)
                lines <- .truncateNEWSLines(lines, maxLines)
                writeLines(lines)
                if (attr(lines, "truncated")) {
                    newsUrl <- sprintf("%srelease/%s/news/%s/NEWS",
                                       urlBase, repo, pkg)
                    fmt <- if ("md" == output)
                        "\n[See the full NEWS](%s) or run `news(package = \"%s\")`.\n"
                    else
                        "\nSee the full NEWS at %s or run news(package = \"%s\").\n"
                    cat(sprintf(fmt, newsUrl, pkg))
                }
            }, error=function(err) {
                warning("print() failed for ", sQuote(pkg),
                        immediate.=TRUE, call.=FALSE)
            })
        }
    })

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
                             currBranch="devel",
                             manifest=c("software.txt", "data-experiment.txt",
                                        "workflows.txt", "data-annotation.txt", "books.txt"),
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

getPackageDescriptions <- function(pkgs, outfile, output=c("md", "text"),relativeLink=FALSE)
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
        "git archive --remote=ssh://git@git.bioconductor.org/packages/%s devel DESCRIPTION|tar -xO > %s"

    urlBase <- ifelse(relativeLink, "/packages/","https://bioconductor.org/packages/")
    desc = lapply(pkgs, function(pkg) {
        system(sprintf(DESC_FILE, pkg, file))
        d = read.dcf(file)[,"Description"]
        paste(strwrap(sprintf("- [%s](%s%s) %s",
                              pkg, urlBase, pkg, d), width=70, exdent=exdent),
              collapse="\n")
    })
    cat(noquote(unlist(desc)), sep="\n\n", file=outfile)
    invisible(NULL)
}
