writeBiocViews <- function(bvList, dir) {
    ## bvList is a list of BiocViews objects
    ## dir is the output directory in which to write the views.
    for (bv in bvList) {
        fn <- file.path(dir, htmlFilename(bv))
        html <- htmlDoc(bv)
        writeHtmlDoc(html, fn)
    }
    ## copy the css
    cssName <- "repository-detail.css"
    cssPath <- system.file(file.path("css", cssName), package="biocViews")
    res <- try(file.copy(cssPath, file.path(dir, cssName)), silent=TRUE)
}

getBiocViews <- function(reposUrl, vocab, local=FALSE) {
    viewList <- getPacksAndViews(reposUrl, vocab, local)
    viewRoster <- permulist(viewList$views, vocab)
    if (local)
      reposUrl <- character(0)
    biocViews <- loadViews(vocab, viewRoster, viewList$pkgList, reposUrl)
    biocViews
}

loadViews <- function(viewGraph, viewRoster, pkgList, reposUrl) {
    views <- nodes(viewGraph)
    viewmat <- as(viewGraph, "matrix")
    viewFactory <- function(name) {
        subViews <- viewmat[name, ] == 1
        if (any(subViews))
          subViews <- views[subViews]
        else
          subViews <- character(0)
        parentViews <- viewmat[ , name] == 1
        if (any(parentViews))
          parentViews <- views[parentViews]
        else
          parentViews <- character(0)
        if (name %in% names(viewRoster)) {
            pkgsInView <- pkgList[viewRoster[[name]]]
        } else
          pkgsInView <- list()
        new("BiocView", name=name, subViews=subViews, parentViews=parentViews,
            packageList=pkgsInView, htmlDir="html", reposRoot=reposUrl)
    }
    biocViews <- lapply(views, viewFactory)
    names(biocViews) <- views
    biocViews
}

getPacksAndViews <- function(reposURL, vocab, local=FALSE) {
    tmpf <- tempfile()
    on.exit(unlink(tmpf))
    method <- "auto"
    ## FIXME: needs error checking and to look for VIEWS.gz first
    z <- download.file(url=paste(reposURL, "VIEWS", sep="/"), destfile=tmpf,
                       method=method, cacheOK=FALSE, quiet=TRUE, mode="wb")
    pmat <- read.dcf(file=tmpf)
    ns <- pmat[,"Package"]
    ## The DESCRIPTION fields we try to parse for tags
    DESC_FIELDS <- c("biocViews")
    bcvl <- vector(mode="list", length=nrow(pmat))
    names(bcvl) <- ns
    for (tagCol in DESC_FIELDS) {
        if (tagCol %in% colnames(pmat)) {
            tags <- pmat[, tagCol]
            names(tags) <- ns
            bcvl <- processTagsField(tags, bcvl)
        }
    }
    ## In case none of the fields were available, make sure everyone
    ## gets a NoViewsProvided tag.
    bcvl <- lapply(bcvl, function(x) {
        if (is.null(x))
          "NoViewProvided"
        else
          x
    })
    bcvl <- normalizeTags(bcvl, vocab)
    if (!local)
      pkgList <- createPackageDetailList(pmat, reposURL)
    else
      pkgList <- createPackageDetailList(pmat)
    list(views=bcvl, pkgList=pkgList)
}


normalizeTags <- function(tagList, vocab) {
    ## try to match tags to the vocab ignoring case.
    ## If found, replace with case as found in vocab.
    knownTerms <- nodes(vocab)
    knownTermsLower <- tolower(knownTerms)
    tagList <- lapply(tagList, function(x) {
        idx <- match(tolower(x), knownTermsLower)
        unknown <- is.na(idx)
        if (any(unknown)) {
            warning("Dropping unknown biocViews terms:\n",
                    paste(x[unknown], collapse=", "), call.=FALSE)
            idx <- idx[!is.na(idx)]
        }
        knownTerms[unique(idx)] ## remove duplicates
    })
    tagList
}


processTagsField <- function(tags, tagList) {
    ## Given a named character vector of comma separated tags,
    ## parse the tags and append data to the given tagList.
    ## Names of tags and tagList must match.
    if (!all.equal(names(tags), names(tagList)))
      stop("Names of tags and tagList must match")
    tags[is.na(tags)] <- "NoViewProvided"
    tags <- gsub("\\\n","",tags)
    fieldSp <- strsplit(tags, ", *")
    names(fieldSp) <- names(tagList)
    for (n in names(tagList)) {
        tagList[[n]] <- c(tagList[[n]], fieldSp[[n]])
    }
    tagList
}
    


permulist <- function(allv, vocab, interp=TRUE) {
    lens <- sapply(allv, length)
    packnames <- names(allv)
    repp <- rep(packnames, lens)
    ans <- split(repp, unlist(allv))
    if (interp)
      ans <- pump(ans, vocab)
    return(ans)
}

