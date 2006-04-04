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
    viewList <- getPacksAndViews(reposUrl, local)
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

getPacksAndViews <- function(reposURL, local=FALSE) {
    tmpf <- tempfile()
    on.exit(unlink(tmpf))
    method <- "auto"
    ## FIXME: needs error checking and to look for VIEWS.gz first
    z <- download.file(url=paste(reposURL, "VIEWS", sep="/"), destfile=tmpf,
                       method=method, cacheOK=FALSE, quiet=TRUE, mode="wb")
    pmat <- read.dcf(file=tmpf)
    ns <- pmat[,"Package"]
    if ("biocViews" %in% colnames(pmat))
      bcv <- pmat[,"biocViews"]
    else 
      bcv <- rep(NA, nrow(pmat))
    bcv[is.na(bcv)] <- "NoViewProvided"
    bcv <- gsub("\\\n","",bcv)
    bcvl <- strsplit(bcv, ", *")
    names(bcvl) <- ns
    ## FIXME: we should validate against the known vocabulary here
    ## patch up some usages that do not capitalize first letter
    bcvl <- lapply(bcvl, function(x) gsub("\\b(\\w)", "\\U\\1", x, perl=TRUE))
    names(bcvl) <- ns
    if (!local)
      pkgList <- createPackageDetailList(pmat, reposURL)
    else
      pkgList <- createPackageDetailList(pmat)
    list(views=bcvl, pkgList=pkgList)
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

