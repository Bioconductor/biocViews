## FIXME this is really dumb as does not
## use the information in VIEWS (VignetteBuilder field)
myStangle <- function(file)
{
    tryCatch(Stangle(file), error=function(e){
        if (file.exists(file) && grepl("\\.Rnw$", file, ignore.case=TRUE))
        {
          rfile <- sub("\\.Rnw$", ".R", file)
          if (file.exists(rfile))
            unlink(rfile)
          purl(file)
        }
      })
}

readPackageInfo <- function(file, fields = NULL, all = FALSE) {
    info <- read.dcf(file = file, fields = fields, all = all)
    if ("vignettes" %in% colnames(info)) {
        info <-
          cbind(info,
                "vignetteScripts" =
                unlist(lapply(strsplit(info[,"vignettes"], ",\n"),
                              function(vigs) {
                              paste(lapply(sub("pdf$", "Rnw", vigs),
                                           function(v) {
                                           if (file.exists(v) && (!file.info(v)$isdir)) {
                                               myStangle(v)
                                               rfile <- sub("Rnw$", "R", basename(v))
                                               file.copy(rfile, dirname(v),
                                                         overwrite = TRUE)
                                               file.remove(rfile)
                                               sub("Rnw$", "R", v)
                                           } else {
                                               v <- sub("Rnw$", "rnw", v)
                                               if (file.exists(v) && (!file.info(v)$isdir)) {
                                                   myStangle(v)
                                                   rfile <- sub("rnw$", "R", basename(v))
                                                   file.copy(rfile, dirname(v),
                                                             overwrite = TRUE)
                                                   file.remove(rfile)
                                                   sub("rnw$", "R", v)
                                               } else
                                                   ""
                                           }
                                           }), collapse = ",\n")
                              })),
                "vignetteTitles" =
                unlist(lapply(strsplit(info[,"vignettes"], ",\n"),
                              function(vigs) {
                              paste(lapply(sub("pdf$", "Rnw", vigs),
                                           function(v) {
                                           if (file.exists(v))
                                               tools:::vignetteInfo(v)[["title"]]
                                           else {
                                               v <- sub("Rnw$", "rnw", v)
                                               if (file.exists(v))
                                                   tools:::vignetteInfo(v)[["title"]]
                                               else
                                                   sub("rnw$", "pdf", basename(v))
                                           }
                                           }), collapse = ",\n")
                              })))
    }
    ## TODO: make the 'manuals' path configurable
    if ("Package" %in% colnames(info)) {
        info <-
          cbind(info,
                "manuals" =
                unlist(lapply(info[,"Package"],
                              function(pkg) {
                                  man <-
                                    file.path("manuals", pkg, "man",
                                              paste(pkg, ".pdf", sep = ""))
                                  if (file.exists(man))
                                      man
                                  else
                                      NA_character_
                              })))
    }
    info
}


writeBiocViews <- function(bvList, dir, backgroundColor="transparent") {
    ## bvList is a list of BiocViews objects
    ## dir is the output directory in which to write the views.
    for (bv in bvList) {
        fn <- file.path(dir, htmlFilename(bv))
        html <- htmlDoc(bv)
        writeHtmlDoc(html, fn)
    }
    ## copy the css
    cssName <- "repository-detail.css"
    cssPath <- system.file(file.path("css", paste(cssName, ".in", sep="")),
                           package="biocViews")
    res <- try(copySubstitute(cssPath, file.path(dir, cssName),
                        symbolValues=list("BACKGROUND_COLOR"=backgroundColor)),
               silent=TRUE)
    res
}


writeTopLevelView <- function(dir, vocab) {
    top <- getRootNode(vocab)
    mainSubViews <- edges(vocab)[[top]]
    topView <- new("BiocView", name=top, subViews=mainSubViews)
    fn <- file.path(dir, htmlFilename(topView))
    writeHtmlDoc(htmlDoc(topView), fn)
}


getBiocViews <- function(reposUrl, vocab, defaultView, local=FALSE, htmlDir="") {
    viewList <- getPacksAndViews(reposUrl, vocab, defaultView, local)
    viewRoster <- permulist(viewList$views, vocab)
    if (local) {
      reposUrl <- character(0)
      if (!htmlDir == "") {
        htmlDir <- htmlDir
      }
    }
    biocViews <- loadViews(vocab, viewRoster, viewList$pkgList, reposUrl, htmlDir)
    biocViews
}


getBiocSubViews <- function(reposUrl, vocab, topTerm, local=FALSE, htmlDir="") {
    root <- getRootNode(vocab)
    terms <- getSubTerms(vocab, topTerm)
    subVocab <- subGraph(c(root, terms), vocab)
    bvl <- getBiocViews(reposUrl, subVocab, topTerm, local, htmlDir)
    bvl[terms] ## exclude root
}


loadViews <- function(viewGraph, viewRoster, pkgList, reposUrl, htmlDir="html") {
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
            packageList=pkgsInView, htmlDir=htmlDir, reposRoot=reposUrl)
    }
    biocViews <- lapply(views, viewFactory)
    names(biocViews) <- views
    biocViews
}

getPacksAndViews <- function(reposURL, vocab, defaultView, local=FALSE)
{
    tmpf <- tempfile()
    on.exit(unlink(tmpf))
    method <- "auto"
    ## FIXME: needs error checking and to look for VIEWS.gz first
    z <- download.file(url=paste(reposURL, "VIEWS", sep="/"), destfile=tmpf,
                       method=method, cacheOK=FALSE, quiet=TRUE, mode="wb")
    pmat <- readPackageInfo(file=tmpf)
    bcvl <- pkgList <- vector(mode="list", length=nrow(pmat))
    if (nrow(pmat) == 0L)
        return(list(views=bcvl, pkgList=pkgList))
    ns <- pmat[,"Package"]
    ## The DESCRIPTION fields we try to parse for tags
    DESC_FIELDS <- c("biocViews")
    names(bcvl) <- ns
    for (tagCol in DESC_FIELDS) {
        if (tagCol %in% colnames(pmat)) {
            tags <- pmat[, tagCol]
            names(tags) <- ns
            bcvl <- processTagsField(tags, bcvl, defaultView)
        }
    }
    ## In case none of the fields were available, make sure everyone
    ## gets a NoViewsProvided tag.
    bcvl <- lapply(bcvl, function(x) {
        if (is.null(x))
          defaultView
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


processTagsField <- function(tags, tagList, defaultTag) {
    ## Given a named character vector of comma separated tags,
    ## parse the tags and append data to the given tagList.
    ## Names of tags and tagList must match.
    if (!all.equal(names(tags), names(tagList)))
      stop("Names of tags and tagList must match")
    tags[is.na(tags)] <- defaultTag
    tags <- gsub("\\\n","",tags)
    fieldSp <- strsplit(tags, ", *")
    names(fieldSp) <- names(tagList)
    for (n in names(tagList)) {
        tagList[[n]] <- c(tagList[[n]], fieldSp[[n]])
    }
    tagList
}
    

getSubTerms <- function(dag, term) {
    c(term, names(acc(dag, term)[[1]]))
}


permulist <- function(allv, vocab, interp=TRUE) {
    if (length(allv) == 0L)
        return(list())
    lens <- sapply(allv, length)
    packnames <- names(allv)
    repp <- rep(packnames, lens)
    ans <- split(repp, unlist(allv))
    if (interp)
      ans <- pump(ans, vocab)
    return(ans)
}
