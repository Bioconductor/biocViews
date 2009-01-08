loadPackageDetails <- function(reposRoot, reposUrl="..", viewUrl="../..", reposFullUrl=reposUrl,
                               downloadStatsUrl="", devHistoryUrl="")
{
    ## Return a list of PackageDetail objects representing
    ## the packages contained in the repository located
    ## on the local filesystem at reposRoot.
    ##
    ## reposRoot - Path to local filesystem CRAN-style repository
    ##
    
    ## FIXME: should allow reading VIEWS from a URL also.
    viewsFile <- file.path(reposRoot, "VIEWS")
    pkgMat <- read.dcf(viewsFile)
    createPackageDetailList(pkgMat, reposUrl, viewUrl, reposFullUrl, downloadStatsUrl,
                            devHistoryUrl)
}


createPackageDetailList <- function(viewMat, reposUrl="..",
                                    viewUrl=character(0),
                                    reposFullUrl=reposUrl,
                                    downloadStatsUrl="",
                                    devHistoryUrl="")
{
    pkgList <- apply(viewMat, 1, viewRowToPackageDetail)
    names(pkgList) <- viewMat[, "Package"]
    pkgList <- setDependsOnMeImportsMeSuggestsMe(pkgList)
    pkgList <- lapply(pkgList, function(p) {
        p@devHistoryUrl <- devHistoryUrl
        p@downloadStatsUrl <- downloadStatsUrl
        p@reposFullUrl <- reposFullUrl
        p@reposRoot <- reposUrl
        p@viewRoot <- viewUrl
        p
    })
    return(pkgList)
}


setDependsOnMeImportsMeSuggestsMe <- function(pkgDetailsList) {
    ## Add list of packages that depend on and suggest each package
    ## listed in pkgDetailsList, a list of PackageDetail objects.

    pkgNames <- names(pkgDetailsList)

    depCols <- lapply(pkgDetailsList,
                      function(x) pkgNames %in% x@Depends)
    depMat <- do.call(cbind, depCols)
    colnames(depMat) <- rownames(depMat) <- pkgNames

    impCols <- lapply(pkgDetailsList,
                      function(x) pkgNames %in% x@Imports)
    impMat <- do.call(cbind, impCols)
    colnames(impMat) <- rownames(impMat) <- pkgNames

    sugCols <- lapply(pkgDetailsList,
                      function(x) pkgNames %in% x@Suggests)
    sugMat <- do.call(cbind, sugCols)
    colnames(sugMat) <- rownames(sugMat) <- pkgNames

    setDepsImpsSugs <- function(pkg) {
        deps <- pkgNames[which(depMat[pkg@Package, ])]
        imps <- pkgNames[which(impMat[pkg@Package, ])]
        sugs <- pkgNames[which(sugMat[pkg@Package, ])]
        pkg@dependsOnMe <- deps
        pkg@importsMe <- imps
        pkg@suggestsMe <- sugs
        return(pkg)
    }
    return(lapply(pkgDetailsList, setDepsImpsSugs))
}


viewRowToPackageDetail <- function(row) {
    ## Given a row from a VIEWS package description matrix as returned by
    ## calling read.dcf on a VIEWS file, return a PackageDetail instance.

    pkg <- new("PackageDetail")
    ## assume we have names on the row
    flds <- names(row)
    ourSlots <- slotNames(getClass("PackageDetail"))
    for (fld in flds) {
        if (! fld %in% ourSlots)
          next
        val <- row[[fld]]
        ## FIXME: are we sure we want to get rid of the NA's here?
        if (is.na(val)) val <- ""
        slot(pkg, fld) <- val
    }
    ## Fix vector fields
    ## FIXME: we are using a private func from tools.  Also,
    ## this func gives more structure (version info) which for now we
    ## ignore.
    cleanPkgField <- function(val) {
        val <- names(tools:::.split_dependencies(val))
        if (is.null(val)) val <- character(0)
        val
    }

    cleanField <- function (x) {
        x <- unlist(strsplit(x, ","))
        if (!length(x)) 
          return(character(0))
        x <- unique(sub("^[[:space:]]*(.*)[[:space:]]*$", "\\1", x))
        x
    }

    cleanVigs <- function(vigs) {
        if (length(vigs) > 0 && !is.na(vigs)) {
            vigs <- gsub("\n", "", vigs)
            ans <- strsplit(vigs, ", *")[[1]]
        } else {
            ans <- character(0)
        }
        return(ans)
    }
    
    pkg@Depends <- cleanPkgField(pkg@Depends)
    pkg@Suggests <- cleanPkgField(pkg@Suggests)
    pkg@Imports <- cleanPkgField(pkg@Imports)
    pkg@biocViews <- cleanField(pkg@biocViews)
    pkg@vignettes <- cleanVigs(pkg@vignettes)
    
    return(pkg)
}


removeEmail <- function(line) {
    line <- gsub("<[a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+>", "", line)
    line <- gsub("[a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+", "", line)
    line <- sub(" +$", "", line)
    line
}


mangleEmail <- function(line) {
    chrA <- c("&Agrave;", "&Aacute;", "&Acirc;", "&Atilde;", "&Auml;",
              "&Aring;", "&AElig;")
    
    chrO <- c("&Ograve;", "&Oacute;", "&Ocirc;", "&Otilde;", "&Ouml;")

    makeAT <- function() {
        i <- sample(seq(length=length(chrA), 1))
        paste(" ", chrA[i], "T", " ", sep="")
    }

    makeDOT <- function() {
        i <- sample(seq(length=length(chrO), 1))
        paste(" ", "D", chrO[i], "T", " ", sep="")
    }

    emailStarts <- gregexpr("<", line, fixed=TRUE)[[1]]
    emailEnds <- gregexpr(">", line, fixed=TRUE)[[1]]

    emails <- sapply(seq(length=length(emailStarts)),
                     function(x)
                         substr(line, emailStarts[x], emailEnds[x]))
    emails <- sapply(emails, function(line) {
        AT <- makeAT()
        DOT <- makeDOT()
        line <- gsub("@", AT, line, fixed=TRUE)
        line <- gsub("\\.", DOT, line, fixed=TRUE)
        line
    })
    other <- strsplit(line, "<[^>]+@[^>]+>")[[1]]
    paste(other, emails, collapse="")
}
