loadPackageDetails <- function(reposRoot, reposUrl="..")
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
    pkgList <- apply(pkgMat, 1, viewRowToPackageDetail)
    names(pkgList) <- pkgMat[, "Package"]
    pkgList <- setDependsOnMeSuggestsMe(pkgList)
    pkgList <- lapply(pkgList, function(p) {
        p@reposRoot <- reposUrl
        p
    })
    return(pkgList)
}


setDependsOnMeSuggestsMe <- function(pkgDetailsList) {
    ## Add list of packages that depend on and suggest each package
    ## listed in pkgDetailsList, a list of PackageDetail objects.

    pkgNames <- names(pkgDetailsList)
    depCols <- lapply(pkgDetailsList,
                         function(x) pkgNames %in% x@Depends)
    depMat <- do.call("cbind", depCols)
    colnames(depMat) <- rownames(depMat) <- pkgNames

    sugCols <- lapply(pkgDetailsList,
                         function(x) pkgNames %in% x@Suggests)
    sugMat <- do.call("cbind", sugCols)
    colnames(sugMat) <- rownames(sugMat) <- pkgNames
    
    setDepsSugs <- function(pkg) {
        deps <- pkgNames[which(depMat[pkg@Package, ])]
        sugs <- pkgNames[which(sugMat[pkg@Package, ])]
        pkg@dependsOnMe <- paste(deps, collapse=", ")
        pkg@suggestsMe <- paste(sugs, collapse=", ")
        return(pkg)
    }
    return(lapply(pkgDetailsList, setDepsSugs))
}


viewRowToPackageDetail <- function(row) {
    ## Given a row from a VIEWS package description matrix as returned by
    ## calling read.dcf on a VIEWS file, return a PackageDetail instance.

    pkg <- new("PackageDetail")
    ## assume we have names on the row
    flds <- names(row)
    for (fld in flds) {
        val <- row[[fld]]
        if (is.na(val)) val <- ""
        slot(pkg, fld) <- val
    }
    ## Fix vector fields
    ## FIXME: we are using a private func from tools.  Also,
    ## this func gives more structure (version info) which for now we
    ## ignore.
    cleanField <- function(val) {
        val <- names(tools:::.split_dependencies(val))
        if (is.null(val)) val <- character(0)
        val
    }

    cleanVigs <- function(vigs) {
        vigs <- gsub("\n", "", vigs)
        strsplit(vigs, ", *")[[1]]
    }
    
    pkg@Depends <- cleanField(pkg@Depends)
    pkg@Suggests <- cleanField(pkg@Suggests)
    pkg@Imports <- cleanField(pkg@Imports)
    pkg@biocViews <- cleanField(pkg@biocViews)
    pkg@vignettes <- cleanVigs(pkg@vignettes)
    
    pkg@Maintainer <- mangleEmail(pkg@Maintainer)
    pkg@Author <- mangleEmail(pkg@Author)
    
    return(pkg)
}


mangleEmail <- function(line) {
    ##  Rafael A. Irizarry <rafa@jhu.edu>
    ##              |
    ##              |
    ##              *
    ##  Rafael A. Irizarry <rafa vATx jhu pDOTl edu>
    ##
    emailStarts <- gregexpr("<", line, fixed=TRUE)[[1]]
    emailEnds <- gregexpr(">", line, fixed=TRUE)[[1]]

    emails <- sapply(seq(length=length(emailStarts)),
                     function(x)
                         substr(line, emailStarts[x], emailEnds[x]))
    emails <- sapply(emails, function(line) {
        wrapRand <- function(text, n=1) {
            st <- paste(sample(letters, n), collapse="")
            en <- paste(sample(letters, n), collapse="")
            paste(" ", st, text, en, " ", sep="")
        }
        AT <- wrapRand("AT")
        DOT <- wrapRand("DOT")
        line <- gsub("@", AT, line, fixed=TRUE)
        line <- gsub("\.", DOT, line, fixed=TRUE)
        line
    })
    other <- strsplit(line, "<[^>]+@[^>]+>")[[1]]
    paste(other, emails, collapse="")
}
