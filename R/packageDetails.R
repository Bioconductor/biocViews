loadPackageDetails <- function(reposRoot, contribPaths)
{
    ## Return a list of PackageDetail objects representing
    ## the packages contained in the repository located
    ## on the local filesystem at reposRoot and including
    ## the paths under contribPaths.
    ##
    ## reposRoot - Path to local filesystem CRAN-style repository
    ##
    ## contribPaths - a named character vector of contrib paths where
    ##                a valid PACKAGES file along with appropriate
    ##                pakcage archives will be found.
    ##
    viewsFile <- file.path(reposRoot, "VIEWS")
    pkgMat <- read.dcf(viewsFile)
    pkgList <- apply(pkgMat, 1, pkgRowToPackageDetail)
    names(pkgList) <- pkgMat[, "Package"]
    pkgList <- setDependsOnMeSuggestsMe(pkgList)
    pkgList <- setArchiveLinks(pkgList, reposRoot, contribPaths)
    pkgList <- setFunctionIndex(pkgList, reposRoot, contribPaths["source"])
    pkgList <- setVignetteLinks(pkgList, reposRoot, "vignettes")
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


setFunctionIndex <- function(pkgList, reposRoot, srcPath) {
    ##TODO: extract function index from packages in some fashion
    pkgList
}


setArchiveLinks <- function(pkgList, reposRoot, contribPaths) {
    ## Return a copy of pkgList, a list of PackageDetail objects
    ## with the archiveLinks set according to the availability
    ## and location of packages in the specified contribPaths.
    ##
    ## contribPaths should be a named character vector where the
    ## names correspond to type: source, win.binary, mac.binary
    ##
    packagesFiles <- file.path(reposRoot, contribPaths, "PACKAGES")
    reposData <- lapply(packagesFiles, read.dcf)
    names(reposData) <- names(contribPaths)
    reposInfo <- lapply(names(reposData),
                        function(type) {
                            dat <- reposData[[type]]
                            ## a pseudo object for repository
                            ## info
                            list(data=dat, type=type,
                                 path=contribPaths[type])
                        })

    getLink <- function(repInfo, pkg) {
        x <- repInfo$data
        idx <- which(x[, "Package"] == pkg@Package)
        f <- paste(pkg@Package, x[idx, "Version"], sep="_")
        ext <- switch(repInfo$type,
                      source=".tar.gz",
                      win.binary=".zip",
                      mac.binary=".tgz",
                      ".UNKNOWN")
        paste(repInfo$path, "/", f, ext, sep="")
    }
        
    for (pkgName in names(pkgList)) {
        pkg <- pkgList[[pkgName]]

        inRepos <- sapply(reposInfo,
                          function(repInfo) {
                              x <- repInfo$data
                              pkg@Package %in% x[, "Package"]
                      })
        if (any(inRepos)) {
            links <- sapply(reposInfo[inRepos], getLink, pkg)
            names(links) <- sapply(reposInfo[inRepos], function(x) x$type)
        } else
          links <- ""
        pkg@downloadLinks <- links
        pkg@reposRoot <- reposRoot
        pkgList[[pkgName]] <- pkg
    }
    pkgList
}


setVignetteLinks <- function(pkgList, reposRoot, vignette.dir) {
    for (pkg in pkgList) {
        vigSubDir <- "inst/doc"
        vigDir <- file.path(reposRoot, vignette.dir, pkg@Package,
                            vigSubDir)
        if (file.exists(vigDir)) {
            vigs <- list.files(vigDir, pattern=".*\.pdf$")
            vigs <- paste("..", vignette.dir, pkg@Package, vigSubDir,
                          vigs, sep="/")
        } else
          vigs <- character(0)
        pkg@vignetteLinks <- vigs
    }
    pkgList
}


pkgRowToPackageDetail <- function(row) {
    ## Given a row from a package description matrix as returned
    ## by available.packages, or calling read.dcf on a PACKAGES
    ## formatted file, return a PackageDetail instance.

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
        if (is.null(val)) val <- ""
        val
    }
    
    pkg@Depends <- cleanField(pkg@Depends)
    pkg@Suggests <- cleanField(pkg@Suggests)
    pkg@Imports <- cleanField(pkg@Imports)
    pkg@biocViews <- cleanField(pkg@biocViews)
    
    pkg@Maintainer <- mangleEmail(pkg@Maintainer)
    pkg@Author <- mangleEmail(pkg@Author)
    
    return(pkg)
}


mangleEmail <- function(line) {
    ## 
    ## @ -> XY for letters sampled
    ## randomly from LETTERS.  And same with "."
    ## Email address must be in enclosed in <s@f.com>.
    emailStarts <- gregexpr("<", line, fixed=TRUE)[[1]]
    emailEnds <- gregexpr(">", line, fixed=TRUE)[[1]]

    emails <- sapply(seq(length=length(emailStarts)),
                     function(x)
                         substr(line, emailStarts[x], emailEnds[x]))
    emails <- sapply(emails, function(line) {
        wrapRand <- function(text, n=2) {
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
