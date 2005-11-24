extractVignettes <- function(reposRoot, srcContrib, destDir="vignettes") {
    ## Extract pdf vignettes from source package tarballs
    ##
    ## reposRoot - Top level path for CRAN-style repos
    ## srcContrib - Location of source packages
    ## destDir - where to extract.
    ##
    ## Notes:
    ## Under destDir, for tarball foo_1.2.3.tar.gz, you will
    ## get destDir/foo/inst/doc/*.pdf
    ##

    extractVignettesFromTarball <- function(tarball, unpackDir=".") {
        ## helper function to unpack pdf files from the vig
        vigPat <- "'*/doc/*.pdf'"
        tarCmd <- paste("tar", "-C", unpackDir, "-xzf", tarball, vigPat)
        cat("Extracting vignettes from", tarball, "\n")
        ret <- system(tarCmd)
        if (ret != 0)
          warning("tar had non-zero exit status for vig extract of: ", tarball)
    }

    tarballs <- list.files(file.path(reposRoot, srcContrib),
                           pattern="\.tar\.gz$", full.names=TRUE)
    if (!file.exists(destDir))
      dir.create(destDir, recursive=TRUE)
    if (!file.info(destDir)$isdir)
      stop("destDir must specify a directory")
    junk <- lapply(tarballs, extractVignettesFromTarball, unpackDir=destDir)
}


getVignetteLinks <- function(pkgList, reposRootPath, vignette.dir) {
    vigList <- list()
    for (pkg in pkgList) {
        vigSubDir <- "inst/doc"
        vigDir <- file.path(reposRootPath, vignette.dir, pkg, vigSubDir)
        if (file.exists(vigDir)) {
            vigs <- list.files(vigDir, pattern=".*\.pdf$")
            vigs <- paste(vignette.dir, pkg, vigSubDir, vigs, sep="/",
                          collapse=", ")
        } else
          vigs <- NA
        vigList[[pkg]] <- vigs
    }
    ## use unlist, as.character does NA --> "NA" ?!
    unlist(vigList)
}


genReposControlFiles <- function(reposRoot, contribPaths)
{
    write_REPOSITORY(reposRoot, contribPaths)
    ## Write PACKAGES files for all contrib paths
    packagesPaths <- file.path(reposRoot, contribPaths)
    names(packagesPaths) <- names(contribPaths)
    for (type in names(packagesPaths)) {
        path <- packagesPaths[[type]]
        write_PACKAGES(path, type=type)
    }
    ## Write a VIEWS file at the top-level containing
    ## detailed package info
    write_VIEWS(reposRoot, type="source")
}


write_REPOSITORY <- function(reposRootPath, contribPaths) {
    contrib <- as.list(contribPaths)
    names(contrib) <- names(contribPaths)
    contrib[["provides"]] <- paste(names(contrib), collapse=", ")
    fn <- file.path(reposRootPath, "REPOSITORY")
    write.dcf(contrib, fn)
}

.write_repository_db <- function(db, dir, fname) {
    if (length(db)) {
##         fields <- colnames(db[[1]])
##         db <- matrix(unlist(db), ncol = length(fields), byrow = TRUE)
##         colnames(db) <- fields
        noPack <- is.na(db[, "Package"])
        db[noPack, "Package"] <- db[noPack, "Bundle"]
        gzname <- paste(fname, "gz", sep=".")
        out <- file(file.path(dir, fname), "wt")
        ##FIXME: gzfile writing segfaults for me
        ##outgz <- gzfile(file.path(dir, gzname), "wt")
        for (i in seq(length = nrow(db))) {
            dbi <- db[i, !(is.na(db[i, ]) | (db[i, ] == 
                                                   "")), drop = FALSE]
            write.dcf(dbi, file = out)
            ##FIXME: writing to the gz file segfaults for me
            ##write.dcf(dbi, file = outgz)
        }
        close(out)
        ##FIXME: writing to the gz file segfaults
        ##close(outgz)
        invisible(nrow(db))
    }
    else invisible(0)
}


write_VIEWS <- function(reposRootPath, fields = NULL,
                        type = c("source", "mac.binary", "win.binary"),
                        verbose = FALSE, vignette.dir="vignettes") {
    ## Copied from tools::write_PACKAGES
    if (is.null(fields))
      fields <- c("Title", "Description", "biocViews",
                  "Author", "Maintainer", "URL", "License",
                  "SystemRequirements")
    if (missing(type))
      type <- "source"
    type <- match.arg(type)

    ## Read REPOSITORY file for contrib path info
    reposInfo <- read.dcf(file.path(reposRootPath, "REPOSITORY"))
    provided <- strsplit(reposInfo[, "provides"], ", *")[[1]]

    ## Use code from tools to build a matrix of package info
    ## by parsing the .tar.gz files
    pkg.dir <- file.path(reposRootPath, reposInfo[, "source"])
    db <- tools:::.build_repository_package_db(pkg.dir, fields, type, verbose)
    dbMat <- do.call("rbind", db)

    ## Integrate version and archive file path info for the different contrib
    ## paths in this repos.  We duplicate the source path info here, but that
    ## makes things easier to handle later on as there is no special case.
    fldNames <- c(colnames(dbMat), paste(provided, "ver", sep="."))
    dbMat <- cbind(dbMat, NA, NA)
    colnames(dbMat) <- fldNames
    for (ctype in provided) {
        cPath <- reposInfo[, ctype]
        buildPkgPath <- function(pkgs, vers) {
            ext <- switch(ctype, source=".tar.gz", win.binary=".zip",
                          mac.binary=".tgz", stop("unknown type"))
            paste(cPath, "/", pkgs, "_", vers, ext, sep="")
        }
        cDat <- read.dcf(file.path(reposRootPath, cPath, "PACKAGES"))
        ## FIXME: part of the lie we tell, there may be binary pkgs
        ## for which we do not have a source pkgs.  For now, ignore.
        cDatGood <- cDat[, "Package"] %in% dbMat[, "Package"]
        dbMatIdx <- match(cDat[cDatGood, "Package"], dbMat[, "Package"])
        dbMatIdx <- dbMatIdx[!is.na(dbMatIdx)]
        col <- paste(ctype, "ver", sep=".")
        dbMat[dbMatIdx, col] <- buildPkgPath(cDat[cDatGood, "Package"],
                                             cDat[cDatGood, "Version"])
    }
    ## Add vignette path info
    vigs <- getVignetteLinks(dbMat[, "Package"], reposRootPath, vignette.dir)
    dbMat <- cbind(dbMat, vigs)
    colnames(dbMat) <- c(fldNames, "vignettes")

    .write_repository_db(dbMat, reposRootPath, "VIEWS")
}


writeRepositoryHtml <- function(reposRoot, title, reposUrl="..") {
    ## Writes package description html under reposRoot/html and an index.html
    ## file under reposRoot.
    ##
    ## Links created in the package description html will use reposUrl as
    ## prefix.
    pkgList <- loadPackageDetails(reposRoot, reposUrl)
    writePackageDetailHtml(pkgList, file.path(reposRoot, "html"))
    writeRepositoryIndexHtml(pkgList, reposRoot, title)
}


writePackageDetailHtml <- function(pkgList, htmlDir="html") {
    if (!file.exists(htmlDir))
      dir.create(htmlDir)
    for (pkg in pkgList) {
        f <- file.path(htmlDir, htmlFilename(pkg))
        cat("writing html for", pkg@Package, "\n")
        writeHtmlDoc(htmlDoc(pkg), f)
    }
}


writeRepositoryIndexHtml <- function(pkgList, reposRoot, title, htmlDir="html")
{
    repos <- new("RepositoryDetail", Title=title, reposRoot=reposRoot,
                 htmlDir=htmlDir, packageList=pkgList)
    f <- file.path(reposRoot, htmlFilename(repos))
    writeHtmlDoc(htmlDoc(repos), f)
}
