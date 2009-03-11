genReposControlFiles <- function(reposRoot, contribPaths)
{
    ## Generate all control files for BioC hosted R
    ## package repositorys
    
    write_REPOSITORY(reposRoot, contribPaths)
    ## Write PACKAGES files for all contrib paths
    packagesPaths <- file.path(reposRoot, contribPaths)
    names(packagesPaths) <- names(contribPaths)
    for (type in names(packagesPaths)) {
        path <- packagesPaths[[type]]
        if(substr(type, 1, 10) == "mac.binary") {
            type <- "mac.binary"
        }
        write_PACKAGES(path, type=type)
    }
    ## Write a VIEWS file at the top-level containing
    ## detailed package info
    write_VIEWS(reposRoot, type="source")

    ## Write a SYMBOLS file at the top-level containing the
    ## exported symbols for all packages that have name
    ## spaces.  This is used to build a searchable index.
    write_SYMBOLS(reposRoot, verbose=TRUE)
}


extractManuals <- function(reposRoot, srcContrib, destDir) {
    ## Extract Rd man pages from source package tarballs and
    ## convert to pdf documents
    ##
    ## reposRoot - Top level path for CRAN-style repos
    ## srcContrib - Location of source packages
    ## destDir - where to extract.
    ##
    ## Notes:
    ## Under destDir, for tarball foo_1.2.3.tar.gz, you will
    ## get destDir/foo/man/*.pdf
    ##

    if (missing(destDir))
        destDir <- file.path(reposRoot, "manuals")

    cleanUnpackDir <- function(tarball, unpackDir) {
        ## Delete manuals from a previous extraction
        pkg <- strsplit(basename(tarball), "_", fixed=TRUE)[[1]][1]
        pkgDir <- file.path(unpackDir, pkg, "man")
        rmRegex <- ".*\\.(pdf|Rd|rd)$"
        if (!file.exists(pkgDir))
            return(FALSE)
        oldFiles <- list.files(pkgDir, pattern=rmRegex, full.names=TRUE)
        if (length(oldFiles) > 0)
            try(file.remove(oldFiles), silent=TRUE)
    }
    
    buildManualsFromTarball <- function(tarball, unpackDir=".") {
        ## helper function to unpack pdf & Rd files from the vig
        manPat <- "--wildcards '*/man/*.[Rr]d'"
        tarCmd <- paste("tar", "-C", unpackDir, "-xzf", tarball, manPat)
        cleanUnpackDir(tarball, unpackDir)
        cat("Extracting man pages from", tarball, "\n")
        ret <- system(tarCmd)
        if (ret != 0)
            warning("tar had non-zero exit status for man pages extract of: ", tarball)
        else {
            pkg <- strsplit(basename(tarball), "_", fixed=TRUE)[[1]][1]
            pkgDir <- file.path(unpackDir, pkg, "man")
            RCmd <- file.path(Sys.getenv("R_HOME"), "bin", "R")
            Rd2pdfCmd <-
              paste(RCmd, " CMD Rd2dvi --no-preview --pdf --output=", pkgDir, "/",
                    pkg, ".pdf --title=", pkg, " ", pkgDir, "/*.[Rr]d", sep = "")
            cat("Building pdf reference manual for", pkg, "\n")
            ret <- system(Rd2pdfCmd)
            if (ret != 0)
                warning("R had non-zero exit status for building ref man for: ", pkg)
        }
    }
    
    tarballs <- list.files(file.path(reposRoot, srcContrib),
            pattern="\\.tar\\.gz$", full.names=TRUE)
    if (!file.exists(destDir))
        dir.create(destDir, recursive=TRUE)
    if (!file.info(destDir)$isdir)
        stop("destDir must specify a directory")
    lapply(tarballs, buildManualsFromTarball, unpackDir=destDir)
}


getRefmanLinks <- function(pkgList, reposRootPath, refman.dir) {
    unlist(lapply(pkgList, function(pkg) {
        refmanSubDir <- "man"
        refmanDir <- file.path(reposRootPath, refman.dir, pkg, refmanSubDir)
        if (file.exists(refmanDir)) {
            refmans <- list.files(refmanDir, pattern=".*\\.pdf$")
            refmans <- paste(refman.dir, pkg, refmanSubDir, refmans, sep="/",
                    collapse=", ")
        } else
            refmans <- NA_character_
        refmans
    }))
}


extractVignettes <- function(reposRoot, srcContrib, destDir) {
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

    if (missing(destDir))
      destDir <- file.path(reposRoot, "vignettes")

    cleanUnpackDir <- function(tarball, unpackDir) {
        ## Delete vignettes from a previous extraction
        pkg <- strsplit(basename(tarball), "_", fixed=TRUE)[[1]][1]
        pkgDir <- file.path(unpackDir, pkg, "inst", "doc")
        rmRegex <- ".*\\.(pdf|Rnw|rnw)$"
        if (!file.exists(pkgDir))
          return(FALSE)
        oldFiles <- list.files(pkgDir, pattern=rmRegex, full.names=TRUE)
        if (length(oldFiles) > 0)
          try(file.remove(oldFiles), silent=TRUE)
    }
    
    extractVignettesFromTarball <- function(tarball, unpackDir=".") {
        ## helper function to unpack pdf & Rnw files from the vig
        vigPat <- "--wildcards '*/doc/*.[pRr][dn][fw]'"
        tarCmd <- paste("tar", "-C", unpackDir, "-xzf", tarball, vigPat)
        cleanUnpackDir(tarball, unpackDir)
        cat("Extracting vignettes from", tarball, "\n")
        ret <- system(tarCmd)
        if (ret != 0)
          warning("tar had non-zero exit status for vig extract of: ", tarball)
    }

    tarballs <- list.files(file.path(reposRoot, srcContrib),
                           pattern="\\.tar\\.gz$", full.names=TRUE)
    if (!file.exists(destDir))
      dir.create(destDir, recursive=TRUE)
    if (!file.info(destDir)$isdir)
      stop("destDir must specify a directory")
    lapply(tarballs, extractVignettesFromTarball, unpackDir=destDir)
}


getVignetteLinks <- function(pkgList, reposRootPath, vignette.dir) {
    unlist(lapply(pkgList, function(pkg) {
        vigSubDir <- "inst/doc"
        vigDir <- file.path(reposRootPath, vignette.dir, pkg, vigSubDir)
        if (file.exists(vigDir)) {
            vigs <- list.files(vigDir, pattern=".*\\.pdf$")
            vigs <- paste(vignette.dir, pkg, vigSubDir, vigs, sep="/",
                          collapse=", ")
        } else
          vigs <- NA_character_
        vigs
    }))
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
            dbi <- db[i, !(is.na(db[i, ]) | (db[i, ] == "")), drop = FALSE]
            write.dcf(dbi, file = out)
            ##FIXME: writing to the gz file segfaults for me
            ##write.dcf(dbi, file = outgz)
            cat("\n", file=out)
        }
        close(out)
        ##FIXME: writing to the gz file segfaults
        ##close(outgz)
        invisible(nrow(db))
    }
    else invisible(0)
}


write_VIEWS <- function(reposRootPath, fields = NULL,
                        type = c("source", "mac.binary", "mac.binary.universal",
                                 "mac.binary.leopard", "win.binary"),
                        verbose = FALSE, vignette.dir="vignettes") {
    ## Copied from tools::write_PACKAGES
    if (is.null(fields))
      fields <- c("Title", "Description", "biocViews",
                  "Author", "Maintainer", "URL", "License",
                  "SystemRequirements", "organism", "manufacturer")
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
    dbMat <- do.call(rbind, db)

    ## Integrate version and archive file path info for the different contrib
    ## paths in this repos.  We duplicate the source path info here, but that
    ## makes things easier to handle later on as there is no special case.
    fldNames <- c(colnames(dbMat), paste(provided, "ver", sep="."))
    dbMat <- cbind(dbMat, matrix(NA, nrow=nrow(dbMat), ncol=length(provided)))
    colnames(dbMat) <- fldNames
    for (ctype in provided) {
        cPath <- reposInfo[, ctype]
        buildPkgPath <- function(pkgs, vers) {
            ext <- switch(ctype, source=".tar.gz", win.binary=".zip",
                          mac.binary=, mac.binary.universal=,
                          mac.binary.leopard=".tgz",
                          stop("unknown type"))
            paste(cPath, "/", pkgs, "_", vers, ext, sep="")
        }
        packagesFile <- file.path(reposRootPath, cPath, "PACKAGES")
        if (!file.exists(packagesFile)) {
            warning("No PACKAGES file found at ",
                    file.path(reposRootPath, cPath),
                    "\nSkipping this contrib path.")
            next
        }
        readOk <- tryCatch({
            cDat <- read.dcf(packagesFile)
            TRUE
        }, error=function(e) FALSE)
        if (!readOk)
          next
        if (!length(cDat)) {
            warning("Empty PACKAGES file found at ",
                    file.path(reposRootPath, cPath),
                    "\nSkipping this contrib path.")
            next
        }
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


writeRepositoryHtml <- function(reposRoot, title, reposUrl="..",
                                viewUrl="../..", reposFullUrl=reposUrl,
                                downloadStatsUrl="", devHistoryUrl="",
                                link.rel=TRUE, backgroundColor="transparent") {
    ## Writes package description html under reposRoot/html and an index.html
    ## file under reposRoot.
    ##
    ## Links created in the package description html will use reposUrl as
    ## prefix.
    pkgList <- loadPackageDetails(reposRoot, reposUrl, viewUrl, reposFullUrl,
                                  downloadStatsUrl, devHistoryUrl)
    writePackageDetailHtml(pkgList, file.path(reposRoot, "html"),
                           backgroundColor=backgroundColor)
    writeRepositoryIndexHtml(pkgList, reposRoot, title, link.rel=link.rel)

    ## copy the css stylesheet
    cssName <- "repository-detail.css"
    cssPath <- system.file(file.path("css", paste(cssName, ".in", sep="")),
                           package="biocViews")
    res <- try(copySubstitute(cssPath, file.path(reposRoot, cssName),
                        symbolValues=list("BACKGROUND_COLOR"=backgroundColor)),
               silent=TRUE)
    res
}


writePackageDetailHtml <- function(pkgList, htmlDir="html",
                                   backgroundColor="transparent") {
    if (!file.exists(htmlDir))
      dir.create(htmlDir)
    for (pkg in pkgList) {
        f <- file.path(htmlDir, htmlFilename(pkg))
        cat("writing html for", pkg@Package, "\n")
        writeHtmlDoc(htmlDoc(pkg), f)
    }
    ## copy the package detail css stylesheet
    cssName <- "package-detail.css"
    cssPath <- system.file(file.path("css", paste(cssName, ".in", sep="")),
                           package="biocViews")
    res <- try(copySubstitute(cssPath, file.path(htmlDir, cssName),
                        symbolValues=list("BACKGROUND_COLOR"=backgroundColor)),
               silent=TRUE)
    res
}


writeRepositoryIndexHtml <- function(pkgList, reposRoot, title, htmlDir="html",
                                     link.rel=TRUE)
{
    if (link.rel)
      linkRoot <- character(0)
    else
      linkRoot <- reposRoot
    repos <- new("RepositoryDetail", Title=title, reposRoot=linkRoot,
                 htmlDir=htmlDir, packageList=pkgList)
    f <- file.path(reposRoot, htmlFilename(repos))
    writeHtmlDoc(htmlDoc(repos), f)
}


write_SYMBOLS <- function(dir, verbose=FALSE, source.dirs=FALSE) {
    con <- file(file.path(dir, "SYMBOLS"), open="w")

    tdir <- tempfile("NAMESPACES")
    dir.create(tdir)
    on.exit(file.remove(tdir, recursive=TRUE))

    extractNAMESPACEFromTarball <- function(tarball, unpackDir=tdir) {
        ## helper function to unpack NAMESPACE file from the tarball
        pat <- "--wildcards '*/NAMESPACE'"
        tarCmd <- paste("tar", "-C", unpackDir, "-xzf", tarball, pat)
        ret <- system(tarCmd)
        if (ret != 0)
          warning("tar had non-zero exit status for NAMESPACE extract of: ",
                  tarball)
    }

    writeField <- function(field, v) {
        ## Helper function for writing DCF
        if (length(v)) {
            vals <- paste(v, collapse=", ")
            field <- paste(field, ":", sep="")
            writeLines(paste(field, vals), con=con)
        }
    }

    if (!source.dirs) {
        tarballs <- list.files(file.path(dir, "src/contrib"),
                               pattern="\\.tar\\.gz$", full.names=TRUE)
        for (t in tarballs) {
            extractNAMESPACEFromTarball(t)
        }
        dir <- tdir
    }
    pkgs <- list.files(dir)
    for (p in pkgs) {
        syms <- tryCatch(parseNamespaceFile(p, dir),
                         error=function(e) character(0))
        numSyms <- (length(syms$exports) + length(syms$exportMethods)
                    +length(syms$exportClasses))
        if (numSyms > 0) {
            writeField("Package", p)
            writeField("Exports", syms$exports)
            writeField("ExportMethods", syms$exportMethods)
            writeField("ExportClasses", syms$exportClasses)
            writeLines("", con=con)
        }
        if (verbose)
          cat(p, numSyms, "symbols\n")
    }
    close(con)
    NULL
}
