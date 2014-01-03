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
        if (type == "win64.binary") {
            type <- "win.binary"
        } else if (substr(type, 1, 10) == "mac.binary") {
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

    cleanUnpackDir <- function(tarball, unpackDir, rmRegex=".*\\.(pdf|Rd|rd)$") {
        ## Delete manuals from a previous extraction
        pkg <- strsplit(basename(tarball), "_", fixed=TRUE)[[1]][1]
        pkgDir <- file.path(unpackDir, pkg, "man")
        if (!file.exists(pkgDir))
            return(FALSE)
        oldFiles <- list.files(pkgDir, pattern=rmRegex, full.names=TRUE)
        if (length(oldFiles) > 0)
            try(file.remove(oldFiles), silent=TRUE)
    }
    
    buildManualsFromTarball <- function(tarball, unpackDir=".") {
        ## helper function to unpack pdf & Rd files from the vig
        if (grepl("data/annotation$", reposRoot))
        {
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
                  paste(RCmd, " CMD Rd2pdf --no-preview --output=", pkgDir, "/",
                        pkg, ".pdf --title=", pkg, " ", pkgDir, "/*.[Rr]d", sep = "")
                cat("Building pdf reference manual for", pkg, "\n")
                ret <- system(Rd2pdfCmd)
                cleanUnpackDir(tarball, unpackDir, rmRegex=".*\\.(Rd|rd)$")
                if (ret != 0)
                    warning("R had non-zero exit status for building ref man for: ", pkg)
            }

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

extractTopLevelFiles <- function(reposRoot, srcContrib, destDir, fileName) {

    cleanUnpackDir <- function(tarball, unpackDir) {
        ## Delete files from a previous extraction
        pkg <- strsplit(basename(tarball), "_", fixed=TRUE)[[1]][1]
        pkgDir <- file.path(unpackDir, pkg)
        if (!file.exists(pkgDir))
          return(FALSE)
        oldFiles <- list.files(pkgDir, pattern=fileName, full.names=TRUE)
        if (length(oldFiles) > 0)
          try(file.remove(oldFiles), silent=TRUE)
    }
    
    extractFileFromTarball <- function(tarball, unpackDir=".") {
        pkg <- strsplit(basename(tarball), "_", fixed=TRUE)[[1]][1]
        pat <- file.path(pkg, fileName)
        tarCmd <- paste("tar", "-C", unpackDir, "-xzf", tarball, pat)
        cleanUnpackDir(tarball, unpackDir)
        cat(paste("Attempting to extract", fileName, "from", tarball, "\n"))
        system(tarCmd, ignore.stdout=TRUE, ignore.stderr=TRUE)
    }
    
    tarballs <- list.files(file.path(reposRoot, srcContrib),
                           pattern="\\.tar\\.gz$", full.names=TRUE)
    if (!file.exists(destDir))
      dir.create(destDir, recursive=TRUE)
    if (!file.info(destDir)$isdir)
      stop("destDir must specify a directory")
    lapply(tarballs, extractFileFromTarball, unpackDir=destDir)
    invisible(NULL)
    
}

extractINSTALLfiles <- function(reposRoot, srcContrib, destDir) {
    extractTopLevelFiles(reposRoot, srcContrib, destDir, "INSTALL")
}



extractReadmes <- function(reposRoot, srcContrib, destDir) {
    ## Extract README files from source package tarballs
    ##
    ## reposRoot - Top level path for CRAN-style repos
    ## srcContrib - Location of source packages
    ## destDir - where to extract.
    ##
    ## Notes:
    ## Under destDir, for tarball foo_1.2.3.tar.gz, you will
    ## get destDir/foo/inst/doc/*.pdf
    ##
    extractTopLevelFiles(reposRoot, srcContrib, destDir, "README")
}



extractNEWS <- function(reposRoot, srcContrib, destDir) {

    if (missing(destDir))
      destDir <- file.path(reposRoot, "news")


    cleanUnpackDir <- function(tarball, unpackDir) {
        ## Delete NEWS from a previous extraction
        pkg <- strsplit(basename(tarball), "_", fixed=TRUE)[[1]][1]
        pkgDir <- file.path(unpackDir, pkg)
        if (!file.exists(pkgDir))
          return(FALSE)
        oldFiles <- list.files(pkgDir, pattern="NEWS", full.names=TRUE)
        if (length(oldFiles) > 0)
          try(file.remove(oldFiles), silent=TRUE)
    }
    
    extractNewsFromTarball <- function(tarball, unpackDir=".") {
        pkg <- strsplit(basename(tarball), "_", fixed=TRUE)[[1]][1]
        newsPat <- "*NEWS*"
        wildcards <- ifelse(Sys.info()["sysname"] == "Darwin", "",
          "--wildcards") 
        tarCmd <- paste("tar", wildcards, "-C", unpackDir, "-xzf",
          tarball, newsPat)
        cleanUnpackDir(tarball, unpackDir)
        cat("Attempting to extract NEWS from", tarball, "\n")
        system(tarCmd, ignore.stdout=TRUE, ignore.stderr=TRUE)
    }
    
    convertNEWSToText <- function(tarball, srcDir, destDir)
    {
        segs <- strsplit(tarball, "_", fixed=TRUE)
        pkgName <- segs[[1]][1]
        segs <- strsplit(pkgName, .Platform$file.sep, fixed=TRUE)
        pkgName <- segs[[1]][length(segs[[1]])]
        file.path()
        srcDir <- file.path(srcDir, pkgName)
        destDir <- file.path(destDir, pkgName)
        if (!file.exists(destDir))
            dir.create(destDir, recursive=TRUE)
        destFile <- file.path(destDir, "NEWS")
        getNEWSFromFile(srcDir, destFile, output="text")
    }
    
    tarballs <- list.files(file.path(reposRoot, srcContrib),
                           pattern="\\.tar\\.gz$", full.names=TRUE)
    if (!file.exists(destDir))
      dir.create(destDir, recursive=TRUE)
    if (!file.info(destDir)$isdir)
      stop("destDir must specify a directory")
    unpackDir <- tempdir()
    lapply(tarballs, extractNewsFromTarball, unpackDir=unpackDir)
    lapply(tarballs, convertNEWSToText, srcDir=unpackDir, destDir=destDir)
    
    invisible(NULL)
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


#


extractHTMLDocuments <- function(reposRoot, srcContrib, destDir) {
    ## Extract HTML documents from source package tarballs
    ## IF any HTML document is present in inst/doc.
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

    
    extractHTMLDocumentsFromTarball <- function(tarball, unpackDir=".") {
        ## helper function to unpack HTML documents and deps from tarball

        ## here we untar twice, once (just listing files) to see
        ## if there are html files in inst/doc, then if there are,
        ## we untar again (extracting). Optimal?
        fileList <- untar(tarball, list=TRUE)
        if (length(grep("inst/doc/.*\\.html$", fileList, ignore.case=TRUE)))
        {
            cat("Found HTML document in", tarball, "\n")
            ## This extracts everything, including 
            ## Rnw and Rmd files...too liberal? Then use vignettes/ dir
            pat <-  "--wildcards '*/inst/doc/*'"
            tarCmd <- paste("tar", "-C", unpackDir, "-xzf", tarball, pat)
            cat("Extracting HTML documents from", tarball, "\n")
            ret <- system(tarCmd)
            if (ret != 0)
              warning("tar had non-zero exit status for HTML extract of: ", tarball)
        }
    }

    tarballs <- list.files(file.path(reposRoot, srcContrib),
                           pattern="\\.tar\\.gz$", full.names=TRUE)
    if (!file.exists(destDir))
      dir.create(destDir, recursive=TRUE)
    if (!file.info(destDir)$isdir)
      stop("destDir must specify a directory")
    invisible(lapply(tarballs, extractHTMLDocumentsFromTarball,
        unpackDir=destDir))
}



#


getDcfValues <- function(values) {
    if (is.na(values)) return (character(0))
    values <- gsub("\n", " ", values, fixed=TRUE)
    l <- unlist(strsplit(values, ", ", fixed=TRUE))
    res <- unlist(lapply(l, function(x) {
        p <- strsplit(x, " |\\(", fixed=FALSE)
        unlist(p)[[1]]
    }))
    res
}



getFileExistsAttr <- function(pkgList, reposRootPath, dir, filename) {
    unlist(lapply(pkgList, function(pkg) {
        ret <- logical(0)
        filedir <- file.path(reposRootPath, dir, pkg)
        file <- file.path(filedir, filename)
        exists <- file.exists(filedir) && file.exists(file)
        ret <- c(ret, exists)
        ret
    }))
}


getFileLinks <- function(pkgList, reposRootPath, vignette.dir, ext,
    ignore.case=FALSE) {
    if (length(pkgList) == 0L)
        return(character(0))
    unlist(lapply(pkgList, function(pkg) {
        vigSubDir <- "inst/doc"
        vigDir <- file.path(reposRootPath, vignette.dir, pkg, vigSubDir)
        if (file.exists(vigDir)) {
            pattern <- paste(".*\\.", ext, "$", sep="")
            vigs <- list.files(vigDir, pattern=pattern,
                ignore.case=ignore.case)
            vigs <- paste(vignette.dir, pkg, vigSubDir, vigs, sep="/",
                          collapse=", ")
        } else
          vigs <- NA_character_
        vigs
    }))
    
}

getVignetteLinks <- function(pkgList, reposRootPath, vignette.dir) {
    if (length(pkgList) == 0L)
        return(character(0))
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


getVignetteTitlesFromListOfPdfs <- function(vigs) {
    rnws <- gsub(".pdf", ".Rnw", vigs, fixed=TRUE)
    vignetteTitles <- character()
    for (vignette in rnws) {
        titles <- character()
        rnwnames = unlist(strsplit(vignette, ", ", fixed=TRUE))
        for (rnwname in rnwnames) {
            #if(is.null(rnwname)) rnwname <- rnwnames ## why?
            if (file.exists(rnwname)) {
                lines <- readLines(rnwname, warn=FALSE)
                title  <- suppressWarnings(grep("VignetteIndexEntry\\{", lines, value=TRUE))
                segs = unlist(strsplit(title, "\\{|\\}"))
                segs <- segs[!grepl("\\\\", segs, fixed=TRUE)]
                segs <- segs[!grepl("^ $", segs, fixed=FALSE)]
                title <- segs[length(segs)]
                title <- gsub(",", ",,", title, fixed=TRUE)
                titles <- c(titles, title)
            } else {
                pdfName = sub(".Rnw", ".pdf", rnwname, fixed=TRUE)
                segs = unlist(strsplit(pdfName, "/", fixed=TRUE))
                titles <- c(titles, segs[length(segs)])
            }
        }
        tmp <- paste(titles, collapse=", ")
        vignetteTitles <- c(vignetteTitles, tmp)
    }
    vignetteTitles
}


getVignetteTitles <- function(pkgList, reposRootPath, vignette.dir) {
    if (length(pkgList) == 0L)
        return(character(0))
    unlist(lapply(pkgList, function(pkg) {
        vigSubDir <- "inst/doc"
        vigDir <- file.path(reposRootPath, vignette.dir, pkg, vigSubDir)
        if (file.exists(vigDir)) {
            vigs <- list.files(vigDir, pattern=".*\\.pdf$")
            vigs <- paste(vignette.dir, pkg, vigSubDir, vigs, sep="/",
                          collapse=", ")
            vigTitles <- getVignetteTitlesFromListOfPdfs(vigs)
        } else {
          vigTitles <- NA_character_
        }
        vigTitles
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
    if ("Bundle" %in% colnames(db)) {
        noPack <- is.na(db[, "Package"])
        db[noPack, "Package"] <- db[noPack, "Bundle"]
    }
    gzname <- paste(fname, "gz", sep=".")
    out <- file(file.path(dir, fname), "wt")
    ##FIXME: gzfile writing segfaults for me
    ##outgz <- gzfile(file.path(dir, gzname), "wt")
    for (i in seq_len(nrow(db))) {
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


write_VIEWS <- function(reposRootPath, fields = NULL,
                        type = c("source", "win.binary", "win64.binary",
                                 "mac.binary", "mac.binary.leopard"),
                        verbose = FALSE, vignette.dir="vignettes") {
    ## Copied from tools::write_PACKAGES
    if (is.null(fields))
      fields <- c("Title", "Description", "biocViews",
                  "Author", "Maintainer", "URL", "License",
                  "SystemRequirements", "organism", "manufacturer",
                  "hasReadme", "VignetteBuilder")
    if (missing(type))
      type <- "source"
    type <- match.arg(type)

    ## Read REPOSITORY file for contrib path info
    reposInfo <- read.dcf(file.path(reposRootPath, "REPOSITORY"))
    provided <-
      strsplit(gsub("[ \t\r\n\v\f]", "", reposInfo[, "provides"]), ",")[[1]]

    ## Use code from tools to build a matrix of package info
    ## by parsing the .tar.gz files
    pkg.dir <- file.path(reposRootPath, reposInfo[, "source"])
    db <- tools:::.build_repository_package_db(pkg.dir, fields, type, verbose)

    ## Turn 'db' into a matrix with 1 row per package
    if (length(db) != 0L) {
        dbMat <- do.call(rbind, db)
    } else {
        fields <- unique(c(tools:::.get_standard_repository_db_fields(), fields))
        dbMat <- matrix(nrow=0L, ncol=length(fields))
        colnames(dbMat) <- fields
    }

    ## Integrate version and archive file path info for the different contrib
    ## paths in this repos.  We duplicate the source path info here, but that
    ## makes things easier to handle later on as there is no special case.
    fldNames <- c(colnames(dbMat), paste(provided, "ver", sep="."))
    dbMat <- cbind(dbMat, matrix(NA, nrow=nrow(dbMat), ncol=length(provided)))
    colnames(dbMat) <- fldNames
    for (ctype in provided) {
        cPath <- reposInfo[, ctype]
        buildPkgPath <- function(pkgs, vers) {
            ext <- switch(ctype,
                          source=".tar.gz", win.binary=, win64.binary=".zip",
                          mac.binary.leopard=".tgz",
                          mac.binary=".tgz",
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
        
        if (length((grep("^win",ctype,value=TRUE)) > 0)  && ("Archs" %in% colnames(cDat))) {
          which1 <- which(dbMat[,"Package"] %in% cDat[,"Package"])
          which2 <- which(cDat[,"Package"] %in% dbMat[,"Package"])
          dbMat[which1, "Archs"] <- cDat[which2, "Archs"]
        }
    }
    ## Add vignette path info
    vigs <- getFileLinks(dbMat[, "Package"], reposRootPath, vignette.dir, "pdf")
    rfiles <- getFileLinks(dbMat[, "Package"], reposRootPath, vignette.dir, "R")

    htmlDocs <- getFileLinks(dbMat[, "Package"],
        reposRootPath, vignette.dir, "html", TRUE)
    htmlDocs[grep("\\/index\\.html$", htmlDocs)] <- NA


    #rfiles <- rfiles[grep("\\.R$", rfiles)]
    vtitles <- getVignetteTitles(dbMat[, "Package"], reposRootPath, vignette.dir)
    readmes <- getFileExistsAttr(dbMat[, "Package"], reposRootPath, "readmes", "README")
    news <- getFileExistsAttr(dbMat[, "Package"], reposRootPath, "news", "NEWS")
    install <- getFileExistsAttr(dbMat[, "Package"], reposRootPath, "install", "INSTALL")
    license <- getFileExistsAttr(dbMat[, "Package"], reposRootPath, "licenses", "LICENSE")
    dbMat <- cbind(dbMat, vigs)
    dbMat <- cbind(dbMat, vtitles)
    dbMat <- cbind(dbMat, readmes)
    dbMat <- cbind(dbMat, news)
    dbMat <- cbind(dbMat, install)
    dbMat <- cbind(dbMat, license)
    dbMat <- cbind(dbMat, rfiles)
    dbMat <- cbind(dbMat, htmlDocs)

    index <- grep("\\.html$", dbMat[, "htmlDocs"],
        invert=TRUE, ignore.case=TRUE)
    dbMat[index, "htmlDocs"] <- NA

    htmlTitles <- getHTMLTitles(dbMat[, "htmlDocs"], reposRootPath)
    dbMat <- cbind(dbMat, htmlTitles)


    colnames(dbMat) <- c(fldNames, "vignettes", "vignetteTitles", "hasREADME",
        "hasNEWS", "hasINSTALL", "hasLICENSE", "Rfiles", "htmlDocs",
        "htmlTitles")
    dependsOnMe <- getReverseDepends(dbMat, "Depends")
    
    index <- grep("\\.R$", dbMat[, "Rfiles"], invert=TRUE)
    dbMat[index, "Rfiles"] <- NA

    dbMat <- cbind(dbMat, dependsOnMe)
    importsMe <- getReverseDepends(dbMat, "Imports")
    dbMat <- cbind(dbMat, importsMe)
    suggestsMe <- getReverseDepends(dbMat, "Suggests")
    dbMat <- cbind(dbMat, suggestsMe)
    
    .write_repository_db(dbMat, reposRootPath, "VIEWS")
}

getReverseDepends <- function(db, fieldName) {
    pkgNames <- db[, "Package"]
    names(pkgNames) <- NULL
    df <- as.data.frame(db, stringsAsFactors=FALSE)
    depCols <- lapply(pkgNames, function(x) {
        pkgRecord <- subset(df, Package==x)
        pkgNames %in% getDcfValues(pkgRecord[fieldName])
    })
    depMat <- do.call(cbind, depCols)
    colnames(depMat) <- rownames(depMat) <- pkgNames
    ret <- character()
    bar <- function(x) {
        deps <- pkgNames[which(depMat[x, ])]
        ret <- c(ret, unlist(paste(deps, collapse=", ")))
    }
    ret <- lapply(pkgNames, bar)
    unlist(ret)
}


getHTMLTitle <- function(file)
{
    ## First look for an old-fashioned VignetteIndexEntry,
    ## because markdown/HTML vignettes will have one (in
    ## an HTML comment) which contains the canonical title.
    res <- grep("^%\\\\VignetteIndexEntry", readLines(file,
        warn=FALSE), value=TRUE)
    if (length(res))
    {
        title <- strsplit(res, "\\{|\\}")[[1]][2]
    } else {
        ## now look for an HTML title
        doc <- htmlParse(file)
        ns <- integer(0)
        tryCatch(ns <- getNodeSet(doc, "//title"),
            error=function(e) {})
        if (length(ns))
        { 
            title <-  gsub("^\\s+|\\s+$", "", xmlValue(ns[[1]])) 
            if (!nchar(title))
                title <- basename(file)
        }
        else ## just return the filename as a title
            title <- basename(file)
    }
    title <- gsub('"', '""', title)
    sprintf('"%s"', title)
}

getHTMLTitles <- function(files, reposRootPath)
{
    res <- c()

    for (file in files)
    {
        title <- NULL
        if (is.na(file))
        {
            res <- c(res, NA)
            next
        }
        if (grepl(", ", file))
        {
            subfiles <- strsplit(file, ", ")[[1]]
            fullpaths <- file.path(reposRootPath, subfiles)
            out <- sapply(fullpaths, getHTMLTitle)
            res <- c(res, paste(out, collapse=", "))
        } else {
            fullpath <- file.path(reposRootPath, file)
            res <- c(res, getHTMLTitle(fullpath))
        }
    }
    res
}

writeRFilesFromVignettes <- function(reposRoot, reposUrl="..",
                                viewUrl="../..", reposFullUrl=reposUrl,
                                downloadStatsUrl="", devHistoryUrl="") {

    pkgList <- loadPackageDetails(reposRoot, reposUrl, viewUrl, reposFullUrl,
          downloadStatsUrl, devHistoryUrl)
    StangleHTMLVignettes(reposRoot)
}


.printf <- function(...) print(noquote(sprintf(...)))

StangleHTMLVignettes <- function(reposRoot)
{
    viewsFile <- file.path(reposRoot, "VIEWS")
    pkgMat <- readPackageInfo(viewsFile)
    info <- read.dcf(file=viewsFile)
    apply(info, 1, function(x){
        if (!is.na(x["htmlDocs"]))
        {
            docs <- strsplit(x["htmlDocs"], ", ")[[1]]
            for (doc in docs) {
                vig <- sub("\\.html", ".Rmd", doc, ignore.case=TRUE)
                out <- sub("\\.html", ".R", doc, ignore.case=TRUE)
                if (file.exists(vig))
                    purl(vig, out)
            }
        }
        })

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
