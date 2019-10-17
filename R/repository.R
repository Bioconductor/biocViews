genReposControlFiles <- function(reposRoot, contribPaths, manifestFile=NA, meatPath=NA)
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
    write_VIEWS(reposRoot, manifestFile=manifestFile, meatPath=meatPath)

    ## Write a SYMBOLS file at the top-level containing the
    ## exported symbols for all packages that have name
    ## spaces.  This is used to build a searchable index.
    write_SYMBOLS(reposRoot, verbose=TRUE)
}

pkgName <- function(tarball) {
    strsplit(basename(tarball), "_", fixed=TRUE)[[1L]][1L]
}

unpack <- function(tarball, unpackDir, wildcards, ...) {
    cmd <- paste("tar", "-C", unpackDir, "-xzf", tarball, "--wildcards", wildcards)
    system(cmd, ...)
}

cleanUnpackDir <- function(tarball, unpackDir, subDir="", pattern=NULL) {
    ## Delete files from a previous extraction
    pkg <- pkgName(tarball)
    pkgDir <- file.path(unpackDir, pkg, subDir)
    files <- list.files(pkgDir, pattern=pattern, full.names=TRUE,
                        recursive=is.null(pattern), include.dirs=is.null(pattern))
    unlink(files)
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

    buildManualsFromTarball <- function(tarball, unpackDir=".") {
        ## helper function to unpack pdf & Rd files from the vig
        status <- TRUE
        cleanUnpackDir(tarball, unpackDir, "man", ".*\\.(pdf|Rd|rd)$")
        ret <- unpack(tarball, unpackDir, "'*/man/*.[Rr]d'")
        if (ret != 0) {
            warning("non-zero exit status ", ret, " extracting man pages: ",
                    tarball)
            status <- FALSE
        } else {
            pkg <- pkgName(tarball)
            pkgDir <- file.path(unpackDir, pkg, "man")
            RCmd <- file.path(Sys.getenv("R_HOME"), "bin", "R")
            Rd2pdfCmd <- paste0(
                RCmd, " CMD Rd2pdf --no-preview ",
                "--output=", pkgDir, "/", pkg, ".pdf ",
                "--title=", pkg, " ", pkgDir, "/*.[Rr]d")
            ret <- system(Rd2pdfCmd)
            cleanUnpackDir(tarball, unpackDir, "man", ".*\\.(Rd|rd)$")
            if (ret != 0) {
                warning("non-zero exit status ", ret, " building ref man: ", pkg)
                status <- FALSE
            }
        }
        status
    }

    tarballs <- list.files(file.path(reposRoot, srcContrib),
            pattern="\\.tar\\.gz$", full.names=TRUE)
    if (!file.exists(destDir))
        dir.create(destDir, recursive=TRUE)
    if (!file.info(destDir)$isdir)
        stop("destDir must specify a directory")
    if (endsWith(reposRoot, "data/annotation")) {
        n <- vapply(tarballs, function(tarball, ...) {
            tryCatch({
                buildManualsFromTarball(tarball, ...)
            }, error = function(e) {
                warning("error extracting manual for: ", tarball,
                        "\n  ", conditionMessage(e))
                FALSE
            })
        }, logical(1), unpackDir=destDir)
    } else {
        n <- 0
    }
    paste(sum(n), "/", length(tarballs), "tarball manuals processsed")
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

    extractFileFromTarball <- function(tarball, unpackDir=".") {
        pkg <- pkgName(tarball)
        cleanUnpackDir(tarball, unpackDir, pattern=fileName)
        cat(paste("Attempting to extract", fileName, "from", tarball, "\n"))
        unpack(tarball, unpackDir, file.path(pkg, fileName),
               ignore.stdout=TRUE, ignore.stderr=TRUE)
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

.extract_citation <- function(tarball) {
    pkgname <- pkgName(tarball)
    tmpdir <- tempdir()

    ## Extract DESCRIPTION file from tarball.
    ## Note that the path separator is **always** / in a tarball, even
    ## on Windows, so do NOT use file.path() here.
    path <- paste0(pkgname, "/DESCRIPTION")
    status <- untar(tarball, path, exdir=tmpdir)
    ## Should never happen.
    if (status != 0L)
        stop("failed to extract DESCRIPTION file from ", tarball)

    ## Extract CITATION file from tarball, if present.
    path <- paste0(pkgname, "/inst/CITATION")
    all_paths <- untar(tarball, list=TRUE)
    status <- attr(all_paths, "status")
    if (!is.null(status) && status != 0L)
        stop("failed to extract list of paths from ", tarball)
    if (path %in% all_paths) {
        status <- untar(tarball, path, exdir=tmpdir)
        ## Should never happen.
        if (status != 0L)
            stop("failed to extract CITATION file from ", tarball)
    } else {
        ## Remove (possibly) pre-existing CITATION file from tmpdir.
        ## Could happen if 'tmpdir' somehow already contained a stale source
        ## tree for 'pkgname'.
        full_path <- file.path(tmpdir, pkgname, "inst", "CITATION")
        status <- unlink(full_path)
        ## Should never happen.
        if (status != 0L)
            stop("failed to remove ", full_path, " file")
    }

    ## Make the citation object.
    description <- packageDescription(pkgname, lib.loc=tmpdir)
    citation(pkgname, lib.loc=tmpdir, auto=description)
}

.write_citation_as_HTML <- function(citation, destdir) {
    destfile <- file.path(destdir, "citation.html")
    if (dir.exists(destdir)) {
        status <- unlink(destfile)
        if (status != 0L)
            stop("failed to remove previous ", destfile, " file")
    } else {
        if (!dir.create(destdir))
            stop("failed to create ", destdir, " directory")
    }
    html <- capture.output(print(citation, style="html"))
    ## Filter out lines starting with \Sexprs.
    html <- html[grep("^\\\\Sexpr", html, invert=TRUE)]
    cat(html, file=destfile, sep="\n")
}

extractCitations <- function(reposRoot, srcContrib, destDir) {
    tarballs <- list.files(
        file.path(reposRoot, srcContrib),
        pattern="\\.tar\\.gz$", full.names=TRUE)

    if (!dir.exists(destDir)) {
        if (!dir.create(destDir, recursive=TRUE))
            stop("failed to create ", destDir, " directory")
    }

    for (tarball in tarballs) {
        cat("Extract and process citation from ", tarball, " ...", sep="")
        citation <- .extract_citation(tarball)
        pkgname <- pkgName(tarball)
        .write_citation_as_HTML(citation, file.path(destDir, pkgname))
        cat("OK\n")
    }
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

    extractNewsFromTarball <- function(tarball, unpackDir=".") {
        pkg <- pkgName(tarball)
        cleanUnpackDir(tarball, unpackDir, pattern="NEWS")
        cat("Attempting to extract NEWS from", tarball, "\n")
        unpack(tarball, unpackDir, "'*NEWS*'",
               ignore.stdout=TRUE, ignore.stderr=TRUE)
    }

    convertNEWSToText <- function(tarball, srcDir, destDir) {
        pkg <- pkgName(tarball)
        srcDir <- file.path(srcDir, pkg)
        destDir <- file.path(destDir, pkg)
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
    ## Extract vignettes from source package tarballs
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

    extractVignettesFromTarball <- function(tarball, unpackDir=".") {
        cleanUnpackDir(tarball, unpackDir, subDir=file.path("inst", "doc"))
        cat("Extracting vignettes from", tarball, "\n")
        ret <- unpack(tarball, unpackDir, "'*/inst/doc/*'")
        if (ret != 0)
            warning("tar had non-zero exit status for vig extract of: ", tarball)
    }

    tarballs <- list.files(file.path(reposRoot, srcContrib),
                           pattern="\\.tar\\.gz$", full.names=TRUE)
    if (!file.exists(destDir))
        dir.create(destDir, recursive=TRUE)
    if (!file.info(destDir)$isdir)
        stop("destDir must specify a directory")

    invisible(lapply(tarballs, extractVignettesFromTarball, unpackDir=destDir))
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
            cat("Extracting HTML documents from", tarball, "\n")
            ret <- unpack(tarball, unpackDir, "'*/inst/doc/*'")
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
        vigs <- NA_character_
        if (file.exists(vigDir)) {
            pattern <- paste(".*\\.", ext, "$", sep="")
            files <- list.files(vigDir, pattern=pattern,
                                ignore.case=ignore.case)
            if (length(files))
                vigs <- paste(vignette.dir, pkg, vigSubDir, files, sep="/",
                              collapse=", ")
        }
        vigs
    }))
}

getDocumentTitles <- function(docs, ext="pdf", src=c("Rnw", "Rmd"), reposRootPath, fun) {
    if (length(docs) == 0L)
        return(character())
    filelist <- strsplit(docs, ", ", fixed = TRUE)
    unlist(lapply(filelist, function(files) {
        if (all(is.na(files))) {
            NA_character_
        }
        else {
            files <- file.path(reposRootPath, files)
            titles <- unlist(lapply(files, function(file) {
                title <- NA_character_
                src <- paste0(sub(sprintf("\\.%s$", ext), ".", file, ignore.case=TRUE), src)
                idx <- which(file.exists(src))[1L]
                ## extract title from source file
                if (!is.na(idx)) {
                    title <- fun(file, src[idx])
                    title <- trimws(title)
                    title <- gsub(",", ",,", title, fixed=TRUE)
                }
                ## use filename if no source file found, title extraction failed,
                ## or the extracted title is empty
                if (is.na(title) || nchar(title)==0L)
                    basename(file)
                else
                    title
            }))
            paste(titles, collapse=", ")
        }
    }))
}

getVignetteIndexEntry <- function(file) {
    lines <- readLines(file, warn=FALSE)
    ## use the same regular expression as in tools:::.get_vignette_metadata
    regex <- "[[:space:]]*%+[[:space:]]*\\\\VignetteIndexEntry\\{([^}]*(\\{[^}]*\\})*[^}]*)\\}.*"
    ## match to first occurance
    res <- grep(regex, lines, value = TRUE)[1L]
    gsub(regex, "\\1", res)
}

getPdfTitle <- function(doc, src) {
    getVignetteIndexEntry(src)
}

getHtmlTitle <- function(doc, src) {
    ## First look for an old-fashioned VignetteIndexEntry in the source file
    title <- getVignetteIndexEntry(src)
    if (is.na(title)) {
        ## now look for an HTML title
        doc <- htmlParse(doc)
        res <- xpathApply(doc, "//title", xmlValue)
        if (length(res))
            title <- res[[1L]]
    }
    title
}

write_REPOSITORY <- function(reposRootPath, contribPaths) {
    contrib <- as.list(contribPaths)
    names(contrib) <- names(contribPaths)
    contrib[["provides"]] <- paste(names(contrib), collapse=", ")
    fn <- file.path(reposRootPath, "REPOSITORY")
    write.dcf(contrib, fn)
}

read_REPOSITORY <- function(reposRootPath)
{
    reposInfo <- read.dcf(file.path(reposRootPath, "REPOSITORY"))
    reposInfo[, "provides"] <- gsub("[ \t\r\n\v\f]", "",
                                    reposInfo[, "provides"])
    provided <- strsplit(reposInfo[, "provides"], ",")[[1L]]
    m <- match(gsub("-", ".", provided), colnames(reposInfo))
    if (anyNA(m))
        stop("malformed REPOSITORY file: 'provides' field is inconsistent ",
             "with other fields)")
    if (anyDuplicated(m))
        stop("malformed REPOSITORY file: several values in 'provides' field ",
             "are mapped to the same entry in the file")
    colnames(reposInfo)[m] <- provided
    reposInfo
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
                        verbose = FALSE, vignette.dir="vignettes",
                        manifestFile=NA, meatPath=NA
                        ) {
    ## Copied from tools::write_PACKAGES
    if (is.null(fields))
      fields <- c("Title", "Description", "biocViews",
                  "Author", "Maintainer", "URL", "License",
                  "SystemRequirements", "organism", "manufacturer",
                  "hasReadme", "VignetteBuilder", "Video", "BugReports",
                  "PackageStatus", "git_url", "git_branch",
                  "git_last_commit", "git_last_commit_date", "Date/Publication")

    ## Read REPOSITORY file for contrib path info
    reposInfo <- read_REPOSITORY(reposRootPath)
    provided <- strsplit(reposInfo[, "provides"], ",")[[1L]]
    fields = unique(c(tools:::.get_standard_repository_db_fields("source"),
        tools:::.get_standard_repository_db_fields("mac.binary"),
        tools:::.get_standard_repository_db_fields("win.binary"),
        fields))

    convertToMat <- function(reposRootPath, reposInfo, os, fields, verbose){

        ## Use code from tools to build a matrix of package info
        pkg.dir <- file.path(reposRootPath, reposInfo[, os])
        if(grepl(os, pattern="mac.binary")) os = "mac.binary"
        if(grepl(os, pattern="win.binary")) os = "win.binary"
        db <- tools:::.build_repository_package_db(pkg.dir, fields, os, verbose)
        ## Turn 'db' into a matrix with 1 row per package
        if (length(db) != 0L) {
            dbMatTemp <- do.call(rbind, db)
        } else {
            dbMatTemp <- matrix(nrow=0L, ncol=length(fields))
            colnames(dbMatTemp) <- fields
        }
        dbMatTemp
    }

    # get standard list of fields information for packages
    os = provided[1]
    dbMat = convertToMat(reposRootPath, reposInfo, os, fields, verbose)
    if (length(provided) > 1){
        otheros = provided[-1]
        for(os in otheros){
            dbMat2 = convertToMat(reposRootPath, reposInfo, os, fields, verbose)
            idx = !(dbMat2[,"Package"] %in% dbMat[, "Package"])
            if (length(which(idx)) != 0){
                tempMat = dbMat2[idx,]
                dbMat = rbind(dbMat, tempMat)
            }
        }
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
                          'source'=".tar.gz",
                          'win.binary'=".zip",
                          'mac.binary'=,
                          'mac.binary.mavericks'=,
                          'mac.binary.el-capitan'=".tgz",
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
    vtitles <- getDocumentTitles(vigs, reposRootPath=reposRootPath, fun=getPdfTitle)

    rfiles <- getFileLinks(dbMat[, "Package"], reposRootPath, vignette.dir, "R")

    htmlDocs <- getFileLinks(dbMat[, "Package"], reposRootPath, vignette.dir, "html", TRUE)
    htmlDocs[grep("\\/index\\.html$", htmlDocs)] <- NA
    htmlTitles <- getDocumentTitles(htmlDocs, ext="html", src=c("Rmd", "Rhtml"), reposRootPath, getHtmlTitle)

    allVigs <- paste(vigs, htmlDocs, sep=", ")
    allTitles <- paste(vtitles, htmlTitles, sep=", ")

    formatVec <- function(vec){
        vec <- gsub(pattern="NA, NA", replacement=NA, vec)
        vec <- gsub(pattern="NA, ", replacement="", vec)
        vec <- gsub(pattern=", NA", replacement="", vec)
        vec
    }
    allVigs <- formatVec(allVigs)
    allTitles <- formatVec(allTitles)
    names(allVigs) <- names(vigs)
    names(allTitles) <- names(vtitles)

    # get any included extra files
    readmes <- getFileExistsAttr(dbMat[, "Package"], reposRootPath, "readmes", "README")
    news <- getFileExistsAttr(dbMat[, "Package"], reposRootPath, "news", "NEWS")
    install <- getFileExistsAttr(dbMat[, "Package"], reposRootPath, "install", "INSTALL")
    license <- getFileExistsAttr(dbMat[, "Package"], reposRootPath, "licenses",
                                 "LICENSE")

    # add additional values to matrix for writing
    dbMat <- cbind(dbMat, allVigs)
    dbMat <- cbind(dbMat, allTitles)
    dbMat <- cbind(dbMat, readmes)
    dbMat <- cbind(dbMat, news)
    dbMat <- cbind(dbMat, install)
    dbMat <- cbind(dbMat, license)
    dbMat <- cbind(dbMat, rfiles)

    colnames(dbMat) <- c(fldNames, "vignettes", "vignetteTitles", "hasREADME",
        "hasNEWS", "hasINSTALL", "hasLICENSE", "Rfiles")

    # get reverse dependency list
    dependsOnMe <- getReverseDepends(dbMat, "Depends")
    dbMat <- cbind(dbMat, dependsOnMe)
    importsMe <- getReverseDepends(dbMat, "Imports")
    dbMat <- cbind(dbMat, importsMe)
    suggestsMe <- getReverseDepends(dbMat, "Suggests")
    dbMat <- cbind(dbMat, suggestsMe)
    linksToMe <- getReverseDepends(dbMat, "LinkingTo")
    dbMat <- cbind(dbMat, linksToMe)


    # add (recursive) dependency count for badge on landing page
    all_repos <- repositories()
    all_pkgs <- available.packages(repos = all_repos)
    bioc_pkgs <- available.packages(repos = all_repos[setdiff(names(all_repos),
                                        "CRAN")] )
    deps <- package_dependencies(rownames(bioc_pkgs), db = all_pkgs,
                                 recursive=TRUE)
    numDeps <- lengths(deps)
    dependencyCount <-  numDeps[dbMat[, "Package"]]
    dbMat <- cbind(dbMat, dependencyCount)

    # Add place Holder for valid packages compared to manifest
    # That haven't built so they get a shell landing page rather
    # than no landing page
    if (!is.na(manifestFile)){
        if(file.exists(manifestFile)){
            file  = readLines(manifestFile)
            fmtFile = vapply(file, FUN = function(vl){
                if(startsWith(vl, "Package")){
                    trimws(gsub(vl, pattern="Package: ", replacement=""))
                }else{
                    ""
                }},
                FUN.VALUE=character(1), USE.NAMES=FALSE)
            man_pkgs = fmtFile[-which(fmtFile=="")]
            missing_pkgs = man_pkgs[!(man_pkgs %in% unname(dbMat[,"Package"]))]
            add_mat = matrix(NA, nrow=length(missing_pkgs), ncol=ncol(dbMat))
            rownames(add_mat) = missing_pkgs
            colnames(add_mat) = colnames(dbMat)
            # manually fill info for missing packages
            add_mat[,which(colnames(dbMat)=="Package")] = missing_pkgs
            if (!is.na(meatPath)){
            for(i in seq_along(missing_pkgs)){

                add_mat = tryCatch({
                    desc = read.dcf(file.path(meatPath, missing_pkgs[i],
                        "DESCRIPTION"))

                    for (dx in colnames(desc)){
                        if (dx %in% colnames(add_mat)){
                            add_mat[i, which(colnames(add_mat) == dx)] = desc[,dx]
                        }else{
                            # check for Authors@R and parse accordingly
                            if (dx == "Authors@R"){
                                ar = desc[,"Authors@R"]
                                env <- new.env(parent=emptyenv())
                                env[["c"]] = c
                                env[["person"]] <- utils::person
                                pp <- parse(text=ar, keep.source=TRUE)
                                people =
                                    tryCatch({
                                        people <- eval(pp, env)
                                        people
                                    }, error=function(e) {
                                        # could not parse Authors@R
                                        people <- "ERROR"
                                        people
                                    }, warning = function(e){
                                        people <- "ERROR"
                                        people
                                    })

                                if (all(people == "ERROR")){
                                    add_mat[i,which(colnames(dbMat)=="Maintainer")] = "ERROR"
                                    add_mat[i,which(colnames(dbMat)=="Author")] = "ERROR"
                                }else{
                                    Author = paste0(unlist(people$given)," ", unlist(people$family), " <", unlist(people$email),">", collapse=", ")
                                    add_mat[i,which(colnames(dbMat)=="Author")] = Author
                                    idx = !is.na(unlist(lapply(people$role, FUN=match, x="cre")))
                                    if (any(idx)){
                                        people = people[idx]
                                        add_mat[i,which(colnames(dbMat)=="Maintainer")] =
                                            paste0(unlist(people$given)," ", unlist(people$family), " <", unlist(people$email),">", collapse=", ")
                                    }else{
                                        add_mat[i,which(colnames(dbMat)=="Maintainer")] = "ERROR"
                                    }

                                }
                            }
                        }
                    }
                    add_mat
                }, error = function(err){
                    add_mat[i,which(colnames(dbMat)=="Maintainer")] = "ERROR"
                    add_mat[i,which(colnames(dbMat)=="Title")] = "ERROR"
                    add_mat
                }, warning = function(err){
                    add_mat[i,which(colnames(dbMat)=="Maintainer")] = "ERROR"
                    add_mat[i,which(colnames(dbMat)=="Title")] = "ERROR"
                    add_mat
                })
            }
        }
            # make sure necessary columns are not NA
            if (any(is.na(add_mat[,"Title"]))){
                add_mat[which(is.na(add_mat[,"Title"])), "Title"] =
                    "ERROR"
            }
             if (any(is.na(add_mat[,"Maintainer"]))){
                add_mat[which(is.na(add_mat[,"Maintainer"])), "Maintainer"] =
                    "ERROR"
            }
            dbMat = rbind(dbMat, add_mat)
        }
    }
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
        if (!is.na(x["vignettes"]))
        {
            if (!requireNamespace("knitr")) {
                stop("'knitr' package required to tangle HTML vignettes")
            }
            docs <- strsplit(x["vignettes"], ",\n")[[1]]
            docs <- docs[endsWith(docs, "html")]
            for (doc in docs) {
                vig <- sub("\\.html", ".Rmd", doc, ignore.case=TRUE)
                out <- sub("\\.html", ".R", doc, ignore.case=TRUE)
                if (file.exists(vig))
                    tryCatch(knitr::purl(vig, out), error=function(e){
                        print(e)
                        })
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
        ret <- unpack(tarball, unpackDir, "'*/NAMESPACE'")
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
