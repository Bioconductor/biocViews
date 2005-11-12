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


genReposControlFiles <- function(reposRoot, contribPaths)
{
    ## Write PACKAGES files for all contrib paths
    packagesPaths <- file.path(reposRoot, contribPaths)
    names(packagesPaths) <- names(contribPaths)
    for (type in names(packagesPaths)) {
        path <- packagesPaths[[type]]
        write_PACKAGES(path, type=type)
    }

    ## Write a VIEWS file at the top-level containing
    ## detailed package info
    write_VIEWS(pkg.dir=packagesPaths[["source"]],
                out.dir=reposRoot, type="source")
}


.write_repository_db <- function(db, dir, fname) {
    if (length(db)) {
        fields <- colnames(db[[1]])
        db <- matrix(unlist(db), ncol = length(fields), byrow = TRUE)
        colnames(db) <- fields
        noPack <- is.na(db[, "Package"])
        db[noPack, "Package"] <- db[noPack, "Bundle"]
        gzname <- paste(fname, "gz", sep=".")
        out <- file(file.path(dir, fname), "wt")
        outgz <- gzfile(file.path(dir, gzname), "wt")
        for (i in seq(length = nrow(db))) {
            dbi <- db[i, !(is.na(db[i, ]) | (db[i, ] == 
                                                   "")), drop = FALSE]
            write.dcf(dbi, file = out)
            write.dcf(dbi, file = outgz)
        }
        close(out)
        close(outgz)
        invisible(nrow(db))
    }
    else invisible(0)
}


write_VIEWS <- function(pkg.dir, out.dir, fields = NULL,
                        type = c("source", "mac.binary", "win.binary"),
                        verbose = FALSE) {
    ## Copied from tools::write_PACKAGES
    if (is.null(fields))
      fields <- c("Title", "Description", "biocViews",
                  "Author", "Maintainer", "URL", "License",
                  "SystemRequirements")
    if (missing(type) && .Platform$OS.type == "windows") 
      type <- "win.binary"
    type <- match.arg(type)
    db <- tools:::.build_repository_package_db(pkg.dir, fields, type, verbose)
    .write_repository_db(db, out.dir, "VIEWS")
}


writeRepositoryHtml <- function(reposRoot, contribPaths, title) {
    pkgList <- loadPackageDetails(reposRoot, contribPaths)
    writePackageDetailHtml(pkgList, reposRoot)
    writeRepositoryIndexHtml(pkgList, reposRoot, title)
}


writePackageDetailHtml <- function(pkgList, reposRoot, htmlDir="html") {
    htmlDir <- file.path(reposRoot, htmlDir)
    if (!file.exists(htmlDir))
      dir.create(htmlDir)
    for (pkg in pkgList) {
        f <- file.path(htmlDir, htmlFilename(pkg))
        cat("writing html for", pkg@Package, "\n")
        writeHtml(pkg, f)
    }
}


writeRepositoryIndexHtml <- function(pkgList, reposRoot, title, htmlDir="html")
{
    repos <- new("RepositoryDetail", Title=title, reposRoot=reposRoot,
                 htmlDir=htmlDir, packageList=pkgList)
    f <- file.path(reposRoot, htmlFilename(repos))
    writeHtml(repos, f)
}
