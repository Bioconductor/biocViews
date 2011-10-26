.msg <-
    function(fmt, ..., width=getOption("width"))
    ## Use this helper to format all error / warning / message text
{
    txt <- strwrap(sprintf(fmt, ...), width=width, indent=4)
    paste(sub("^ +", "", txt), collapse="\n")
}


## This function assumes that all BioC packages installed are 
## the latest versions, or at least from the current release.
## This is a safe assumption to make if the function is run
## on the build machine for the release in question.
collatePackageNEWS <- function() {
    vers <- getRversion()
    biocVers <-
        tryCatch(tools:::.BioC_version_associated_with_R_version,
                 error=function(...) numeric_version(0.0))
    manifestUrl <-
      paste("https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/bioc_",
      biocVers, ".manifest", sep="")
      
    rawManifest <- getURL(manifestUrl, userpwd="readonly:readonly")
    manifestLines <- unlist(strsplit(rawManifest, "\n"))
    pkgLines <- manifestLines[grep("^Package: ", manifestLines)]
    manifestPkgs <- gsub("Package: ", "", pkgLines)
    
    ips <- installed.packages()[, "Package"]

    matches <- ips[ips %in% manifestPkgs]
    matches <- sort(matches)
    names(matches) <- NULL
    lv <- manifestPkgs %in% ips
    nonmatches <- manifestPkgs[!lv]
    nmstr <- paste(nonmatches, collapse="' '")
    

    
    newsList <- list()
    ip <- installed.packages()

    for (pkg in matches) {
        vec <- ip[, "Package"] == pkg
        pkgRecord <- ip[vec, , drop=FALSE]
        pkgVers <- pkgRecord[, "Version"]
        cat(pkgVers, "\n")
        segs = unlist(strsplit(pkgVers, ".", fixed=TRUE))
        segs[3] <- "0"
        firstReleaseVer <- paste(segs, collapse=".")
        news <- NULL
        try(news <- utils::news(Version >= firstReleaseVer, package=pkg),
          silent=TRUE)
        if (!is.null(news)){
            newsList <- c(newsList, news)
        }
    }

    

    warning(.msg("The following packages in the manifest are not installed: %s",
      nmstr))

    unlist(newsList)
    
}

