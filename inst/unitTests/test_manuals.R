tmpRoot    <- tempdir()
repoRoot   <- file.path("inst", "unitTests", "repo")
bioc       <- file.path(tmpRoot, "repo", "bioc")
annotation <- file.path(tmpRoot, "repo", "data", "annotation")
experiment <- file.path(tmpRoot, "repo", "data", "experiment")
srcContrib <- file.path("src", "contrib")
builds     <- c("bioc", annotation, experiment)

copyToTmp <- function() {
    if (!dir.exists(tmpRoot))
        dir.create(tmpRoot)
    file.copy(repoRoot, tmpRoot, recursive = TRUE)
}

filesExist <- function(path, pkg) {
    file_list <- list.files(path, recursive = TRUE)
    paste0(pkg, ".html") %in% file_list && paste0(pkg, ".pdf") %in% file_list
}

test_extractManuals <- function() {
    copyToTmp()

    # check all manuals processed
    # package with manual
    checkEquals(extractManuals(bioc, srcContrib),
                "1 / 1 tarball manuals processed")
    # package without a manual
    checkEquals(extractManuals(annotation, srcContrib),
                "1 / 1 tarball manuals processed")
    # package without a manual
    checkEquals(extractManuals(experiment, srcContrib),
                "1 / 1 tarball manuals processed")

    # check for both html and pdf files
    myBiocPkgMan <- file.path(bioc, "manuals", "myBiocPkg", "man")
    checkTrue(filesExist(myBiocPkgMan, "myBiocPkg"))
    myAnnPkgMan <- file.path(annotation, "manuals", "myAnnPkg", "man")
    checkTrue(filesExist(myAnnPkgMan, "myAnnPkg"))
    myExpPkgMan <- file.path(experiment, "manuals", "myExpPkg", "man")
    checkTrue(filesExist(myExpPkgMan, "myExpPkg"))
}
