
setup1 <- function()
{
    t <- file.path(tempdir(), "testing")
    if (file.exists(t))
        unlink(t, recursive=TRUE)
    dir.create(t)
    dir.create(file.path(t, "testrepos", "src", "contrib"), recursive=TRUE)
    dir.create(file.path(t, "maketarballs", "biocViews"), recursive=TRUE)
    dir.create(file.path(t, "destdir"))
    file.copy(system.file("DESCRIPTION", package="biocViews"),
        file.path(t, "maketarballs", "biocViews"))
    vers <- as.character(packageVersion("biocViews"))
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(file.path(t, "maketarballs"))
    tar(file.path(t, "testrepos", "src", "contrib", 
        paste0("biocViews_", vers, ".tar.gz")),
        "biocViews",
        compression="gzip"
    )
}

test_citation_from_description <- function() 
{
    setup1()
    t <- file.path(tempdir(), "testing")
    unlink(file.path(t, "destdir"), recursive=TRUE)
    dir.create(file.path(t, "destdir"))


    extractCitations(file.path(t, "testrepos"), "src/contrib",
        file.path(t, "destdir"))
    checkTrue(file.exists(file.path(t, "destdir",
        "biocViews", "citation.html")))
    lines <- readLines(file.path(t, "destdir",
        "biocViews", "citation.html"))
    #browser()
    checkTrue(any(grepl("Categorized views", lines)))    
}


setup2 <- function()
{
    t <- file.path(tempdir(), "testing")
    if (file.exists(t))
        unlink(t, recursive=TRUE)
    dir.create(t)
    dir.create(file.path(t, "testrepos", "src", "contrib"), recursive=TRUE)
    dir.create(file.path(t, "maketarballs", "biocViews2",
        "inst"), recursive=TRUE)
    dir.create(file.path(t, "destdir"))
    file.copy(system.file("DESCRIPTION", package="biocViews"),
        file.path(t, "maketarballs", "biocViews2"))
    file.copy(system.file("unitTests",
        "CITATION-tmpl", package="biocViews"),
        file.path(t, "maketarballs", "biocViews2", "inst", "CITATION"))
    vers <- as.character(packageVersion("biocViews"))
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(file.path(t, "maketarballs"))
    tar(file.path(t, "testrepos", "src", "contrib", 
                  paste0("biocViews2_", vers, ".tar.gz")),
        "biocViews2",
        compression="gzip"
    )
}

test_citation_from_citation <- function() 
{
    setup2()
    t <- file.path(tempdir(), "testing")
    unlink(file.path(t, "destdir"), recursive=TRUE)
    dir.create(file.path(t, "destdir"))

    extractCitations(file.path(t, "testrepos"), "src/contrib",
        file.path(t, "destdir"))
    checkTrue(file.exists(file.path(t, "destdir",
        "biocViews2", "citation.html")))
    lines <- readLines(file.path(t, "destdir",
        "biocViews2", "citation.html"))
    checkTrue(any(grepl("Open software development", lines)))    
}
