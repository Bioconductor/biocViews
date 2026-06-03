test_getHtmlTitle <- function() {
    doc <- system.file(
        "htmlfrags",
        "topfrag.html",
        package = "biocViews",
        mustWork = TRUE
    )
    src <- system.file(
        "extdata",
        "vignette.Rmd",
        package = "biocViews",
        mustWork = TRUE
    )
    checkIdentical(
        getVignetteIndexEntry(src),
        "Vignette for beginners"
    )
    checkIdentical(
        getHtmlTitle(doc, src),
        "Vignette for beginners"
    )
    checkIdentical(
        suppressWarnings({
            getHtmlTitle(doc, "")
        }),
        "Bioconductor Task View: top level views"
    )
}

test_getDocumentTitles <- function() {
    htmlDocs <- system.file("extdata", package = "biocViews") |>
        list.files(pattern = "\\.[Hh][Tt][Mm][Ll]$", full.names = TRUE)

    checkIdentical(
        getDocumentTitles(
            basename(htmlDocs),
            ext = "html",
            src = c("Rmd", "Rhtml"),
            dirname(htmlDocs),
            getHtmlTitle
        ),
        "Vignette for beginners"
    )
}

test_getVignetteIndexEntry <- function() {
    rmdDocs <- system.file("extdata", package = "biocViews") |>
        list.files(pattern = "\\.[Rr][Mm][Dd]$", full.names = TRUE)

    checkIdentical(
        getVignetteIndexEntry(rmdDocs),
        "Vignette for beginners"
    )
}
