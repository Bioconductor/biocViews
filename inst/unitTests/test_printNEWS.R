.toyNEWS <- function()
{
    md <- tempfile(fileext=".md")
    on.exit(unlink(md))
    writeLines(c(
        "# toy 1.1.2", "",
        "- Item one", "- Item two", "- Item three", "",
        "# toy 1.1.1", "",
        "- Older item"
    ), md)
    tools:::.build_news_db_from_package_NEWS_md(md)
}

.printToy <- function(...)
{
    con <- textConnection("out", "w", local=TRUE)
    printNEWS(list(toy=.toyNEWS()), con, ...)
    close(con)
    out
}

test_printNEWS_no_truncation <- function()
{
    out <- .printToy(relativeLink=TRUE, maxLines=Inf)
    checkTrue(any(grepl("Older item", out)))
    checkTrue(!any(grepl("See the full NEWS", out)))
}

test_printNEWS_truncation_keeps_whole_items <- function()
{
    ## version header + 3 items = 4 lines; the 1.1.1 header that would
    ## be left without items is dropped
    out <- .printToy(relativeLink=TRUE, maxLines=5L)
    checkTrue(any(grepl("Item three", out)))
    checkTrue(!any(grepl("1\\.1\\.1|Older item", out)))
    checkTrue(any(out == paste0(
        "[See the full NEWS](/packages/release/bioc/news/toy/NEWS) ",
        "or run `news(package = \"toy\")`."
    )))
}

test_printNEWS_truncation_link_uses_repo <- function()
{
    out <- .printToy(maxLines=2L, repo="data/experiment")
    checkTrue(any(grepl(
        "(https://bioconductor.org/packages/release/data/experiment/news/toy/NEWS)",
        out, fixed=TRUE
    )))
}

test_truncateNEWSLines <- function()
{
    lines <- c(
        "", "        Changes in version 2.0.0", "",
        "NEW FEATURES", "",
        "- a", "  continued", "",
        "- b",
        "        Changes in version 1.0.0", "",
        "- c"
    )
    f <- biocViews:::.truncateNEWSLines
    ## 7 non-blank lines in units of 1, 1, 2, 1, 1, 1 lines
    res <- f(lines, 4L)
    checkTrue(attr(res, "truncated"))
    checkIdentical(as.vector(res), lines[1:7])
    res <- f(lines, 5L)
    checkIdentical(as.vector(res), lines[1:9])
    ## 'NEW FEATURES' with no items is dropped
    res <- f(lines, 3L)
    checkIdentical(as.vector(res), lines[1:2])
    checkTrue(!attr(f(lines, 7L), "truncated"))
    checkTrue(!attr(f(lines, Inf), "truncated"))
}

test_mdNEWS <- function()
{
    f <- biocViews:::.mdNEWS
    checkIdentical(f("  o nested"), "  - nested")
    checkIdentical(f("  • nested"), "  - nested")
    checkIdentical(f("<save_path>_log.txt"), "&lt;save_path>_log.txt")
    checkIdentical(f("  <https://x.org>"), "  <https://x.org>")
    checkIdentical(f("<- assignment"), "<- assignment")
}
