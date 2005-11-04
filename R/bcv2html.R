bcv2html <- function (x, file = NULL, css = "../../../R.css", packageURL = "../Descriptions/", 
    reposname = "CRAN") 
{
    if (is.character(x)) 
        x <- read.ctv(x)
    if (is.null(file)) 
        file <- paste(x$name, ".html", sep = "")
    ampersSub <- function(x) gsub("&", "&amp;", x)
    zpaste <- function(..., sep = "", collapse = NULL) paste(..., 
        sep = sep, collapse = collapse)
    htm1 <- c("<html>", "<head>", zpaste("  <title>", reposname, 
        " Task View: ", ampersSub(x$topic), "</title>"), zpaste("  <link rel=stylesheet type=\"text/css\" href=\"", 
        css, "\">"), "</head>", "", "<body>", zpaste("  <h2>", 
        reposname, " Task View: ", ampersSub(x$topic), "</h2>"), 
        zpaste("  <h3>Maintainer: ", ampersSub(x$maintainer), 
            "</h3>"))
    htm2 <- ampersSub(x$info)
### BAD!!!!  implies installation
    titles <- sapply(x$packagelist[, 1], packageDescription, 
        field = "Title")
    titles <- paste("[", titles, "]", sep = "")
    pkg2html <- function(a, b, tt) zpaste("    <li><a href=\"", 
        packageURL, a, ".html\">", a, "</a>", " ", tt, if (b) 
            " (core)"
        else "", "</li>")
    htm3 <- c(zpaste("  <h3>", reposname, " packages:</h3>"), 
        "  <ul>", sapply(1:NROW(x$packagelist), function(i) pkg2html(x$packagelist[i, 
            1], x$packagelist[i, 2], titles[i])), "  </ul>")
    htm4 <- c("  <h3>Related links:</h3>", "  <ul>", sapply(x$links, 
        function(x) paste("    <li>", ampersSub(x), "</li>", 
            sep = "")), "  </ul>")
    htm <- c(htm1, "", htm2, "", htm3, "", htm4, "", "</body>", 
        "</html>")
    writeLines(htm, con = file)
    invisible(htm)
}
