setMethod("htmlValue", signature(object="Htmlized"),
          function(object) object@.htmlDom$value())


setReplaceMethod("htmlHeader",
                 signature(object="Htmlized", value="character"),
                 function(object, value) {
                     value <- as.list(value)
                     htmlHeader(object) <- value
                     object
                 })

setReplaceMethod("htmlHeader", signature(object="Htmlized", value="list"),
                 function(object, value) {
                     attrs <- value
                     html <- xmlOutputDOM("html")
                     html$addTag("head", close=FALSE)
                     html$addTag("title", attrs$title)
                     myAttrs <- c(rel="stylesheet",
                                   type="text/css",
                                   href=attrs$css)
                     html$addTag("link", attrs=myAttrs)
                     html$closeTag()
                     html$addTag("body", close=FALSE)
                     object@.htmlDom <- html
                     object
                 })


setReplaceMethod("htmlFooter", signature(object="Htmlized", value="character"),
                 function(object, value) {
                     html <- object@.htmlDom
                     html$closeTag() ## close body tag
                     object@.htmlDom <- html
                     object
                 })


setMethod("writeHtml", signature(object="Htmlized"),
          function(object, file) {
              if (missing(file))
                file <- htmlFilename(object)
              saveXML(htmlValue(object), file)
          })


tableHelper <- function(tableData, table.attrs) {
    dom <- xmlOutputDOM("table", attrs=table.attrs)
    odd <- TRUE
    for (fld in names(tableData)) {
        rowClass <- if(odd) "row_odd" else "row_even"
        odd <- !odd
        dom$addTag("tr", attrs=c(class=rowClass), close=FALSE)
        dom$addTag("th", fld)
        dom$addTag("td", tableData[[fld]])
        dom$closeTag()
    }
    dom$closeTag() ## end details table
    dom$value()
}


cleanText <- function(text) {
    text <- gsub("&", "&amp;", text, fixed=TRUE)
    text <- gsub("<", "&lt;", text, fixed=TRUE)
    text <- gsub(">", "&gt;", text, fixed=TRUE)
    text
}
 

setMethod("htmlFilename", signature(object="RepositoryDetail"),
          function(object) "index.html")


setMethod("htmlValue", signature(object="RepositoryDetail"),
          function(object) {
              htmlHeader(object) <- list(title=object@Title,
                                        css="repository-detail.css")
              dom <- object@.htmlDom
              dom$addTag("h1", cleanText(object@Title))

              ## Package table
              dom$addTag("table", attrs=c(class="repos_index"), close=FALSE)
              odd <- TRUE
              alphaOrder <- order(tolower(names(object@packageList)))
              for (pkg in object@packageList[alphaOrder]) {
                  rowClass <- if(odd) "row_odd" else "row_even"
                  odd <- !odd
                  dom$addTag("tr", attrs=c(class=rowClass), close=FALSE)
                  dom$addTag("td", attrs=c(class="package"), close=FALSE)
                  infoPage <- paste(object@htmlDir, htmlFilename(pkg), sep="/")
                  dom$addTag("a", attrs=c(href=infoPage), pkg@Package)
                  dom$closeTag()
                  dom$addTag("td", pkg@Version, attrs=c(class="version"))
                  dom$addTag("td", pkg@Title, attrs=c(class="title"))
                  dom$closeTag() ## end tr
              }
              dom$closeTag() ## end package table
              object@.htmlDom <- dom
              htmlFooter(object) <- ""
              callNextMethod()
          })


setMethod("htmlFilename", signature(object="PackageDetail"),
          function(object) {
              paste(object@Package, "html", sep=".")
          })


htmlAuthorInfo <- function(object, css.class) {
    ## Generate chunk of HTML (a dl) given a PackageDetail instance
    ## Return the xmlNode structure
    dom <- xmlOutputDOM("dl", attrs=c(class=css.class))
    dom$addTag("dt", "Author")
    dom$addTag("dd", cleanText(object@Author))
    dom$addTag("dt", "Maintainer")
    dom$addTag("dd", cleanText(object@Maintainer))
    dom$closeTag()
    dom$value()
}


htmlVignetteInfo <- function(object, css.class) {
    ## Generate chunk of HTML (a table) given a PackageDetail instance
    ## Return the xmlNode structure
    dom <- xmlOutputDOM("table", attrs=c(class=css.class))
    odd <- TRUE
    if (length(object@vignetteLinks) > 0) {
        for (vig in object@vignetteLinks) {
            rowClass <- if(odd) "row_odd" else "row_even"
            odd <- !odd
            dom$addTag("tr", attrs=c(class=rowClass), close=FALSE)
            dom$addTag("td", close=FALSE)
            dom$addTag("a", basename(vig), attrs=c(href=vig))
            dom$closeTag()
            dom$closeTag() ## end tr
        }
    } else {
        dom$addTag("tr", attrs=c(class="row_odd"), close=FALSE)
        dom$addTag("td", "No vignettes available")
        dom$closeTag()
    }
    dom$value()
}


htmlDownloadInfo <- function(object, css.class) {
    ## Generate chunk of HTML (a table) given a PackageDetail instance
    ## Return the xmlNode structure
    fileTypes <- list(source="source", win.binary="Windows", mac.binary="OS X")
    nms <- names(object@downloadLinks)
    makeLinkHelper <- function(type) {
        pos <- match(type, nms)
        if (!is.na(pos)) {
            f <- object@downloadLinks[pos]
            ref <- paste("..", f, sep="/")
            aTag <- xmlNode("a", basename(f), attrs=c(href=ref))
        } else {
            aTag <- "Not Available"
        }
        aTag
    }
    fileLinks <- lapply(fileTypes, makeLinkHelper)
    names(fileLinks) <- fileTypes
    domValue <- tableHelper(fileLinks, table.attrs=list(class=css.class))
    domValue
}


htmlDetailsInfo <- function(object, css.class) {
    flds <- c("Depends", "Suggests", "Imports", "SystemRequirements",
              "License", "URL", "biocViews", "dependsOnMe",
              "suggestsMe")
    formatField <- function(x) paste(slot(object, x), collapse=", ")
    tableDat <- lapply(flds, formatField)
    names(tableDat) <- flds
    domValue <- tableHelper(tableDat, table.attrs=list(class=css.class))
    domValue
}


setMethod("htmlValue", signature(object="PackageDetail"),
          function(object) {
              ## TODO: factor out some duplication
              ## to make this a reasonably sized method
              htmlHeader(object) <- c(title=object@Title,
                                      css="package-detail.css")

              dom <- object@.htmlDom
              
              ## Heading
              dom$addTag("h1", paste(object@Package, object@Version))
              dom$addTag("h2", cleanText(object@Title))

              ## Author info
              dom$addNode(htmlAuthorInfo(object, css.class="author_info"))

              ## Description
              dom$addTag("p", cleanText(object@Description),
                                     attrs=c(class="description"))

              ## Details
              dom$addTag("h3", "Details")
              dom$addNode(htmlDetailsInfo(object, css.class="details"))

              ## Download links
              dom$addTag("h3", "Download Package")
              dom$addNode(htmlDownloadInfo(object, css.class="downloads"))
              
              ## Vignettes
              dom$addTag("h3", "Vignettes (Documentation)")
              dom$addNode(htmlVignetteInfo(object, css.class="vignette"))
              
              object@.htmlDom <- dom
              htmlFooter(object) <- ""
              callNextMethod()
          })
