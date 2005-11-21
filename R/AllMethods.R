makeHtmlHeader <- function(title, stylesheet) {
    html <- xmlOutputDOM("html")
    html$addTag("head", close=FALSE)
    html$addTag("title", title)
    myAttrs <- c(rel="stylesheet",
                 type="text/css",
                 href=stylesheet)
    html$addTag("link", attrs=myAttrs)
    html$closeTag()
    html
}


setMethod("htmlDoc", signature(object="Htmlized"),
          function(object, title, stylesheet="style.css") {
              dom <- makeHtmlHeader(title, stylesheet)
              dom$addTag("body", close=FALSE)
              dom$addNode(htmlValue(object))
              dom$closeTag()
              dom$value()
          })


setMethod("htmlDoc", signature(object="PackageDetail"),
          function(object) {
              title <- paste(object@Package, object@Version)
              stylesheet="package-detail.css"
              callNextMethod(object, title, stylesheet)
          })


setMethod("htmlDoc", signature(object="RepositoryDetail"),
          function(object) {
              title <- object@Title
              stylesheet="repository-detail.css"
              callNextMethod(object, title, stylesheet)
          })


setMethod("htmlDoc", signature(object="BiocView"),
          function(object) {
              title <- paste("Bioconductor Task View", object@name)
              sylesheet="repository-detail.css"
              callNextMethod(object, title, stylesheet)
          })


setMethod("htmlFilename", signature(object="character"),
          function(object) paste(object, ".html", sep=""))

setMethod("htmlFilename", signature(object="RepositoryDetail"),
          function(object) "index.html")


setMethod("htmlFilename", signature(object="BiocView"),
          function(object) {
              if (object@name == "vocRoot")
                "index.html"
              else
                paste(object@name, ".html", sep="")
          })


writeHtmlDoc <- function(html, file) saveXML(html, file)


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


setMethod("htmlValue", signature(object="rdPackageTable"),
          function(object) {
              dom <- xmlOutputDOM("table", attrs=c(class="repos_index"))
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
              dom$value()
          })


setMethod("htmlValue", signature(object="RepositoryDetail"),
          function(object) {
              dom <- xmlOutputDOM("div", attrs=c(class="RepositoryDetail"))
              
              dom$addTag("h1", cleanText(object@Title))
              ## Package table
              pkgTable <- as(object, "rdPackageTable")
              dom$addNode(htmlValue(pkgTable))
              dom$value()
          })


setMethod("htmlFilename", signature(object="PackageDetail"),
          function(object) {
              htmlFilename(object@Package)
          })


setMethod("htmlValue", signature(object="pdAuthorMaintainerInfo"),
          function(object) {
              dom <- xmlOutputDOM("dl", attrs=c(class="author_info"))
              dom$addTag("dt", "Author")
              dom$addTag("dd", cleanText(object@Author))
              dom$addTag("dt", "Maintainer")
              dom$addTag("dd", cleanText(object@Maintainer))
              dom$closeTag()
              dom$value()
          })


setMethod("htmlValue", signature(object="pdVignetteInfo"),
          function(object) {
              dom <- xmlOutputDOM("table", attrs=c(class="vignette"))
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
          })


setMethod("htmlValue", signature(object="pdDownloadInfo"),
          function(object) {
              fileTypes <- list(source="source", win.binary="Windows",
                                mac.binary="OS X")
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
              domValue <- tableHelper(fileLinks,
                                      table.attrs=list(class="downloads"))
              domValue
          })


setMethod("htmlValue", signature(object="pdDetailsInfo"),
          function(object) {

              flds <- c("Depends", "Suggests", "Imports", "SystemRequirements",
                        "License", "URL", "biocViews", "dependsOnMe",
                        "suggestsMe")
              formatField <- function(x) paste(slot(object, x), collapse=", ")
              tableDat <- lapply(flds, formatField)
              names(tableDat) <- flds
              domValue <- tableHelper(tableDat,
                                      table.attrs=list(class="details"))
              domValue
          })


setMethod("htmlValue", signature(object="pdDescriptionInfo"),
          function(object) {
              node <- xmlNode("p", cleanText(object@Description),
                              attrs=c(class="description"))
              node
          })


setMethod("htmlValue", signature(object="PackageDetail"),
          function(object) {
              dom <- xmlOutputDOM("div", attrs=c(class="PackageDetail"))

              ## Heading
              dom$addTag("h1", paste(object@Package, object@Version))
              dom$addTag("h2", cleanText(object@Title))

              ## Author info
              authorInfo <- as(object, "pdAuthorMaintainerInfo")
              dom$addNode(htmlValue(authorInfo))

              ## Description
              descInfo <- as(object, "pdDescriptionInfo")
              dom$addNode(htmlValue(descInfo))

              ## Details
              dom$addTag("h3", "Details")
              detailsInfo <- as(object, "pdDetailsInfo")
              dom$addNode(htmlValue(detailsInfo))

              ## Download links
              dom$addTag("h3", "Download Package")
              downloadInfo <- as(object, "pdDownloadInfo")
              dom$addNode(htmlValue(downloadInfo))
              
              ## Vignettes
              dom$addTag("h3", "Vignettes (Documentation)")
              vigInfo <- as(object, "pdVignetteInfo")
              dom$addNode(htmlValue(vigInfo))

              return(dom$value())
          })


setAs("BiocView", "rdPackageTable", function(from) {
    as(as(from, "RepositoryDetail"), "rdPackageTable")
    })


setMethod("show", signature(object="BiocView"),
          function(object) {
              cat("Bioconductor View:", object@name, "\n")
              cat("Parent Views:\n")
              print(object@parentViews)
              cat("Subviews:\n")
              print(object@subViews)
              cat("Contains packages:\n")
              print(names(object@packageList))

          })


viewsHelper <- function(views) {
    dom <- xmlOutputDOM("ul")
    for (v in views) {
        ## FIXME: this should be done in one place
        if (v == "vocRoot")
          link <- "index.html"
        else
          link <- htmlFilename(v)
        dom$addTag("li", close=FALSE)
        dom$addTag("a", v, attrs=c(href=link))
        dom$closeTag()
    }
    dom$value()
}


setMethod("htmlValue", signature(object="bvSubViews"),
          function(object) {
              dom <- xmlOutputDOM("div", attrs=c(class="bv_subviews"))
              dom$addTag("h2", "Subviews")
              dom$addNode(viewsHelper(object@subViews))
              dom$value()
          })


setMethod("htmlValue", signature(object="bvParentViews"),
          function(object) {
              dom <- xmlOutputDOM("div", attrs=c(class="bv_parentviews"))
              dom$addTag("h2", "Subview of")
              dom$addNode(viewsHelper(object@parentViews))
              dom$value()
          })


setMethod("htmlValue", signature(object="BiocView"),
          function(object) {
              dom <- xmlOutputDOM("div", attrs=c(class="BiocView"))

              ## Heading
              dom$addTag("h1", paste("Bioconductor Task View:", object@name))
              
              ## Parent Views
              if (length(object@parentViews) > 0) {
                  parentViews <- as(object, "bvParentViews")
                  dom$addNode(htmlValue(parentViews))
              }

              ## Subviews
              if (length(object@subViews) > 0) {
                  subViews <- as(object, "bvSubViews")
                  dom$addNode(htmlValue(subViews))
              }

              dom$addTag("h2", "Packages in view")
              if (length(object@packageList) > 0) {
                  pkgTable <- as(object, "rdPackageTable")
                  dom$addNode(htmlValue(pkgTable))
              } else {
                  dom$addTag("p", "No packages in this view")
              }

              dom$value()
          })
              

              
