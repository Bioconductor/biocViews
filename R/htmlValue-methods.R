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
    text <- gsub("&([a-zA-Z0-9#]+;)", "@_@_@\\1", text)
    text <- gsub("&", "&amp;", text, fixed=TRUE)
    text <- gsub("@_@_@([a-zA-Z0-9#]+;)", "&\\1", text)
    text <- gsub("<", "&lt;", text, fixed=TRUE)
    text <- gsub(">", "&gt;", text, fixed=TRUE)
    text
}


setMethod("htmlValue", signature(object="rdPackageTable"),
          function(object) {
              dom <- xmlOutputDOM("table", attrs=c(class="repos_index"))
              odd <- TRUE
              alphaOrder <- order(tolower(names(object@packageList)))
              dom$addTag("tr", close=FALSE)
              dom$addTag("th", "Package")
              dom$addTag("th", "Maintainer")
              dom$addTag("th", "Title")
              dom$closeTag()
              for (pkg in object@packageList[alphaOrder]) {
                  rowClass <- if(odd) "row_odd" else "row_even"
                  odd <- !odd
                  dom$addTag("tr", attrs=c(class=rowClass), close=FALSE)
                  dom$addTag("td", attrs=c(class="package"), close=FALSE)
                  if (length(object@reposRoot) > 0)
                    root <- paste(object@reposRoot, object@htmlDir, sep="/")
                  else
                    root <- object@htmlDir
                  infoPage <- paste(root, htmlFilename(pkg), sep="/")
                  dom$addTag("a", attrs=c(href=infoPage), pkg@Package)
                  dom$closeTag()
                  dom$addTag("td", removeEmail(pkg@Maintainer),
                             attrs=c(class="maintainer"))
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


setMethod("htmlValue", signature(object="pdAuthorMaintainerInfo"),
          function(object) {
              dom <- xmlOutputDOM("table", attrs=c(class="author_info"))
              dom$addTag("tr", close=FALSE)
              dom$addTag("td", "Author")
              dom$addTag("td", cleanText(removeEmail(object@Author)))
              dom$closeTag()
              dom$addTag("tr", close=FALSE)
              dom$addTag("td", "Maintainer")
              dom$addTag("td", cleanText(removeEmail(object@Maintainer)))
              dom$closeTag()
              dom$value()
          })


setMethod("htmlValue", signature(object="pdVignetteInfo"),
          function(object) {
              dom <- xmlOutputDOM("table", attrs=c(class="vignette"))
              odd <- TRUE
              if (length(object@vignettes) > 0) {
                  for (vig in object@vignettes) {
                      rowClass <- if(odd) "row_odd" else "row_even"
                      odd <- !odd
                      dom$addTag("tr", attrs=c(class=rowClass), close=FALSE)
                      dom$addTag("td", close=FALSE)
                      vlink <- paste(object@reposRoot, vig, sep="/")
                      dom$addTag("a", basename(vig), attrs=c(href=vlink))
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
              flds <- c(source="source.ver",
                        win.binary="win.binary.ver",
                        mac.binary="mac.binary.ver")
              
              fileTypes <- list(source="Source", win.binary="Windows binary",
                                mac.binary="OS X binary")
              makeLinkHelper <- function(type) {
                  pkgPath <- slot(object, flds[type])
                  if (!is.na(pkgPath) && length(pkgPath) > 0 && pkgPath != "") {
                      ref <- paste(object@reposRoot, pkgPath, sep="/")
                      aTag <- xmlNode("a", basename(pkgPath), attrs=c(href=ref))
                  } else {
                      aTag <- "Not Available"
                  }
                  aTag
              }
              fileLinks <- lapply(names(fileTypes), makeLinkHelper)
              names(fileLinks) <- fileTypes
              domValue <- tableHelper(fileLinks,
                                      table.attrs=list(class="downloads"))
              domValue
          })


setMethod("htmlValue", signature(object="pdDetailsInfo"),
          function(object) {

              flds <- c("biocViews", "Depends", "Suggests", "Imports",
                        "SystemRequirements", "License", "URL", "dependsOnMe",
                        "suggestsMe")

              ## handle biocViews separately
              buildViewLink <- function(v) {
                  if (nchar(v) == 0 || !length(object@viewRoot))
                    return(v)
                  link <- paste(object@viewRoot, "/", v, ".html", sep="")
                  node <- xmlNode("a", v, attrs=c(href=link))
                  return(node)
              }
              vlinks <- lapply(object@biocViews, buildViewLink)
              args <- list(name="div")
              if (length(vlinks) > 0)
                args <- c(args, list(vlinks[[1]]))
              if (length(vlinks) > 1) {
                  for (v in vlinks[2:length(vlinks)]) {
                      args <- c(args, list(", "), list(v))
                  }
              }
              args[["attrs"]] <- c(class="views")
              views <- do.call("xmlNode", args)
              
              formatField <- function(x) paste(slot(object, x), collapse=", ")
              tableDat <- lapply(flds, formatField)
              names(tableDat) <- flds
              tableDat[["biocViews"]] <- views
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


setMethod("htmlValue", signature(object="pdVigsAndDownloads"),
          function(object) {
              dom <- xmlOutputDOM("table", attrs=c(class="vigsAndDownloads"))
              ## Create a table for Vignettes and Downloads
              dom$addTag("tr", close=FALSE)
              dom$addTag("td", xmlNode("h3", "Vignettes (Documentation)"))
              dom$addTag("td", xmlNode("h3", "Package Downloads"))
              dom$closeTag() ## tr
              
              dom$addTag("tr", close=FALSE)
              vigInfo <- as(object, "pdVignetteInfo")
              dom$addTag("td", htmlValue(vigInfo))
              downloadInfo <- as(object, "pdDownloadInfo")
              dom$addTag("td", htmlValue(downloadInfo))
              dom$closeTag() ## tr

              dom$value()
          })


setMethod("htmlValue", signature(object="PackageDetail"),
          function(object) {
              dom <- xmlOutputDOM("div", attrs=c(class="PackageDetail"))

              ## Heading
              dom$addTag("h1", object@Package)
              dom$addTag("h2", cleanText(object@Title))

              ## Description
              descInfo <- as(object, "pdDescriptionInfo")
              dom$addNode(htmlValue(descInfo))

              ## Author info
              authorInfo <- as(object, "pdAuthorMaintainerInfo")
              dom$addNode(htmlValue(authorInfo))

              ## Create a table for Vignettes and Downloads
              ## FIXME: clean this up so it is readable!
              dom$addTag("table", attrs=c(class="vigsAndDownloads"),
                         close=FALSE)
              dom$addTag("tr", close=FALSE)
              dom$addTag("td", close=FALSE)
              ## Vignettes
              dom$addTag("h3", "Vignettes (Documentation)")
              dom$closeTag()
              dom$addTag("td", close=FALSE)
              ## Download links
              dom$addTag("h3", "Package Downloads")
              dom$closeTag()
              dom$closeTag()
              
              dom$addTag("tr", close=FALSE)
              dom$addTag("td", close=FALSE)
              vigInfo <- as(object, "pdVignetteInfo")
              dom$addNode(htmlValue(vigInfo))
              dom$closeTag()
              dom$addTag("td", close=FALSE)
              downloadInfo <- as(object, "pdDownloadInfo")
              dom$addNode(htmlValue(downloadInfo))
              dom$closeTag() ## td
              dom$closeTag() ## tr
              dom$closeTag() ## table
              
              ## Details
              dom$addTag("h3", "Details")
              detailsInfo <- as(object, "pdDetailsInfo")
              dom$addNode(htmlValue(detailsInfo))

              return(dom$value())
          })


viewsHelper <- function(views) {
    dom <- xmlOutputDOM("ul")
    for (v in views) {
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
              
