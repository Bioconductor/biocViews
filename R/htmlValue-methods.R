writeHtmlDoc <- function(html, file)
{
    ## Temporary fix: we open and close 'file' here instead of passing it
    ## directly to saveXML because of a bug in current XML::saveXML
    ## (from XML 1.3-2). Bug reported to XML's author on 2006-12-14. Herve.
    f <- file(file, open="w")
    ## another temp fix: write the DOCTYPE header here, perhaps we should
    ## use prefix for this in the call to saveXML?
    writeLines(paste('<!DOCTYPE html PUBLIC',
                     '"-//W3C//DTD XHTML 1.0 Strict//EN"',
                     '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'),
               con=f)
    saveXML(html, f, prefix="")
    close(f)
}


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
              rowClass <- "row_odd"
              if (length(object@vignettes) > 0) {
                  vignetteTitles <-
                    ifelse(nzchar(object@vignetteTitles), object@vignetteTitles,
                           basename(object@vignettes))
                  for (i in order(vignetteTitles)) {
                      rowClass <- if(odd) "row_odd" else "row_even"
                      dom$addTag("tr", attrs=c(class=rowClass), close=FALSE)
                      dom$addTag("th", vignetteTitles[i])
                      dom$addTag("td", close=FALSE)
                      pdflink <-
                        paste(object@reposRoot, object@vignettes[i], sep="/")
                      dom$addTag("a", "PDF", attrs=c(href=pdflink))
                      dom$closeTag()
                      if (nchar(object@vignetteScripts[i]) > 0) {
                          dom$addTag("td", close=FALSE)
                          Rlink <-
                            paste(object@reposRoot, object@vignetteScripts[i],
                                  sep="/")
                          dom$addTag("a", "R Script", attrs=c(href=Rlink))
                          dom$closeTag()
                      }
                      dom$closeTag() ## end tr
                      odd <- !odd
                  }
              } else {
                  dom$addTag("tr", attrs=c(class=rowClass), close=FALSE)
                  dom$addTag("td", "No vignettes available")
                  dom$closeTag()
                  odd <- !odd
              }
              rowClass <- if(odd) "row_odd" else "row_even"
              if (length(object@manuals) > 0 && !is.na(object@manuals[1])) {
                  dom$addTag("tr", attrs=c(class=rowClass), close=FALSE)
                  dom$addTag("td", close=FALSE)
                  mlink <- paste(object@reposRoot, object@manuals[1], sep="/")
                  dom$addTag("a", "Reference Manual", attrs=c(href=mlink))
                  dom$closeTag()
                  dom$closeTag() ## end tr
                  odd <- !odd
              } else {
                  dom$addTag("tr", attrs=c(class=rowClass), close=FALSE)
                  dom$addTag("td", "No reference manual available")
                  dom$closeTag()
                  odd <- !odd
              }
              dom$value()
          })


setMethod("htmlValue", signature(object="pdDownloadInfo"),
          function(object) {
              flds <- c(source="source.ver",
                        win.binary="win.binary.ver",
                        mac.binary="mac.binary.ver",
                        mac.binary.mavericks="mac.binary.mavericks.ver",
                        `mac.binary.el-capitan`="mac.binary.el-capitan.ver")

              fileTypes <- list(source="Package source",
                                win.binary="Windows 32-bit binary",
                                mac.binary="macOS 10.13 (High Sierra) binary",
                                mac.binary.mavericks="MacOS X 10.9 (Mavericks) binary",
                                `mac.binary.el-capitan`="MacOS X 10.11 (El Capitan) binary")
              makeLinkHelper <- function(type) {


                  isAvailable = TRUE
                  archs <- slot(object, "Archs")
                  if (length(archs) > 0 && nchar(archs) > 0) {
                    if (type == "win.binary") {
                      if (length(grep("i386", archs, value=TRUE)) == 0) {
                        isAvailable = FALSE
                      }
                    }
                  }


                  pkgPath <- slot(object, flds[type])
                  if (isAvailable && !is.na(pkgPath) && length(pkgPath) > 0 && pkgPath != "") {
                      ref <- paste(object@reposRoot, pkgPath, sep="/")
                      aTag <- xmlNode("a", basename(pkgPath), attrs=c(href=ref))
                  } else {
                      aTag <- "Not Available"
                  }
                  aTag
              }
              fileLinks <- lapply(names(fileTypes), makeLinkHelper)
              names(fileLinks) <- fileTypes
              downloadStatsUrl <- slot(object, "downloadStatsUrl")
              if ((length(downloadStatsUrl) == 1) &&
                  (nchar(downloadStatsUrl) > 0)) {
                  fileLinks <- c(fileLinks,
                                 list("Package Downloads Report" =
                                      xmlNode("a", "Downloads Stats",
                                              attrs=c(href=paste(downloadStatsUrl, "/",
                                                                 slot(object, "Package"),
                                                                 ".html", sep="")))))
              }
              domValue <- tableHelper(fileLinks,
                                      table.attrs=list(class="downloads"))
              domValue
          })


setMethod("htmlValue", signature(object="pdDetailsInfo"),
          function(object) {
              ## link generating functions
              buildLinks <- function(x, root, class, check = FALSE) {
                  nodes <-
                    lapply(x,
                           function(y) {
                               urlError <- FALSE
                               if (nchar(y) == 0 || length(root) == 0) {
                                   urlError <- TRUE
                               } else {
                                   if (check) {
                                       oldWarn <- options()[["warn"]]
                                       options(warn = -1)
                                       for (i in seq_len(length(root))) {
                                           link <- paste(root[i], "/", y, ".html", sep="")
                                           con <- try(url(link, "r"), silent = TRUE)
                                           if (class(con)[[1]] != "try-error")
                                               break;
                                       }
                                       options(warn = oldWarn)
                                       if (class(con)[[1]] == "try-error") {
                                           urlError <- TRUE
                                       } else {
                                           close(con)
                                       }
                                   } else {
                                       link <- paste(root[1], "/", y, ".html", sep="")
                                   }
                               }
                               if (urlError) {
                                   node <- y
                               } else {
                                   node <- xmlNode("a", y, attrs=c(href=link))
                               }
                               return(node)
                           })
                  if (length(nodes) == 0) {
                      args <- list()
                  } else if (length(nodes) == 1) {
                      args <- nodes
                  } else {
                      args <- vector("list", 2*length(nodes) - 1)
                      args[seq(1, 2*length(nodes) - 1, by = 2)] <- nodes
                      args[seq(2, 2*(length(nodes) - 1), by = 2)] <- list(", ")
                  }
                  args <- c(list(name = "div"), args, list(attrs = c(class=class)))
                  return(do.call(xmlNode, args))
              }
              buildViewLinks <- function(x) buildLinks(x, object@viewRoot, class="views")
              buildPkgLinks <- function(x)
                buildLinks(x, paste(object@reposFullUrl, "/html", sep=""),
                           class="packages", check=TRUE)
              buildURLLink <- function(u) {
                  if (!length(u) || nchar(u) == 0)
                      node <- ""
                  else
                      node <- xmlNode("a", u, attrs=c(href=u))
                  return(node)
              }

              ## create list elements for fields
              flds <- c("biocViews"="biocViews", "Depends"="Depends",
                        "Imports"="Imports", "Suggests"="Suggests",
                        "System Requirements"="SystemRequirements",
                        "License"="License", "URL"="URL",
                        "Depends On Me"="dependsOnMe",
                        "Imports Me"="importsMe",
                        "Suggests Me"="suggestsMe",
                        "Development History"="devHistoryUrl")
              tableDat <- vector("list", length = length(flds))
              names(tableDat) <- flds

              ## add biocViews info
              tableDat[["biocViews"]] <- buildViewLinks(object@biocViews)

              ## add Depends, Imports, Suggests, dependsOnMe, importsMe, suggestsMe
              pkgFlds <-
                c("Depends", "Imports", "Suggests",
                  "dependsOnMe", "importsMe", "suggestsMe")
              tableDat[pkgFlds] <-
                lapply(pkgFlds, function(x) buildPkgLinks(slot(object, x)))

              ## add SystemRequirements and License info
              otherFlds <- c("SystemRequirements", "License")
              tableDat[otherFlds] <-
                lapply(otherFlds, function(x) paste(slot(object, x), collapse=", "))

              ## add URL info
              tableDat[["URL"]] <- buildURLLink(object@URL)

              ## add development history
              devHistoryUrl <- object@devHistoryUrl
              if ((length(devHistoryUrl) == 1) &&
                  (nchar(devHistoryUrl) > 0)) {
                  tableDat[["devHistoryUrl"]] <-
                    xmlNode("a", "Bioconductor Changelog",
                            attrs=c(href=paste(devHistoryUrl, "/",
                                               object@Package, sep="")))
              } else {
                  flds <- flds[- match("devHistoryUrl", flds)]
                  tableDat[["devHistoryUrl"]] <- NULL
              }

              ## rename rows
              names(tableDat) <- names(flds)

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
              dom$addTag("h1", object@Package)
              dom$addTag("h2", cleanText(object@Title))

              ## Description
              descInfo <- as(object, "pdDescriptionInfo")
              dom$addNode(htmlValue(descInfo))

              ## Author info
              authorInfo <- as(object, "pdAuthorMaintainerInfo")
              dom$addNode(htmlValue(authorInfo))

              ## Installation Instructions
              dom$addTag("div", attrs=c(class="installInstruct"), close=FALSE)
              dom$addTag("p", paste("To install this package,",
                                    "start R and enter:"),
                         attrs=c(class="install"))
              dom$addTag("pre",
                         paste("  if (!require(\"BiocManager\"))",
                               "\n      install.packages(\"BiocManager\")",
                               "\n  BiocManager::install(\"",
                               object@Package,
                               "\")", sep="")
                         )
              dom$closeTag() # div

              ## Documentation
              dom$addTag("h3", "Documentation")
              vigInfo <- as(object, "pdVignetteInfo")
              dom$addNode(htmlValue(vigInfo))

              ## Details
              dom$addTag("h3", "Details")
              detailsInfo <- as(object, "pdDetailsInfo")
              dom$addNode(htmlValue(detailsInfo))

              ## Package Downloads
              dom$addTag("h3", "Package Downloads")
              downloadInfo <- as(object, "pdDownloadInfo")
              dom$addNode(htmlValue(downloadInfo))

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

