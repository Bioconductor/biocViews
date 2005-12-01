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
              title <- object@Package
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
