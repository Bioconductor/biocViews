makeHtmlHeader <- function(title, stylesheet) {
## Right now xmlTree's addNode method doesn't accept XMLNode objects
##     html <- xmlTree("html",
##                     attrs=c(xmlns="http://www.w3.org/1999/xhtml",
##                       "xml:lang"="en", lang="en"),
##                     dtd=c('html',
##                          '-//W3C//DTD XHTML 1.0 Strict//EN',
##                          'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'))
    html <- xmlOutputDOM("html",
                    attrs=c(xmlns="http://www.w3.org/1999/xhtml",
                      "xml:lang"="en", lang="en"))
### gaah! header is only supported by xmlOutputBuffer ! :-(
### so instead we write out the DOCTYPE in the writeDoc method.
##                     header=paste('<!DOCTYPE html PUBLIC',
##                          '"-//W3C//DTD XHTML 1.0 Strict//EN"',
##                          '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'))
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
          function(object, ..., title, stylesheet="style.css") {
              dom <- makeHtmlHeader(title, stylesheet)
              dom$addTag("body", close=FALSE)
              dom$addNode(htmlValue(object))
              dom$closeTag()
              dom$value()
          })


setMethod("htmlDoc", signature(object="PackageDetail"),
          function(object, ...) {
              title <- object@Package
              stylesheet="package-detail.css"
              callNextMethod(object=object, title=title,
                             stylesheet=stylesheet)
          })


setMethod("htmlDoc", signature(object="RepositoryDetail"),
          function(object, ...) {
              title <- object@Title
              stylesheet="repository-detail.css"
              callNextMethod(object=object, title=title,
                             stylesheet=stylesheet)
          })


setMethod("htmlDoc", signature(object="BiocView"),
          function(object, ...) {
              title <- paste("Bioconductor Task View", object@name)
              sylesheet="repository-detail.css"
              callNextMethod(object=object, title=title,
                             stylesheet=stylesheet)
          })
