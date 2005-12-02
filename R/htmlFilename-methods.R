setMethod("htmlFilename", signature(object="character"),
          function(object) paste(object, ".html", sep=""))

setMethod("htmlFilename", signature(object="RepositoryDetail"),
          function(object) "index.html")


setMethod("htmlFilename", signature(object="BiocView"),
          function(object) {
              paste(object@name, ".html", sep="")
          })


setMethod("htmlFilename", signature(object="PackageDetail"),
          function(object) {
              htmlFilename(object@Package)
          })
