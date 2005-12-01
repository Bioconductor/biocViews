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
