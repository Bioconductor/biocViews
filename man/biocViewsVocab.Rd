\name{biocViewsVocab}
\alias{biocViewsVocab}
\docType{data}
\title{Bioconductor Task Views Vocabulary Data}
\description{
  A \code{\link[graph]{graphNEL-class}} instance representing the Bioconductor
  Task Views as a directed graph.
}
\usage{data(biocViewsVocab)}
\format{
  The format is: graphNEL instance
}
\details{
  The source for the vocabulary data is in the dot directory of the
  package in file biocViewsVocab.dot.  This is transformed to GXL using
  the dot2gxl command line utility from the graphviz package.  Then the
  \code{fromGXL} function from the \code{graph} package is used to
  convert to \code{graphNEL-class}.
}
\examples{
data(biocViewsVocab)
biocViewsVocab
## If you have Rgraphviz available, you can
## plot the vocabulary with plot(biocViewsVocab)
}
\keyword{datasets}
