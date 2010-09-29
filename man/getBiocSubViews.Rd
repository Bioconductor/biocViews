\name{getBiocSubViews}
\alias{getBiocSubViews}
\title{Build a list of BiocView objects from a package repository}
\description{
  This function returns a list of \code{\link{BiocView-class}} objects
  corresponding to the subgraph of the views DAG induced by
  \code{topTerm}.  In short, this does the same thing as
  \code{\link{getBiocViews}}, but limits the vocabulary to
  \code{topTerm} and all of its decendents.
}
\usage{
getBiocSubViews(reposUrl, vocab, topTerm, local = FALSE, htmlDir = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reposUrl}{URL for a CRAN-style repository that hosts a
  \code{VIEWS} file at the top-level.}
  \item{vocab}{A \code{\link[graph]{graph-class}} object representing
    the ontologyof views.  This graph should be a directed acyclic graph
    (DAG).}
  \item{topTerm}{A string giving the name of the subview DAG.  This view
    and all of its decendents will be included in the result.}
  \item{local}{logical indicating whether to assume a local package
  repository.  The default is \code{FALSE} in which case absolute links
  to package detail pages are created.}
  \item{htmlDir}{if the \code{local} argument is \code{FALSE}, this will
  be used as the relative path for package HTML files.}
}
\details{
  The root of the vocabulary DAG is implicitly included in the view
  creation process order to build views with a link back to the top.  It
  is removed from the return list.

  This function is tailored to generation of Bioconductor Task Views.
  With the current vocabulary, it probably only makes sense to call it
  with \code{topView} set to one of \code{"Software"},
  \code{"AnnotationData"}, or \code{"ExperimentData"}.  This is a hack
  to allow the biocViews code to manage HTML views across more than one
  repository.
}
 
\value{
  A list of \code{BiocView-class} objects.  The names of the list give
  the name of the corresponding view.
}
\author{Seth Falcon}

\seealso{
  \code{\link{write_VIEWS}},
  \code{\link{writeBiocViews}}
}

\examples{
data(biocViewsVocab)
reposPath <- system.file("doc", package="biocViews")
reposUrl <- paste("file://", reposPath, sep="")
biocViews <- getBiocSubViews(reposUrl, biocViewsVocab, "Software")
print(biocViews[1:2])
}

\keyword{utilities}
