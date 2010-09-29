\name{getBiocViews}
\alias{getBiocViews}
\title{Build a list of BiocView objects from a package repository}
\description{
  Given the URL to a CRAN-style package repository containing a
  \code{VIEWS} file at the top-level and a \code{\link[graph]{graph-class}} object
  representing a DAG of views, this function returns a list of
  \code{\link{BiocView-class}} objects.
}
\usage{
getBiocViews(reposUrl, vocab, defaultView, local = FALSE, htmlDir = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reposUrl}{URL for a CRAN-style repository that hosts a
  \code{VIEWS} file at the top-level.}
  \item{vocab}{A \code{\link[graph]{graph-class}} object representing
    the ontology of views.  This graph should be a directed acyclic graph
    (DAG).}
  \item{defaultView}{A string giving the term to use for packages that
    do not list a term of their own via the \code{biocViews} field in the
    \file{DESCRIPTION} file.}
  \item{local}{logical indicating whether to assume a local package
  repository.  The default is \code{FALSE} in which case absolute links
  to package detail pages are created.}
  \item{htmlDir}{if the \code{local} argument is \code{TRUE}, this will
  be used as the relative path for package HTML files.}
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
biocViews <- getBiocViews(reposUrl, biocViewsVocab, "NoViewProvided")
print(biocViews[1:2])
}

\keyword{utilities}
