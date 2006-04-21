\name{getPacksAndViews}
\alias{getPacksAndViews}
\alias{permulist}
%Undocumented code objects:
%  makeVocInfo pump tellSubTop tellSuperTop
%Undocumented data sets:
%  gg oct05
\alias{makeVocInfo}
\alias{pump}
\alias{tellSubTop}
\alias{tellSuperTop}
\title{Parse VIEWS file for views and packages}
\description{
  Given a repository URL, download and parse the VIEWS file.
}

\usage{
getPacksAndViews(reposURL, vocab, defaultView, local=FALSE)
}
\arguments{
  \item{reposURL}{character vector giving the URL of a CRAN-style
    repository containing a VIEWS file at the top-level.}
  \item{vocab}{A \code{\link[graph]{graph-class}} object representing
    the ontologyof views.  This graph should be a directed acyclic graph
    (DAG).}
  \item{defaultView}{A string giving the term to use for packages that
    do not list a term of their own via the \code{biocViews} field in the
    \file{DESCRIPTION} file.}
  \item{local}{logical indicating whether certain links should be
    absolute (using \code{reposURL}) or relative.}
}
\value{
  A list with named elements:

  \code{views}: Vector of view memberships.  Names are package names.

  \code{pkgList}: A list of \code{\link{PackageDetail-class}} objects.
}
\author{Seth Falcon}

\keyword{utilities}
