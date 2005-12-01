\name{extractVignettes}
\alias{extractVignettes}
\title{Extract pdf vignettes from local package repository}
\description{
  This function extracts pdf files from the \code{inst/doc} subdirectory
  of R source packages archives (\code{.tar.gz}) found in a local
  package repository.

  All pdf files found in \code{inst/doc} will be extracted.  Only source
  package archives will be processed.  The extracted pdf files will be
  extracted under \code{destDir} and will be found in
  \code{PKGNAME/inst/doc/*.pdf}.
}
\usage{
extractVignettes(reposRoot, srcContrib, destDir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reposRoot}{character vector giving the path to the root of the
    local CRAN-style package repository}
  \item{srcContrib}{character vector giving the relative path from the
    \code{reposRoot} to the source packages.  In a standard CRAN-style
    repository, this will be \code{src/contrib}.}
  \item{destDir}{character vector specifying the directory in which the
    extracted files will be written.  If missing, files will be written
    to \code{<reposRoot>/vignettes}.}
}
\author{Seth Falcon}

\keyword{utilities}

