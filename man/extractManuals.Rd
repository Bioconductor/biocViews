\name{extractManuals}
\alias{extractManuals}
\alias{extractCitations}
\title{Generate html and pdf reference manuals from local package
  repository}
\description{
  This function generates html and pdf reference manuals
  in the \code{man} subdirectory of R source packages archives
  (\code{.tar.gz}) found in a local package repository.

  Only source package archives will be processed.  The constructed
  html and pdf files will be extracted under \code{destDir}
  and will be found in \code{PKGNAME/man/*.html} or
  \code{PKGNAME/man/*.pdf}.
}
\usage{
extractManuals(reposRoot, srcContrib, destDir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reposRoot}{character vector giving the path to the root of the
    local CRAN-style package repository, such as
    \code{$HOME/PACKAGES/3.21/data/experiment}.}
  \item{srcContrib}{character vector giving the relative path from the
    \code{reposRoot} to the source packages.  In a standard CRAN-style
    repository, this will be \code{src/contrib}.}
  \item{destDir}{character vector specifying the directory in which the
    extracted files will be written.  If missing, files will be written
    to \code{<reposRoot>/manuals}.}
}
\author{Patrick Aboyoun}

\keyword{utilities}
