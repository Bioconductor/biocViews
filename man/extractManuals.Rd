\name{extractManuals}
\alias{extractManuals}
\title{Extract Rd man pages and build pdf reference manuals from local package
  repository}
\description{
  This function extracts Rd man pages and builds pdf reference manuals
  from the \code{man} subdirectory of R source packages archives
  (\code{.tar.gz}) found in a local package repository.

  All Rd files found in \code{man} will be extracted and used during
  the pdf construction process.  Only source package archives will be
  processed.  The constructed pdf files will be extracted under
  \code{destDir} and will be found in \code{PKGNAME/man/*.pdf}.

  Prior to extraction, all Rd and pdf files in \code{destDir/PKGNAME/man}
  will be removed.
}
\usage{
extractManuals(reposRoot, srcContrib, destDir)
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
    to \code{<reposRoot>/manuals}.}
}
\author{Patrick Aboyoun}

\keyword{utilities}
