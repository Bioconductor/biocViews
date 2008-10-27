\name{write_VIEWS}
\alias{write_VIEWS}
\title{Write a VIEWS control file for a CRAN-style package repository}
\description{
  This function writes a \code{VIEWS} file to the top-level of a
  CRAN-style package repository.  The \code{VIEWS} file is in DCF format
  and describes all packages found in the repository.

  The \code{VIEWS} file contains the complete \code{DESCRIPTION} file
  for each source package in the repository.  In addition, metadata for
  available binary packages and vignettes is centralized here.
}
\usage{
write_VIEWS(reposRootPath, fields = NULL,
            type = c("source", "mac.binary", "mac.binary.universal", "mac.binary.leopard", "win.binary"),
            verbose = FALSE, vignette.dir = "vignettes")
}
\arguments{
  \item{reposRootPath}{character vector containing the path to the
    CRAN-style repository root directory.}
  \item{fields}{Any additional fields to include.  You shouldn't need
    this, but if you have added fields to the DESCRIPTION files of the
    packages in the repository, you may want it.}
  \item{type}{One of \code{source}, \code{mac.binary},
    \code{mac.binary.universal}, \code{mac.binary.leopard},
    or \code{win.binary} indicating which set of packages should be used to
    build up the "shared" information.  Since a repository can contain
    different versions of the same package (source vs binary) the shared
    information may be a lie.}
  \item{verbose}{logical, if \code{TRUE}, print progress messages.}
  \item{vignette.dir}{character specifying where to look for vignettes.}
}
\author{Seth Falcon}
\section{Warning}{
  This function uses a private function from the \code{tools} package:
  \code{tools:::.build_repository_package_db}.
}

\seealso{
  \code{\link[tools]{write_PACKAGES}},
  \code{\link{extractVignettes}},
  \code{\link{genReposControlFiles}},
  \code{\link{write_REPOSITORY}}
}

\keyword{utilities}
