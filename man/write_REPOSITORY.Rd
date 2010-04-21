\name{write_REPOSITORY}
\alias{write_REPOSITORY}
\title{Write a REPOSITORY control file for a CRAN-style package repository}
\description{
  This function writes a \code{REPOSITORY} file at the top-level of a
  CRAN-style repository.  This file is DCF formatted and describes the
  location of packages available in the repository.  Here is an example
  for a repository containing only source and Windows binary packages:

  \preformatted{
    source: src/contrib
    win.binary: bin/windows/contrib/2.6
    win64.binary: bin/windows64/contrib/2.6
    mac.binary.leopard: bin/mac/leopard/contrib/2.6
    provides: source, win.binary, win64.binary, mac.binary.leopard
  }
}
\usage{
write_REPOSITORY(reposRootPath, contribPaths)
}
\arguments{
  \item{reposRootPath}{character vector containing the path to the
  CRAN-style repository root directory.}
  \item{contribPaths}{A named character vector.  Valid names are
    \code{source}, \code{win.binary}, \code{win64.binary},
    \code{mac.binary}, and \code{mac.binary.leopard}.
    Values indicate the paths to the package archives relative to
    the \code{reposRoot}.}
}
\author{Seth Falcon}

\seealso{
  \code{\link[tools:writePACKAGES]{write_PACKAGES}},
  \code{\link{extractVignettes}},
  \code{\link{genReposControlFiles}},
  \code{\link{write_VIEWS}}
}
\keyword{utilities}

