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
    win.binary: bin/windows/contrib/2.3
    provides: source, win.binary
  }
}
\usage{
write_REPOSITORY(reposRootPath, contribPaths)
}
\arguments{
  \item{reposRootPath}{character vector containing the path to the
  CRAN-style repository root directory.}
  \item{contribPaths}{A named character vector.  Valid names are
    \code{source}, \code{win.binary}, \code{mac.binary},
    \code{mac.binary.universal}, and \code{mac.binary.leopard}.
    Values indicate the paths to the package archives relative to
    the \code{reposRoot}.}
}
\author{Seth Falcon}

\seealso{
  \code{\link[tools]{write_PACKAGES}},
  \code{\link{extractVignettes}},
  \code{\link{genReposControlFiles}},
  \code{\link{write_VIEWS}}
}
\keyword{utilities}

