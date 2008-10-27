\name{genReposControlFiles}
\alias{genReposControlFiles}
\title{Generate CRAN-style repository control files}
\description{
  This function generates control files for CRAN-style repositories.
  For each path specified in \code{contribPaths} a \code{PACKAGES} file
  is written.  In addition, two top-level control files are created:

  \code{REPOSITORY} contains information about the specified contrib
  paths.

  \code{VIEWS} contains metadata for all packages in the repository
  including the paths to any extracted vignettes, if found.  This file
  is useful for generating HTML views of the repository.
}
\usage{
genReposControlFiles(reposRoot, contribPaths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reposRoot}{character vector containing the path to the
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
  \code{\link{write_REPOSITORY}},
  \code{\link{write_VIEWS}}
}
\keyword{utilities}

