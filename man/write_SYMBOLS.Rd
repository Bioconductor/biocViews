\name{write_SYMBOLS}
\alias{write_SYMBOLS}

\title{Write a SYMBOLS file}
\description{
  Writes a DCF formatted file, SYMBOLS, containing the symbols
  exported by each package in a directory containg R package
  source directories.
}
\usage{
write_SYMBOLS(dir, verbose = FALSE, source.dirs=FALSE)
}

\arguments{

\item{dir}{The root of a CRAN-style package repository containing
    source packages.  When \code{source.dirs} is \code{TRUE},
    \code{dir} should be a directory containing R package source
    directories}

\item{verbose}{Logical.  When \code{TRUE}, progress is printed to
    the standard output.}

\item{source.dirs}{Logical.  When \code{TRUE}, interpret \code{dir} as
  a directory containing source package directories.  When
  \code{FALSE}, the default, \code{dir} is assumed to be the root of a
  CRAN-style package repository and the function will operate on the
  source package tarballs in \code{dir/src/contrib}.}  

}
\value{
  Returns \code{NULL}.  Called for the side-effect of creating a
  SYMBOLS file in \code{dir}.
}
\author{S. Falcon}
\seealso{
  \code{\link{write_PACKAGES}}
  \code{\link{write_VIEWS}}
}
\keyword{utilities}
