\name{writeBiocViews}
\alias{writeBiocViews}
\title{Write a list of BiocView objects to HTML}
\description{
  This function serializes a list of \code{\link{BiocView-class}}
  objects to a series of HTML files.
}
\usage{
writeBiocViews(bvList, dir, backgroundColor="transparent")
}
\arguments{
  \item{bvList}{A list of \code{BiocView-class} objects}
  \item{dir}{A character vector giving the directory where the HTML
    files will be written.}
  \item{backgroundColor}{A character vector giving the background color
    for the body in the CSS file.}
}
\author{Seth Falcon}

\seealso{
  \code{\link{getBiocViews}},
  \code{\link{genReposControlFiles}},
  \code{\link{write_VIEWS}}
}

\keyword{utilities}
