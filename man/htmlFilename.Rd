\name{htmlFilename}
\alias{htmlFilename}
\alias{htmlFilename,character-method}

\title{Return a filename for an object's HTML representation}
\description{
  This function returns a string containing an appropriate filename for
  storing the object's HTML representation.
}
\usage{
htmlFilename(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object.}
  \item{\dots}{Not currently used}
}
\value{
  A character vector of length one containing the filename.
}
\author{Seth Falcon}
\seealso{\code{\link{htmlValue}}, \code{\link{htmlDoc}} }
\keyword{methods}
