\name{htmlDoc}
\alias{htmlDoc}

\title{Create a complete HTML document representation of an object}
\description{
  This generic function should return an \code{XMLNode} instance
  representing the specified object in HTML as a complete HTML document.
}
\usage{
htmlDoc(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object}
  \item{\dots}{Not currently used.}
}
\value{
  An instance of \code{XMLNode} from the \code{XML} package.
}
\author{Seth Falcon}
\seealso{\code{\link{htmlValue}}, \code{\link{htmlFilename}}}
\keyword{methods}
