\name{bcv2html}
\alias{bcv2html}
\title{serialize a cran task view to HTML, with bioconductor extension  }
\description{serialize a cran task view to HTML, with bioconductor extension
}
\usage{
bcv2html(x, file=NULL, css="../../../R.css", packageURL="../Descriptions/", reposname="CRAN")
}
\arguments{
  \item{x}{ x -- instance of \code{\link[ctv]{ctv}} class}
  \item{file}{ file -- destination}
  \item{css}{ css}
  \item{packageURL}{ packageURL -- URL stub for use in link creation}
  \item{reposname}{ reposname -- string identifying repository name in titles}
}
\details{minor modification of \code{\link[ctv]{ctv2html}} that propagates
the title information into the anchor.
}
\value{
an html character stream
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
# see the HOWTO-BCV.Rnw in inst/doc
}
\keyword{ models }
