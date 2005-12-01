\name{rdPackageTable-class}
\docType{class}
\alias{rdPackageTable-class}
\alias{coerce,BiocView,rdPackageTable-method}
\alias{htmlValue,rdPackageTable-method}

\title{Class "rdPackageTable" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rdPackageTable", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{Title}:}{Object of class \code{"character"} ~~ }
    \item{\code{reposRoot}:}{Object of class \code{"character"} ~~ }
    \item{\code{homeUrl}:}{Object of class \code{"character"} ~~ }
    \item{\code{htmlDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{packageList}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"RepositoryDetail"}, directly.
Class \code{"Htmlized"}, by class \code{"RepositoryDetail"}.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "BiocView", to = "rdPackageTable")}: ... }
    \item{htmlValue}{\code{signature(object = "rdPackageTable")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
	or \code{\link{CLASSNAME-class}} for links to other classes
}
\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
