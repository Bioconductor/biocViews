\name{BiocView-class}
\docType{class}
\alias{BiocView-class}
\alias{coerce,BiocView,rdPackageTable-method}
\alias{htmlDoc,BiocView-method}
\alias{htmlFilename,BiocView-method}
\alias{htmlValue,BiocView-method}
\alias{show,BiocView-method}

\title{Class "BiocView" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BiocView", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{subViews}:}{Object of class \code{"character"} ~~ }
    \item{\code{parentViews}:}{Object of class \code{"character"} ~~ }
    \item{\code{Title}:}{Object of class \code{"character"} ~~ }
    \item{\code{reposRoot}:}{Object of class \code{"character"} ~~ }
    \item{\code{homeUrl}:}{Object of class \code{"character"} ~~ }
    \item{\code{htmlDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{packageList}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"RepositoryDetail"}, directly.
Class \code{"Htmlized"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "BiocView", to = "rdPackageTable")}: ... }
    \item{htmlDoc}{\code{signature(object = "BiocView")}: ... }
    \item{htmlFilename}{\code{signature(object = "BiocView")}: ... }
    \item{htmlValue}{\code{signature(object = "BiocView")}: ... }
    \item{show}{\code{signature(object = "BiocView")}: ... }
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
