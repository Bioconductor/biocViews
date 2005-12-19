\name{RepositoryDetail-class}
\docType{class}
\alias{RepositoryDetail-class}

\alias{rdPackageTable-class}

\alias{htmlDoc,RepositoryDetail-method}
\alias{htmlFilename,RepositoryDetail-method}

\alias{htmlValue,RepositoryDetail-method}
\alias{htmlValue,rdPackageTable-method}

\title{Class "RepositoryDetail"}
\description{Representation of R package repository index}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RepositoryDetail", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{Title}:}{Object of class \code{"character"} giving the
    title for the repository.}
    \item{\code{reposRoot}:}{Object of class \code{"character"} giving the root
    URL of the repository}
    \item{\code{homeUrl}:}{Object of class \code{"character"} ?}
    \item{\code{htmlDir}:}{Object of class \code{"character"} ? }
    \item{\code{packageList}:}{Object of class \code{"list"} consisting
    of objects of class \code{PackageDetail-class} }
  }
}
\section{Extends}{
Class \code{"Htmlized"}, directly.
}
\section{Methods}{
  \describe{
    \item{htmlDoc}{\code{signature(object = "RepositoryDetail")}: ... }
    \item{htmlFilename}{\code{signature(object = "RepositoryDetail")}: ... }
    \item{htmlValue}{\code{signature(object = "RepositoryDetail")}: ... }
	 }
}
\author{Seth Falcon}
\keyword{classes}
