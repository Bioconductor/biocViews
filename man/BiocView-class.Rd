\name{BiocView-class}
\docType{class}
\alias{BiocView-class}

\alias{bvTitle-class}
\alias{bvPackageTable-class}
\alias{bvSubViews-class}
\alias{bvParentViews-class}

\alias{coerce,BiocView,rdPackageTable-method}
\alias{htmlDoc,BiocView-method}
\alias{htmlFilename,BiocView-method}

\alias{htmlValue,BiocView-method}
\alias{htmlValue,bvSubViews-method}
\alias{htmlValue,bvParentViews-method}

\alias{show,BiocView-method}

\title{Class "BiocView" }
\description{Representation of of Bioconductor "view".}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BiocView", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{"character"} giving the
    name of the view. }
    \item{\code{subViews}:}{Object of class \code{"character"} giving
    the names of the subviews of this view.}
    \item{\code{parentViews}:}{Object of class \code{"character"} giving
    the names of the views that are this view's parents.}
    \item{\code{Title}:}{Object of class \code{"character"} giving
    longer description of view?}
    \item{\code{reposRoot}:}{Object of class \code{"character"} URL for repository }
    \item{\code{homeUrl}:}{Object of class \code{"character"} ? }
    \item{\code{htmlDir}:}{Object of class \code{"character"} ? }
    \item{\code{packageList}:}{Object of class \code{"list"} consisting
    of \code{PackageDetail-class} objects }
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
\author{Seth Falcon}

\keyword{classes}
