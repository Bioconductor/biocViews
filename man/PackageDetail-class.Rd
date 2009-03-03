\name{PackageDetail-class}
\docType{class}
\alias{PackageDetail-class}

\alias{pdAuthorMaintainerInfo-class}
\alias{pdVignetteInfo-class}
\alias{pdDownloadInfo-class}
\alias{pdDetailsInfo-class}
\alias{pdDescriptionInfo-class}
\alias{pdVigsAndDownloads-class}

\alias{htmlDoc,PackageDetail-method}
\alias{htmlFilename,PackageDetail-method}

\alias{htmlValue,PackageDetail-method}
\alias{htmlValue,pdAuthorMaintainerInfo-method}
\alias{htmlValue,pdVignetteInfo-method}
\alias{htmlValue,pdDownloadInfo-method}
\alias{htmlValue,pdDetailsInfo-method}
\alias{htmlValue,pdDescriptionInfo-method}
\alias{htmlValue,pdVigsAndDownloads-method}

\title{Class "PackageDetail"}
\description{Representation of R package metadata.  Most slots
correspond to fields in a package's DESCRIPTION file.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PackageDetail", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Package}:}{Object of class \code{"character"} see DESCRIPTION }
    \item{\code{Version}:}{Object of class \code{"character"}  see
      DESCRIPTION }
    \item{\code{Title}:}{Object of class \code{"character"}  see
      DESCRIPTION }
    \item{\code{Description}:}{Object of class \code{"character"}  see
      DESCRIPTION }
    \item{\code{Author}:}{Object of class \code{"character"} see DESCRIPTION }
    \item{\code{Maintainer}:}{Object of class \code{"character"} see
      DESCRIPTION }
    \item{\code{Depends}:}{Object of class \code{"character"} see DESCRIPTION }
    \item{\code{Imports}:}{Object of class \code{"character"} see DESCRIPTION }
    \item{\code{Suggests}:}{Object of class \code{"character"} see DESCRIPTION }
    \item{\code{SystemRequirements}:}{Object of class \code{"character"}
      see DESCRIPTION }
    \item{\code{License}:}{Object of class \code{"character"} see DESCRIPTION }
    \item{\code{URL}:}{Object of class \code{"character"} see DESCRIPTION }
    \item{\code{biocViews}:}{Object of class \code{"character"} see
      DESCRIPTION }
    \item{\code{vignettes}:}{Object of class \code{"character"} giving
    paths to vignette pdf files in the repository }
    \item{\code{vignetteTitles}:}{Object of class \code{"character"} giving
    the titles of the vignette files in the repository }
    \item{\code{source.ver}:}{Object of class \code{"character"} version
    string for the source package}
    \item{\code{win.binary.ver}:}{Object of class \code{"character"}
    version string for the Windows binary package }
    \item{\code{mac.binary.universal.ver}:}{Object of class \code{"character"}
    version string for the OS X Tiger binary package }
    \item{\code{mac.binary.leopard.ver}:}{Object of class \code{"character"}
    version string for the OS X Leopard binary package }
    \item{\code{downloadStatsUrl}:}{Object of class \code{"character"} An
    optional URL for the download history statistics. }
    \item{\code{manuals}:}{Object of class \code{"character"} giving paths to
    reference manual pdf files in the repository}
    \item{\code{dependsOnMe}:}{Object of class \code{"character"} giving
    packages found in the repository that depend on this package}
    \item{\code{importsMe}:}{Object of class \code{"character"} giving
    packages found in the repository that imports this package}
    \item{\code{suggestsMe}:}{Object of class \code{"character"} giving
    packages found in the repository that suggest this package}
    \item{\code{functionIndex}:}{Object of class \code{"character"} Not
    used.  Intended to hold function index data. }
    \item{\code{reposFullUrl}:}{Object of class \code{"character"} The URL
    for the full URL of the root of the repository. }
    \item{\code{reposRoot}:}{Object of class \code{"character"} The URL
    for the root of the repository. }
    \item{\code{viewRoot}:}{Object of class \code{"character"} The URL
    for the view of the repository. }
    \item{\code{devHistoryUrl}:}{Object of class \code{"character"} The URL
    for the development changelog. }
  }
}
\section{Extends}{
Class \code{"Htmlized"}, directly.
}
\section{Methods}{
  \describe{
    \item{htmlDoc}{\code{signature(object = "PackageDetail")}: Return an
      \code{XMLNode} instance containg a complete HTML document
      representation of the package.}
    \item{htmlFilename}{\code{signature(object = "PackageDetail")}:
      Return a filename appropriate for the HTML document representation. }
    \item{htmlValue}{\code{signature(object = "PackageDetail")}: Return
      \code{XMLNode} instance containing an HTML representation of the package. }
  }
}

\section{Details}{
  \code{pdAuthorMaintainerInfo-class}
  \code{pdVignetteInfo-class}
  \code{pdDownloadInfo-class}
  \code{pdDetailsInfo-class}
  \code{pdDescriptionInfo-class}
  \code{pdVigsAndDownloads-class}

  Dummy classes for HTML generation.  Each dummy class is a simple
  extension (it does not add any slots).  The purpose of each dummy
  class is to allow for method dispatch to generate HTML via the
  \code{\link{htmlValue}} method.

  You can convert convert a \code{PackageDetail} instance to one of the
  dummy classes like this: \code{descInfo <- as(pdObj,
    "pdDescriptionInfo")}
  
}

\author{Seth Falcon}

\examples{
pd <- new("PackageDetail",
          Package="MyFancyPackage",
          Version="1.2.3",
          Title="A Fancy Package",
          Description="This package does fancy things",
          Author="A. Coder",
          Maintainer="A. Coder <acoder@foo.bar.net>",
          Depends="methods",
          Imports="ASimplePackage",
          Suggests="MyDataPackage",
          biocViews="Infrastructure",
          vignettes="vignettes/MyFancyPackage/inst/doc/MFP1.pdf,\nvignettes/MyFancyPackage/inst/doc/MFP2.pdf",
          vignetteTitles="MFP1 Document,\nMFP2 Document",
          source.ver="src/contrib/MyFancyPackage_1.2.3.tar.gz",
          win.binary.ver="bin/windows/contrib/2.3/MyFancyPackage_1.2.2.zip",
          mac.binary.universal.ver="bin/macosx/universal/contrib/2.3/MyFancyPackage_1.2.3.tgz",
          mac.binary.leopard.ver="bin/macosx/leopard/contrib/2.3/MyFancyPackage_1.2.3.tgz",
          dependsOnMe=c("PackageThatExposesMe"),
          importsMe=c("AnEvenFancierPackage","AMuchFancierPackage"),
          suggestsMe="PackageThatUsesMeInVignette",
          reposRoot="http://foo.bar.org")

html <- htmlValue(pd)
pd

}
      
\keyword{classes}
