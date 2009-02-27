\name{writePackageDetailHtml}
\alias{writePackageDetailHtml}

\title{Write HTML files for packages in a CRAN-style repository}
\description{
  This function creates package "homepages" that describe the package
  and provide links to download package artifacts in the repository.
}
\usage{
writePackageDetailHtml(pkgList, htmlDir = "html", backgroundColor="transparent")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkgList}{A list of \code{PackageDescription} objects.}
  \item{htmlDir}{The files will be written to this directory.}
  \item{backgroundColor}{A character vector giving the background color
    for the body in the CSS file.}
}
\author{Seth Falcon}
\seealso{\code{\link{writeRepositoryHtml}}}
\keyword{utilities}

