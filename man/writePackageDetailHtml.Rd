\name{writePackageDetailHtml}
\alias{writePackageDetailHtml}

\title{Write HTML files for packages in a CRAN-style repository}
\description{
  This function creates package "homepages" that describe the package
  and provide links to download package artifacts in the repository.
}
\usage{
writePackageDetailHtml(pkgList, htmlDir = "html")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkgList}{A list of \code{PackageDescription} objects.}
  \item{htmlDir}{The files will be written to this directory.}
}
\author{Seth Falcon}
\seealso{\code{\link{writeRepositoryHtml}}}
\keyword{utilities}

