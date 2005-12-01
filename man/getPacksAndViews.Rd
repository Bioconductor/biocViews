\name{getPacksAndViews}
\alias{getPacksAndViews}
\alias{permulist}
%Undocumented code objects:
%  makeVocInfo pump tellSubTop tellSuperTop
%Undocumented data sets:
%  gg oct05
\alias{makeVocInfo}
\alias{pump}
\alias{tellSubTop}
\alias{tellSuperTop}
\title{Parse VIEWS file for views and packages}
\description{
  Given a repository URL, download and parse the VIEWS file.
}

\usage{
getPacksAndViews(reposURL, local=FALSE)
}
\arguments{
  \item{reposURL}{character vector giving the URL of a CRAN-style
  repository containing a VIEWS file at the top-level.}
  \item{local}{logical indicating whether certain links should be
    absolute (using \code{reposURL}) or relative.}
}
\value{
  A list with named elements:

  \code{views}: Vector of view memberships.  Names are package names.

  \code{pkgList}: A list of \code{\link{PackageDetail-class}} objects.
}
\author{Seth Falcon}

\examples{
## TODO
}
\keyword{utilities}
