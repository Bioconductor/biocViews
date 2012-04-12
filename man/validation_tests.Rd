\name{validate_bioc_views}
\alias{validate_bioc_views}
\alias{validation_tests}
\title{
  Validate a package's biocViews.
}
\description{

  Ensures that a package has biocViews and that they are valid.
  Function is designed to be called from the unit tests of another
  package.
}
\usage{
validate_bioc_views(pkg)
}

\arguments{

  \item{pkg}{\code{character(1)} Name of package to validate.}
}
\value{
  \code{invisible(NULL)} if tests pass.
}
\author{
  Dan Tenenbaum \url{dtenenba@fhcrc.org}
}
\examples{
validate_bioc_views("biocViews")
}
\keyword{manip}% __ONLY ONE__ keyword per line
