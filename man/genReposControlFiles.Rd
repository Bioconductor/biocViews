\name{genReposControlFiles}
\alias{genReposControlFiles}
\title{Generate CRAN-style repository control files}
\description{
  This function generates control files for CRAN-style repositories.
  For each path specified in \code{contribPaths} a \code{PACKAGES} file
  is written.  In addition, two top-level control files are created:

  \code{REPOSITORY} contains information about the specified contrib
  paths.

  \code{VIEWS} contains metadata for all packages in the repository
  including the paths to any extracted vignettes, if found.  This file
  is useful for generating HTML views of the repository.
}
\usage{
genReposControlFiles(reposRoot, contribPaths, manifestFile = NA,
  meatPath = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reposRoot}{character vector containing the path to the
  CRAN-style repository root directory.}
  \item{contribPaths}{A named character vector.  Valid names are
    \code{source}, \code{win.binary}, \code{mac.binary},
    \code{mac.binary.mavericks}, and \code{mac.binary.el-capitan}.
    Values indicate the paths to the package archives relative to
    the \code{reposRoot}.}
  \item{manifestFile}{character(1). File path location to Bioconductor
  formatted manifest file that lists all current packages. This file
  will be used in the write_VIEWS function to cross check successfully
  built packages with all expected packages. Packages that have not
  built will be given dummy entry for complete listing in bioc_VIEWS. If
  NA cross check is skipped and packages not built on any system will be
  missing from biocVIEWS}
  \item{meatPath}{character(1). File path location to the directory
  containing cloned repositories of Bioconductor packages. If
  manifestFile is used for cross checking and the meatPath is provided,
  entries from the DESCRIPTION file are manually entered into biocVIEWS
  information. If NA dummy values for minimal fields for landing page
  generation are included with ERROR. This attempts to fill in as much
  information as possible for packages that have failed to build.}
}
\author{Seth Falcon}
\seealso{
  \code{\link[tools:writePACKAGES]{write_PACKAGES}},
  \code{\link{extractVignettes}},
  \code{\link{write_REPOSITORY}},
  \code{\link{write_VIEWS}}
}
\keyword{utilities}

