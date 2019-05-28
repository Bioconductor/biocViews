\name{write_VIEWS}
\alias{write_VIEWS}
\title{Write a VIEWS control file for a CRAN-style package repository}
\description{
  This function writes a \code{VIEWS} file to the top-level of a
  CRAN-style package repository.  The \code{VIEWS} file is in DCF format
  and describes all packages found in the repository.

  The \code{VIEWS} file contains the complete \code{DESCRIPTION} file
  for each source package in the repository.  In addition, metadata for
  available binary packages and vignettes is centralized here.
}
\usage{
write_VIEWS(reposRootPath, fields = NULL,
          verbose = FALSE, vignette.dir = "vignettes",
          manifestFile = NA, meatPath = NA)
}
\arguments{
  \item{reposRootPath}{character vector containing the path to the
    CRAN-style repository root directory.}
  \item{fields}{Any additional fields to include.  You shouldn't need
    this, but if you have added fields to the DESCRIPTION files of the
    packages in the repository, you may want it.}
  \item{type}{One of \code{source}, \code{win.binary},
    \code{mac.binary}, \code{mac.binary.mavericks},
    or \code{mac.binary.el-capitan} indicating which set of
    packages should be used to build up the "shared" information.  Since a
    repository can contain different versions of the same package (source
    vs binary) the shared information may not be reliable.}
  \item{verbose}{logical, if \code{TRUE}, print progress messages.}
  \item{vignette.dir}{character specifying where to look for vignettes.}
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
\section{Warning}{
  This function uses a private function from the \code{tools} package:
  \code{tools:::.build_repository_package_db}.
}

\seealso{
  \code{\link[tools:writePACKAGES]{write_PACKAGES}},
  \code{\link{extractVignettes}},
  \code{\link{genReposControlFiles}},
  \code{\link{write_REPOSITORY}}
}

\keyword{utilities}
