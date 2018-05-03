\name{getPackageNEWS}
\alias{getPackageNEWS}
\alias{printNEWS}
\title{
  Retrieve and print package NEWS
}
\description{

  These functions visit two Bioconductor releases, identifying
  packages that are present in the \sQuote{current} repository and have
  NEWS since the base version of the same package in the
  \sQuote{previous} release. All NEWS is reported for packages only
  in the current repository.
}
\usage{
getPackageNEWS(prevRepos="3.6", 
               currRepos="3.7", 
               repo=c("bioc", "data/experiment", "workflows"))
printNEWS(dbs, destfile, overwrite = FALSE, width = 68,
          output=c("md", "text"), ...)
}

\arguments{

  \item{prevRepos}{\code{character(1)} Bioconductor version from which NEWS starts.}

  \item{currRepos}{\code{character(1)} Bioconductor version for current packages.}

  \item{repo}{\code{character(1)} Which repository to get NEWS
  for. bioc is software packages, data/experiment is for data experiment
  packages and workflows for workflow packages}

  \item{dbs}{A list of \code{news_db} elements, as returned by
    \code{getPackageNEWS}.}

  \item{destfile}{\code{character(1)} file path to the location where
    NEWS will be printed.}

  \item{overwrite}{\code{logical(1)} indicating whether \code{destfile}
    can be over-written, if it exists.}

  \item{width}{\code{numeric(1)} number of characters news items are to
    be wrapped to, excluding indent.}

  \item{output}{\code{character(1)} output to text or markdown format.}

  \item{...}{additional arguments, unused.}

}
\value{
  A list of \code{news_db} files, as returned by \code{utils::news}, 
  for each package for which relevant NEWS is available.
}
\author{
  Martin Morgan \url{mtmorgan@fhcrc.org} and Lori Shepherd
}

\keyword{manip}% __ONLY ONE__ keyword per line
