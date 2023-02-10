\name{biocViews-deprecated}

\alias{getPackageNEWS}
\alias{printNEWS}
\alias{extractNEWS}
\alias{biocViews-deprecated}

\title{Deprecated NEWS functions in biocViews}

\description{
    Functions working with a package's NEWS file have been moved out of the
    package. These include \code{getPackageNEWS}, \code{printNEWS}, and
    \code{extractNEWS}. Please see the \code{ReleaseLaunch} package.
}

\usage{
getPackageNEWS(
    prevRepos="3.6",
	currRepos="3.7",
	repo=c("bioc", "data/experiment", "workflows"),
	srcdir = NULL
)
printNEWS(
    dbs, destfile, overwrite = FALSE, width = 68, output=c("md", "text"),
    relativeLink=FALSE, ...
)
extractNEWS(reposRoot, srcContrib, destDir)
}

\arguments{

  \item{prevRepos}{\code{character(1)} Bioconductor version from which NEWS starts.}

  \item{currRepos}{\code{character(1)} Bioconductor version for current packages.}

  \item{repo}{\code{character(1)} Which repository to get NEWS
  for. bioc is software packages, data/experiment is for data experiment
  packages and workflows for workflow packages}

  \item{srcdir}{Path to local checkout of package repositories, if NULL
  will try and use files on main builders}

  \item{dbs}{A list of \code{news_db} elements, as returned by
    \code{getPackageNEWS}.}

  \item{destfile}{\code{character(1)} file path to the location where
    NEWS will be printed.}

  \item{overwrite}{\code{logical(1)} indicating whether \code{destfile}
    can be over-written, if it exists.}

  \item{width}{\code{numeric(1)} number of characters news items are to
    be wrapped to, excluding indent.}

  \item{output}{\code{character(1)} output to text or markdown format.}

  \item{relativeLink}{Should links to packages be relative links on
  bioconductor.org website or include full url
  'https//bioconductor.org'. default: FALSE is full url.}

  \item{...}{additional arguments, unused.}
  
  \item{reposRoot}{Top level path for CRAN-style repos}
  
  \item{srcContrib}{Location of source packages}
  
  \item{destDir}{where to extract}
}

\value{
  A list of \code{news_db} files, as returned by \code{utils::news},
  for each package for which relevant NEWS is available.
}
\author{
  Martin Morgan \url{mtmorgan@fhcrc.org} and Lori Shepherd
}

\seealso{\url{https://github.com/Bioconductor/ReleaseLaunch}}

\keyword{manip}% __ONLY ONE__ keyword per line
\keyword{utilities}
