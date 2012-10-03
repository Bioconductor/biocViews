\name{getPackageNEWS}
\alias{getPackageNEWS}
\alias{printNEWS}
\title{
  Retrieve and print package NEWS
}
\description{

  These functions visit two repository PACKAGE files, identifying
  packages that are present in the \sQuote{current} repository and have
  NEWS since the base version of the same package in the
  \sQuote{previous} repository. All NEWS is reported for packages only
  in the current repository.
}
\usage{
getPackageNEWS(prevRepos="http://www.bioconductor.org/packages/2.10/bioc", 
               currRepos="http://www.bioconductor.org/packages/2.11/bioc", 
               srcDir)
printNEWS(dbs, destfile, overwrite = FALSE, width = 68,
          output=c("md", "text"), ...)
}

\arguments{

  \item{prevRepos}{\code{character(1)} repository from which NEWS starts.}

  \item{currRepos}{\code{character(1)} repository of current packages.}

  \item{srcDir}{\code{character(1)} directory containing the source all
    current pacakges}

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
  Martin Morgan \url{mtmorgan@fhcrc.org}
}

\keyword{manip}% __ONLY ONE__ keyword per line
