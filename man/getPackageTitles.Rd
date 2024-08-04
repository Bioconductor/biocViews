\name{getPackageTitles}
\alias{getPackageTitles}
\alias{getPackageTitles}
\alias{getPackageDescriptions}
\title{
  Retrieve list of package titles and print package Description
}
\description{

  These functions visit two Bioconductor releases branches, identifying
  differnt packages that are present in the \sQuote{current} repository
  from the \sQuote{previous} release. Utilizes the devel branch of
  bioconductor to retrieve description.
}
\usage{
getPackageTitles(prevBranch="RELEASE_3_6",
                 currBranch="devel",
                 manifest=c("software.txt", "data-experiment.txt", 
                   "workflows.txt", "data-annotation.txt", "books.txt"),
                 status = c("new", "removed"))
getPackageDescriptions(pkgs, outfile, output=c("md", "text"), relativeLink=FALSE)
}

\arguments{

  \item{prevBranch}{\code{character(1)} Bioconductor branch to compare to}

  \item{currBranch}{\code{character(1)} Bioconductor branch for current packages.}

  \item{manifest}{\code{character(1)} Which repository of pakcages to
  compare. software.txt is software packages, data-experiment.txt is for
  data experiment packages and workflows.txt for workflow packages}

  \item{status}{get new or removed package list comparing currBranch to prevBranch}

  \item{pkgs}{character() A list of packages to retrieve DESCRIPTION}

  \item{outfile}{\code{character(1)} file path to the location where
    DESCRIPTIONS will be printed.}

  \item{output}{\code{character(1)} output to text or markdown format.}

  \item{relativeLink}{Should links to packages be relative links on
  bioconductor.org website or include full url
  'https//bioconductor.org'. default: FALSE is full url.}
}
\value{
  A list of package titles.
}
\author{
  Martin Morgan \url{mtmorgan@fhcrc.org} and Lori Shepherd
}
\examples{
\dontrun{
# At release time get a list of new or removed or deprecated packages

# get new packages in release 3.7 that are not in 3.6
newSoft = getPackageTitles()
# get removed packages from 3.6 
rmSoft = getPackageTitles(currBranch="RELEASE_3_7", status="removed")
# get depreacted package for 3.7
deprecatedSoft = setdiff(getPackageTitles(status="removed"), rmSoft)

# repeated above for data-experiment packages
newData = getPackageTitles(manifest="data-experiment.txt")
rmData = getPackageTitles(currBranch="RELEASE_3_7", manifest="data-experiment.txt", status="removed")
deprecatedData = setdiff(getPackageTitles(manifest="data-experiment.txt", status="removed"), rmData)
}
}
\keyword{manip}% __ONLY ONE__ keyword per line
