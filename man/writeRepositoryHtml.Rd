\name{writeRepositoryHtml}
\alias{writeRepositoryHtml}
\title{Write package descriptions and a repository index as HTML}
\description{
  This function generates an HTML file for each package in a repository
  and generates an \code{index.html} file that provides an alphabetized
  listing of the packages.
}
\usage{
writeRepositoryHtml(reposRoot, title, reposUrl = "..", link.rel = TRUE)
}
\arguments{
  \item{reposRoot}{character vector specifying the path to the root of
    the CRAN-style package repository.}
  \item{title}{character vector giving the title for the repository}
  \item{reposUrl}{character vector giving the prefix for URL in links
    generated on the package description pages.  The default is
    \code{".."} which works well if the package description HTML files are
    written to an \code{html} subdirectory under the root of the
    repository.}
  \item{link.rel}{logical indicating whether the index page should
    generate relative URL links.  The default is \code{TRUE}.  If you are
    generating HTML for a remote repository, you will want to set this to
    \code{FALSE}. }
}
\author{Seth Falcon}
\keyword{utilities }
