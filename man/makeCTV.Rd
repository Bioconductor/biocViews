\name{makeCTV}
\alias{makeCTV}
\alias{getCTVlist}
\alias{bcv2html}
\title{tools for building HTML pages from task view lists}
\description{
tools for building HTML pages from task view lists
}
\usage{
makeCTV(viewname, topic, viewmaintainer, packs, links, vocab, root="vocRoot")
getCTVlist(vpal, vocab)
bcv2html (x, file = NULL, css = "../../../R.css", packageURL = "../Descriptions/",
    reposname = "CRAN")
}
\arguments{
  \item{viewname}{ viewname -- character string naming a view}
  \item{topic}{ topic -- character string expressing the related topic}
  \item{viewmaintainer}{ viewmaintainer -- character string naming the maintainer of the view}
  \item{packs}{ packs -- character vector listing packages in the view}
  \item{links}{ links -- character vector of links, not currently used}
  \item{vocab}{ vocab -- graph of the biocViews vocabulary}
  \item{root}{ root -- root of the vocabulary graph}
  \item{vpal}{ vpal -- list of views, as created by \code{\link{permulist}}}
  \item{x}{x -- ctv object}
  \item{file}{file -- destination filename string}
  \item{css}{css -- path to governing stylesheet}
  \item{packageURL}{path to description of package}
  \item{reposname}{string that will be used in HTML titles}
}
\details{
\code{makeCTV} creates an XML document reflecting the \code{ctv} document
structure, writes it to a temporary file, and invokes \code{\link{read.ctv}}
to create an instance of the \code{ctv} class.

\code{getCTVlist} applies \code{makeCTV} along a list of views, creating
a list of \code{ctv} objects.
}
\value{
\code{makeCTV} returns an instance of the \code{ctv} class

\code{getCTVlist} returns a list of \code{ctv} objects

\code{bcv2html} returns an HTML document stream and writes it to a file.
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
example(getPacksAndViews) # creates pp
cc <- getCTVlist(pp, oct05)
cc[1:3]
lapply( cc[1], bcv2html, reposname="Bioconductor" )[[1]][1:5]
}
\keyword{ models }
