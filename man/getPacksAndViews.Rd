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
\title{Obtain the package names and biocViews entries from
a repository PACKAGES file; also create an interpolated view-centered list
on the basis of a vocabulary graph.}
\description{
}
\usage{
getPacksAndViews(reposURL)
permulist(allv, vocab, root, interp=TRUE)
}
\arguments{
  \item{reposURL}{ reposURL -- the URL path to PACKAGES for a repository}
  \item{plist}{ plist -- a list (one element per package, each element
is a vector of view names prescribed by package author, or \code{NoViewProvided})}
  \item{vgraph}{ vgraph -- a graphNEL encoding the rooted DAG representing the vocabulary of views}
  \item{root}{the node that is the root of vgraph}
  \item{interp}{A logical variable; if TRUE, then if package P is in view V and
view W is a generalization of V, then P is in view W.  Otherwise only direct
view associations are used.}
}
\details{
Requires internet access.  Uses a locally coded variation of \code{\link{available.packages}}
to harvest the \code{biocViews} field from the PACKAGES data.
}
\value{
}
\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
\note{  }


\seealso{  }

\examples{
require(Biobase)
if (testBioCConnection()) {
	gg <- getPacksAndViews("http://bioconductor.org/packages/bioc/1.8/src/contrib/")
} else data(gg)
gg[1:4]
data(oct05) # get the vocabulary graph
pp <- permulist(gg, oct05, "vocRoot")
pp[1:4]
}
\keyword{ models }
