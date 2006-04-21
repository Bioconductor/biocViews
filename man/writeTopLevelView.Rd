\name{writeTopLevelView}
\alias{writeTopLevelView}

\title{Write the view for the root of a vocabulary to disk}
\description{
  Given a directory and a vocabulary represented as a \code{graphNEL}
  containing a DAG of terms, write the top-level term to disk as HTML.

  This assumes your vocabulary has a single term with no parents.
}
\usage{
writeTopLevelView(dir, vocab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{A string giving a directory in which to write the HTML file}
  \item{vocab}{A \code{graphNEL} instance giving the DAG of terms.  It
    should have a root node.  That is, there should be exactly one node
    with no incoming edges.}
}
\author{S. Falcon}
\keyword{utilities }
