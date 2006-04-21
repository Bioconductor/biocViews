\name{getSubTerms}
\alias{getSubTerms}

\title{Retrieve a term and its children from a vocab DAG}
\description{
  Given a Directed Acyclic Graph (DAG) represented as a \code{graphNEL}
  instance, return a character vector consisting of the specified
  \code{term} and all of its descendants.  That is, give the list of
  terms for which a path exists starting at \code{term}.
}
\usage{
getSubTerms(dag, term)
}

\arguments{
  \item{dag}{A \code{graphNEL} representing a DAG}
  \item{term}{A string giving a term in the vocabulary (a node in \code{dag})}
}
\value{
  A character vector of term names.
}
\author{S. Falcon}
\examples{
data(biocViewsVocab)
getSubTerms(biocViewsVocab, "Software")
}
\keyword{utilities}

