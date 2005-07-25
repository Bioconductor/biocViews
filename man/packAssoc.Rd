\name{packAssoc}
\alias{packAssoc}
\title{ interactive interface for associating R packages with vocabulary terms }
\description{
 interactive interface for associating R packages with vocabulary terms 
}
\usage{
packAssoc(packlist, vocGraph, root="vocRoot")
}
\arguments{
  \item{packlist}{ packlist -- vector of packagenames}
  \item{vocGraph}{ vocGraph -- graph encoding a vocabulary}
  \item{root}{ root -- root node of vocabulary}
}
\details{
prompting occurs using tcltk widgets
}
\value{
a list with package information
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
if (interactive()) {
 data(bcVoc)
 packAssoc("Biobase", bcVoc)
}
}
\keyword{ models }
