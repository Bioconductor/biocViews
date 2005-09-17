\name{getBcViews}
\alias{getBcViews}
\title{build a view-package association list  }
\description{build a view-package association list
}
\usage{
getBcViews(packnames)
}
\arguments{
  \item{packnames}{ vector of package names; all named packages must be installed}
}
\details{
extracts the biocViews tag value from each DESCRIPTION file,
parses it (it must have x, y, z format, not hugging commas), and
builds the view-package association list (one vector element per topic)
}
\value{
a named list with one element per used topic and a vector of
package names in each element
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
pl <- c("Biobase", "clusterStab", "limmaGUI", "GeneTS", "RBGL")
getBcViews(pl)
}
\keyword{ models }
