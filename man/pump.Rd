\name{pump}
\alias{pump}
\title{interpolate topics in a package-view association  }
\description{interpolate topics in a package-view association
}
\usage{
pump(viewlist, vocab, root="vocRoot")
}
\arguments{
  \item{viewlist}{ viewlist}
  \item{vocab}{ vocab}
  \item{root}{ root}
}
\details{If T is a hypernym of S and package P is associated 
with task S, P should also be associated with task T.  This function
assures this.
}
\value{a revised view list
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(sep05)
pump(list(DifferentialExpression="limma"),sep05)
}
\keyword{ models }
