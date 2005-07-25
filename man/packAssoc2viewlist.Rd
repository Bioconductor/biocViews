\name{packAssoc2viewlist}
\alias{packAssoc2viewlist}
\title{permute a package:view list structure to a list
of views composed of package names}
\description{permute a package:view list structure to a list
of views composed of package names}
}
\usage{
packAssoc2viewlist(pael)
}
\arguments{
  \item{pael}{ pael is a  list that is produced by \code{\link{packAssoc}}.}
}
\details{
simple list manipulation
}
\value{
a list whose elements are named views, consisting of string vectors
of package names comprising the views
}
\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
\note{  }


%\seealso{  }

\examples{
data(pal)
packAssoc2viewlist( pal )
}
\keyword{ models }
