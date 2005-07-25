\name{getCTVs}
\alias{getCTVs}
\alias{makeCTV}
\title{iterate over a list of views, creating a ctv object
for each view  }
\description{ iterate over a list of views, creating a ctv object  
for each view
}
\usage{
getCTVs(vpal, vocab)
}
\arguments{
  \item{vpal}{ vpal -- output of packAssoc2viewlist}
  \item{vocab}{ vocab -- biocViews vocabulary graph}
}
\details{ getCTVs uses makeCTV repetitively
}
\value{
a list of ctv objects suitable for serialization to HTML
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(pal)
vpal <- packAssoc2viewlist(pal)
data(bcVoc)
ct <- getCTVs( vpal, bcVoc )
ct[[1]]
}
\keyword{ models }
