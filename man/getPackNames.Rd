\name{getPackNames}
\alias{getPackNames}
\title{ extract names of source images
of packages in a given directory  }
\description{ extract names of source images
of packages in a given directory
}
\usage{
getPackNames(rpackDir, majv=1)
}
\arguments{
  \item{rpackDir}{ rpackDir -- a folder holding source images of R packages}
  \item{majv}{ majv -- version threshold, so that packages are
excluded if they do not have
major version at least equal to the threshold}
}
%\details{
%}
\value{
a character vector of package names
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
# depends on your folder system
}
\keyword{ models }
