\name{getCurrentbiocViews}
\alias{getCurrentbiocViews}
\title{
Get a list of biocViews for each branch
}
\description{
This function looks returns a list containing all the biocViews that are present
on the Bioconductor website. 
}
\usage{
getCurrentbiocViews()
}
\details{
It parses the dot file present inside the biocViews package.
}
\value{
It returns a  named list with 3 components.
  \item{Software}{biocViews from the software branch}
  \item{ExperimentData}{biocViews from the ExperimentData branch}
  \item{AnnotationData}{biocViews from the AnnotationData branch}
}
\author{
Sonali Arora
}
\examples{
ans <- getCurrentbiocViews()
## only the first 6 from each branch are shown here.  
lapply(ans, head)
}