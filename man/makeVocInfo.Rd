\name{makeVocInfo}
\alias{makeVocInfo}
\alias{tellSubTop}
\alias{tellSuperTop}
\title{perform operations on a vocabulary graph
to obtain context of a given topic  }
\description{perform operations on a vocabulary graph
to obtain context of a given topic
}
\usage{
makeVocInfo(topic, vocab, root="vocRoot")
}
\arguments{
  \item{topic}{ topic -- character string naming a topic}
  \item{vocab}{ vocab -- a graph describing a vocabulary}
  \item{root}{ root -- node that is vocabulary root}
}
\details{
tellSubTop gives the accessibility list of the vocabulary graph from topic node;
tellSuperTop traces up to the root from topic node; makeVocInfo returns a list
with both types of information
}
%\value{
%}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(bcVoc)
makeVocInfo("DifferentialExpression", bcVoc)
}
\keyword{ models }
