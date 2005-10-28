
\name{topwrap}
\alias{topwrap}
\title{ create kernel of an overview HTML file  }
\description{ create kernel of an overview HTML file
}
\usage{
topwrap(gra)
}
\arguments{
  \item{gra}{ gra a vocablary graph}
}
\details{
provides unnumbered list elements of all top level views, need to
wrap this with titles and footers
}
%\value{
%}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(oct05)
tmp <- tempfile()
els <- topwrap(oct05)
toptop <- readLines(system.file("htmlfrags/topfrag.html", package="biocViews"))
cat(toptop,els,"</ul>\n", file=tmp)
readLines(tmp)
}
\keyword{ models }

