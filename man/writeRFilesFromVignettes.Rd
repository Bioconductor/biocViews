\name{writeRFilesFromVignettes}
\alias{writeRFilesFromVignettes}

\title{Write R files from vignettes}
\description{
  Ensures that .R files from vignette code chunks are written out.
}
\usage{
    writeRFilesFromVignettes(reposRoot, reposUrl="..",
                                    viewUrl="../..", reposFullUrl=reposUrl,
                                    downloadStatsUrl="", devHistoryUrl="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reposRoot}{Root directory of a CRAN-style repository}
  \item{reposUrl}{URL of repository}
  \item{viewUrl}{url of VIEWS file}
  \item{reposFullUrl}{Full URL of VIEWS file}
  \item{downloadStatsUrl}{URL to download stats page}
  \item{devHistoryUrl}{Dev history URL}
}
\keyword{utilities}

