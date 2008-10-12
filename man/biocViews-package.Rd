\name{biocViews-package}
\alias{biocViews-package}
\alias{biocViews}
\docType{package}
\title{
Categorized views of R package repositories
}
\description{
Structures for vocabularies and narratives of views. This can be
used to create HTML views of the package structure in a Bioconductor
repository.
}
\details{
\tabular{ll}{
Package: \tab biocViews\cr
Version: \tab 1.7.3\cr
Depends: \tab R (>= 2.4.0), methods, tools, graph (>= 1.9.26), RBGL (>= 1.13.5), XML\cr
License: \tab Artistic-2.0\cr
URL: \tab http://www.bioconductor.org/packages/release/BiocViews.html\cr
biocViews: \tab Infrastructure\cr
}

Index:
\preformatted{
BiocView-class          Class "BiocView"
Htmlized-class          Class "Htmlized"
PackageDetail-class     Class "PackageDetail"
RepositoryDetail-class
                        Class "RepositoryDetail"
biocViewsVocab          Bioconductor Task Views Vocabulary Data
extractVignettes        Extract pdf vignettes from local package
                        repository
genReposControlFiles    Generate CRAN-style repository control files
getBiocSubViews         Build a list of BiocView objects from a package
                        repository
getBiocViews            Build a list of BiocView objects from a package
                        repository
getPacksAndViews        Parse VIEWS file for views and packages
getSubTerms             Retrieve a term and its children from a vocab
                        DAG
htmlDoc                 Create a complete HTML document representation
                        of an object
htmlFilename            Return a filename for an object's HTML
                        representation
htmlValue               HTML Representation of an Object
writeBiocViews          Write a list of BiocView objects to HTML
writeHtmlDoc            Write an XML DOM containing HTML to a file
writePackageDetailHtml
                        Write HTML files for packages in a CRAN-style
                        repository
writeRepositoryHtml     Write package descriptions and a repository
                        index as HTML
writeTopLevelView       Write the view for the root of a vocabulary to
                        disk
write_REPOSITORY        Write a REPOSITORY control file for a
                        CRAN-style package repository
write_SYMBOLS           Write a SYMBOLS file
write_VIEWS             Write a VIEWS control file for a CRAN-style
                        package repository
}

The terms of the vocabulary are stored in a DAG, which can be loaded as
the serialized data object \code{biocViewsVocab}. For listing of
available terms use function \code{getSubTerms}.

Further information is available in the following two vignettes:
\tabular{ll}{
\code{HOWTO-BCV} \tab Basic package usage\cr
\code{createReposHtml} \tab Further information for repository admins\cr
}
}
\author{
VJ Carey <stvjc@channing.harvard.edu>, BJ Harshfield <rebjh@channing.harvard.edu>, S Falcon <sfalcon@fhcrc.org>

Maintainer: Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>
}

\keyword{ package }

\examples{
data(biocViewsVocab)
getSubTerms(biocViewsVocab, "AssayTechnologies")
}
