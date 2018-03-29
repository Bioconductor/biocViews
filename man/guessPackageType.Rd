\name{guessPackageType}
\alias{guessPackageType}
\title{
    Guess Package Type (Software, ExperimentData, AnnotationData)  using existing biocViews.
}
\description{
    biocViews are "keywords" which are used to describe a given package. They
    are broadly divided into three categories, representing the type of 
    packages present in the Bioconductor Project - Software, Annotation Data 
    and Experiment Data. biocViews are supposed to come from only one of
    the three fields, but this function will check the list of biocViews
    and guess the package type based on how many biocViews came from
    each field.
}
\usage{
    guessPackageType(biocViews)
}

\arguments{
  \item{biocViews}{ A character vector containing a list of biocViews.}
}

\value{ 
    A character(1) of package type: either "Software",
    "ExperperimentData", or "AnnotationData". 
}
\author{
    Lori Shepherd
}
\examples{
    guessPackageType(c("clustering", "classification"))
    guessPackageType(c("Organism", "Homo Sapien"))
}
