\name{recommendPackages}
\alias{recommendPackages}
\title{
    Recommend Packages using existing biocViews.
}
\description{
    biocViews are "keywords" which are used to describe a given package. They
    are broadly divided into three categories, representing the type of 
    packages present in the Bioconductor Project - Software, Annotation Data 
    and Experiment Data. One can find packages which are tagged with biocViews
    using this function.
}
\usage{
    recommendPackages(biocViews, use.release=TRUE, intersect.views=TRUE)
}

\arguments{
  \item{biocViews}{ A character vector containing a list of biocViews. Currently
  only biocViews from the software branch are supported.
  }
  \item{use.release}{ A logical character indicating if you want packages 
  recommended from  the release branch of Biocondutor.
  } 
  \item{intersect.views}{ A logical character indicating if you want packages 
  which are tagged with all the input biocViews or packages tagged with any one
  or more of the biocViews.
  }
}

\value{ 
    A character vector containing a list of packages. If multiple biocViews
    are given as input, the result returns packages tagged with all or atleast 
    one of the input biocViews. 
}
\author{
    Sonali Arora. 
}
\examples{
    recommendPackages(c("clustering", "classification"))
}
