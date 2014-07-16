\name{recommendBiocViews}
\alias{recommendBiocViews}
\title{
    Recommend biocViews for an existing Package.
}
\description{
    Packages being added to the Bioconductor Project require biocViews in their 
    DESCRIPTION file.(Note that the field name "biocViews" is case-sensitive and 
    must begin with a lower-case 'b'.)biocViews are "keywords" which are used to 
    describe a given package. They are broadly divided into three categories, 
    representing the type of packages present in the Bioconductor Project -
    Software, Annotation Data and Experiment Data.
}
\usage{
    recommendBiocViews(pkgdir, branch)
}
\arguments{
  \item{pkgdir}{
    The path of the package Directory. 
  }
  \item{branch}{
    The branch which your package will belong to. It can be either 'Software',
    'AnnotationData' or 'ExperimentData'.
  }
}
\details{
    This function parses the package directory provided by the user to recommend
    biocViews to the user. 
    The output is a suggested list - the user of this function is expected to go 
    through this list and find which biocViews best describe his or her package.
    It uses the following strategies. 
\itemize{
    \item It parses the "Description", "Title", "Package" of the DESCRIPTION 
    page to find biocViews. 
    \item It looks up the biocViews of the packages in the "Depends" field of 
    the given package to recommend biocViews
    \item It parses the text from the man pages and the vignettes to suggest 
    biocViews. 
}
Please note the following:
\itemize{
    \item Do not make up your own biocViews.
    \item Double check the spelling and case of the biocViews added.
    \item Please add biocViews only from the appropriate branch. eg: Software 
    packages should have only Software biocViews. 
}
}
\value{
 A list is returned with 3 characters - current , recommended and remove. 
 \itemize{
    \item "current" contains the biocViews from the package's DESCRIPTION file.
    \item "recommended" are the recommended biocViews - This is a suggested list
    which the user can add in addition to "current" biocViews - the user is 
    expected to go through this list and find which biocViews best describe 
    their package.
    \item "remove" are those biocViews which are inconsistent with the 
    Bioconductor biocViews. (Hint - check for spelling, cases and plural) 
 }
}
\author{
Sonali Arora. 
}



