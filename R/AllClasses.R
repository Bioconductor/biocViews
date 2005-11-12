## TODO: make the reposInfo list into an S4 class to represent
## repository data

setOldClass("XMLOutputDOM")
setClassUnion("XMLOutputDOMOrNULL", members=c("XMLOutputDOM", "NULL"))
setClass("Htmlized", representation(.htmlDom="XMLOutputDOMOrNULL", "VIRTUAL"))


setClass("PackageDetail", contains="Htmlized",
         representation("Package"="character",
                        "Version"="character",
                        "Title"="character",
                        "Description"="character",
                        "Author"="character",
                        "Maintainer"="character",
                        "Depends"="character",
                        "Suggests"="character",
                        "Imports"="character",
                        "SystemRequirements"="character",
                        "License"="character",
                        "URL"="character",
                        "biocViews"="character",
                        "downloadLinks"="character",
                        "vignetteLinks"="character",
                        "manualLinks"="character",
                        "dependsOnMe"="character",
                        "suggestsMe"="character",
                        "functionIndex"="character",
                        "reposRoot"="character"))


setClass("RepositoryDetail", contains="Htmlized",
         representation(Title="character",
                        reposRoot="character",
                        homeUrl="character",
                        htmlDir="character",
                        packageList="list"))




    ## Outline

    ## 1. given a repos with VIEWS file, run extractVignettes.
    ## 2. Now get a pkgList using loadPackageDetails()
    ## 3. write HTML
