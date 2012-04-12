## These are test functions that can be called from the unit tests of any BioC
## package.

validate_bioc_views <- function(pkg)
{
    bvStr <- packageDescription(pkg)$biocViews
    checkTrue(!is.null(bvStr), paste("No biocViews defined for package", pkg))
    bvStr <- gsub(" ", "", bvStr)
    views <- strsplit(bvStr, ",")[[1]]
    data("biocViewsVocab")
    nodes <- nodes(biocViewsVocab)
    for (view in views)
    {
        checkTrue(view %in% nodes, 
            paste("Invalid view", view, "in package", pkg))
    }
    invisible(NULL)
}
