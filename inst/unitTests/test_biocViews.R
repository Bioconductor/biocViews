test_findBranchReadDot <- function() 
{
    
    checkException(.findBranchReadDot(current=c("ChipName"), branch="Software"))
    checkException(.findBranchReadDot(current=c("RNASeq","ChipName"), 
                                      branch="Software"))
    checkException(.findBranchReadDot(current=c("Software")))
    
    checkException(.findBranchReadDot(
        current=c("GUI, DNAMethylation, MethylationArray, IlluminaChip"),
        branch=c("Software","AnnotationData","ExperimentData")))
    
}

test_recommendPackages <- function()
{
    checkException(recommendPackages(""))
    checkException(recommendPackages(c("foo")))
    checkException(recommendPackages(c("aCGH","Agilentchip")))
    checkException(recommendPackages(c("aCGH","Agilentchip", "CancerData")))
    
    
    pca <- recommendPackages(c("PrincipalComponent"))
    dr <- recommendPackages(c("DimensionReduction"))
    ans <- intersect(dr,pca)
    test <- recommendPackages(c("PrincipalComponent", "DimensionReduction"))
    checkEquals(length(test), length(ans))
    checkIdentical(test, ans)
    
    test2 <- recommendPackages(c("PrincipalComponent", "DimensionReduction"), 
                               intersect.views=FALSE)
    checkEquals(length(unique(c(pca,dr))), length(test2))
}