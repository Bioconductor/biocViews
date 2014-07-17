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