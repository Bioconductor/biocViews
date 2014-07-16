test_findBranchReadDot <- function() 
{
    
    checkException(.findBranchReadDot(current=c("ChipName"), branch="Software"))
    checkException(.findBranchReadDot(current=c("RNASeq","ChipName"), 
                                      branch="Software"))
    checkException(.findBranchReadDot(current=c("Software")))
}