test_findBranchReadDot <- function() 
{
    
    checkException(.findBranchReadDot(current=c("ChipName"), branch="software"))
    checkException(.findBranchReadDot(current=c("RNASeq","ChipName"), 
                                      branch="software"))
}