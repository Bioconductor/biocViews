getBcViews <- function(packnames, vocab, root="vocRoot", interp=TRUE) {
 allv <- sapply(packnames, packageDescription, field="biocViews")
#
# in sept 05, packageDescription could introduce a newline
#
 allv <- gsub("\\\n", " ", allv)
#
# if they use biocViews: x,y,z
#  this particular approach will work -- thanks seth
 allv <- strsplit(allv, ", *")
 lens <- sapply(allv, length)
 repp <- rep(packnames, lens)
 ans <- split( repp, unlist(allv) )
 if (interp) pump(ans, vocab, root)
 else ans
}
