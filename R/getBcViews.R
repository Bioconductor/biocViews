getBcViews <- function(packnames) {
 allv <- sapply(packnames, packageDescription, field="biocViews")
#
# in sept 05, packageDescription could introduce a newline
#
 allv <- gsub("\\\n", " ", allv)
#
# i am assuming that people use format biocViews: x, y, z
# if they use biocViews: x,y,z
#  this particular approach will not work
 allv <- strsplit(allv, ", ")
 lens <- sapply(allv, length)
 repp <- rep(packnames, lens)
 split( repp, unlist(allv) )
}
