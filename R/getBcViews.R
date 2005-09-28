getBcViews <- function(packnames, vocab, root="vocRoot", interp=TRUE) {
 allv <- sapply(packnames, packageDescription, field="biocViews")
#
# in sept 05, packageDescription could introduce a newline
#
 allv <- gsub("\\\n", " ", allv)
#
# if they use biocViews: x,y,z
#  this particular approach will work -- thanks seth
 allv <- strsplit(allv, ",[ \t]*")
 knownTerms <- nodes(vocab)
 knownTermsLower <- tolower(knownTerms)
 allv <- lapply(allv, function(x) {
     idx <- match(tolower(x), knownTermsLower)
     unknown <- is.na(idx)
     if (any(unknown)) {
         warning("Dropping unknown biocViews terms:\n",
                 paste(x[unknown], collapse=", "))
         idx <- idx[!is.na(idx)]
     }
     knownTerms[idx]
 })
 lens <- sapply(allv, length)
 repp <- rep(packnames, lens)
 ans <- split( repp, unlist(allv) )
 if (interp) pump(ans, vocab, root)
 else ans
}
