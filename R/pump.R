pump <- function(viewlist, vocab, root="vocRoot") {
 # make packages annotated to hypernyms of their
 # direct annotations
 vs <- names(viewlist)
 for (v in vs) {
   st <- tellSuperTop( v, vocab, root )
   if (length(st) > 0) {
     for (sti in st)
       viewlist[[sti]] <- union(viewlist[[sti]], viewlist[[v]])
   }
 }
 viewlist
}


tellSuperTop <- function( topic, vocab, root="vocRoot" ) {
                                        # returns vector of supertopics
    if (length(topic)>1) stop("must have length 1 topic")
    if (!(topic %in% nodes(vocab))) {
        warning(paste("attempt to interpolate term [", topic,
                      "] that is not even in the vocabulary! just returning term"))
        return(topic)
    }
    require(RBGL)
    path <- sp.between.scalar( vocab, root, topic )$path
    path[-c(1, length(path))]
}


tellSubTop <- function( topic, vocab ) {
 if (length(topic)>1) stop("must have length 1 topic")
# returns vector of subtopics
 if (!(topic %in% nodes(vocab))) {
   warning(paste("attempt to interpolate term [", topic,
                 "] that is not even in the vocabulary! just returning term"))
   return(topic)
   }
 desc <- acc( vocab, topic )[[1]]
 names(desc)[desc==1]
}
