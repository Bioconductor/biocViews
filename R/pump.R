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
 
