
showVoc <- function(g,outfile=tempfile()) {
 top <- adj(g, nodes(g)[1])[[1]]
 dd <- xmlTree("a")
 dd$addTag("body", close=FALSE)
 for (i in 1:length(top)) {
   dd$addTag("H2", top[i])
   nxt <- adj(g, top[i])[[1]]
   if (length(nxt)>0) {
      dd$addTag("UL", close=FALSE)
      for (j in 1:length(nxt))
         dd$addTag("LI", nxt[j])
      dd$closeTag()
      }
   }
 dd$closeTag()
 cat(saveXML(dd$value()),sep="\n",file=outfile)
}
