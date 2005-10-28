
#
# what we need to do here: build an xhtml document component on the
# subtopics of each topic, embed the `info' narrative component if any
# and build the package/links list
#

tellSuperTop <- function( topic, vocab, root="vocRoot" ) {
# returns vector of supertopics
 if (length(topic)>1) stop("must have length 1 topic")
 if (!(topic %in% nodes(vocab))) {
   warning(paste("attempt to interpolate term [", topic, "] that is not even in the vocabulary! just returning term"))
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
   warning(paste("attempt to interpolate term [", topic, "] that is not even in the vocabulary! just returning term"))
   return(topic)
   }
 desc <- acc( vocab, topic )[[1]]
 names(desc)[desc==1]
}


###################################################
### chunk number 12: makeVocInfo
###################################################
makeVocInfo <- function(topic, vocab, root="vocRoot") {
 list(supertopics=tellSuperTop(topic,vocab, root),
   subtopics=tellSubTop(topic,vocab))
}

getCTVlist <- function (vpal, vocab) 
{
    vn <- names(vpal)
    nv <- length(vn)
    out <- list()
    for (i in 1:length(vn)) {
        tmp <- makeCTV(vn[i], vn[i], "None", vpal[[i]], "None", 
            vocab)
        tf <- tempfile()
        saveXML(tmp, file = tf)
        out[[vn[i]]] <- read.ctv(tf)
        unlink(tf)
    }
    out
}

 
makeCTV <- function( viewname, topic, viewmaintainer, packs, links, vocab, root="vocRoot" ) {
 require(XML)
 tr <- xmlOutputDOM("a")
 tr$addTag("CRANTaskView", close=FALSE)
  tr$addTag("name", viewname)
  tr$addTag("topic", topic)
  tr$addTag("maintainer", viewmaintainer)
  vocInf <- makeVocInfo( viewname, vocab, root )
  if (length(sut <- vocInf$supertopics)>0 | length(sbt <- vocInf$subtopics)>0) { 
       tr$addTag("info", close=FALSE)
       if (length(sut)>0) 
	{
	tr$addNode(xmlTextNode("<p>Subview of:</p>"))
	tr$addTag("ul", close=FALSE)
        for (i in 1:length(sut)) {
		tr$addTag("li", close=FALSE)
		tr$addTag("view", sut[i])
		tr$closeTag() # li
		}
	tr$closeTag() # ul
        }
       if (length(sbt)>0) 
	{
	tr$addNode(xmlTextNode("<p>Has subviews:</p>"))
	tr$addTag("ul", close=FALSE)
        for (i in 1:length(sbt)) {
		tr$addTag("li", close=FALSE)
		tr$addTag("view", sbt[i])
		tr$closeTag() # li
		}
	tr$closeTag() # ul
        }
       tr$closeTag() #info
   } else # done doing  vocab info
  tr$addTag("info", "none")
  tr$addTag("packagelist", close=FALSE)
  if (length(packs)>0) {
    for (i in 1:length(packs)) 
 	tr$addTag("pkg", packs[i])
    }
  tr$closeTag()
  tr$addTag("links", links)
  tr$closeTag()
  xmlChildren(tr$value())[[1]] # return node
}

#packAssoc2vlist <- function(pael) {
# allconc <- unique(sapply(pael, function(x) c(x$top, x$second, x$third)))
# tall <- t(allconc)
# pks <- rep(rownames(tall),3) # assumes 3 level hierarchy
# split(pks, as.character(tall))
#}

#getctv <- function(vl) {
# n <- length(vl)
# nms <- names(vl)
# out <- list()
# for (i in 1:n) {
##makeCTV <- function( viewname, topic, viewmaintainer, info, packs, links ) {
#  out[[i]] <- makeCTV( nms[i], nms[i], vl[[i]]$maintainer , "no", vl[[i]], "no" )
# }
# out
#}


#<CRANTaskView>
#
#  <name>MachineLearning</name>
#  <topic>Machine Learning &amp; Statistical Learning</topic>
#  <maintainer>Torsten Hothorn</maintainer>
#  
#  <info>
#    Several add-on packages implement ideas and methods developed at the
#    borderline between computer science and statistics - this field of research
#    is usually referred to as machine learning. 
#
#    The packages can be roughly structured into the following topics:
#    <ul>
#      <li><i>Neural Networks</i>: Single-hidden-layer neural network are 
#             implemented in package <tt>nnet</tt> as part of the <pkg>VR</pkg>
#             bundle (shipped with base R). </li>
#      <li><i>Recursive Partitioning</i>: Tree-structured models for
#             regression, classification and survival analysis, following the
#	     ideas in the CART book, are
#             implemented in <pkg>rpart</pkg> (shipped with base R) and <pkg>tree</pkg>.
#             An adaptation of <pkg>rpart</pkg> for multivariate responses
#             is available in package <pkg>mvpart</pkg>. The validity of
#             trees can be investigated via permutation approaches with package 
#             <pkg>rpart.permutation</pkg> and a tree algorithm fitting 
#             nearest neighbors in each node is implemented in package 
#             <pkg>knnTree</pkg>. For problems with binary input variables
#             the package <pkg>LogicReg</pkg> implements logic regression.
#             Graphical tools for the visualization of
#             trees are available in packages <pkg>maptree</pkg> and 
#             <pkg>pinktoe</pkg>.</li>
#      <li><i>Regularized and Shrinkage Methods</i>: Regression models with some
#             constraint on the parameter estimates can be fitted with the
#             <pkg>lasso2</pkg> and <pkg>lars</pkg> packages. The shrunken
#             centroids classifier and utilities for gene expression analyses are
#             implemented in package <pkg>pamr</pkg>.</li>
#      <li><i>Random Forests</i>: The reference implementation of the random
#             forest algorithm for regression and classification is available in 
#             package <pkg>randomForest</pkg>. Package <pkg>ipred</pkg> has bagging
#             for regression, classification and survival analysis as well as
#             bundling, a combination of multiple models via
#             ensemble learning.</li>
#      <li><i>Boosting</i>: Various forms of gradient boosting are
#             implemented in packages <pkg>gbm</pkg> and <pkg>boost</pkg>.</li>
#      <li><i>Support Vector Machines</i>: The function <tt>svm()</tt> from 
#             <pkg>e1071</pkg> offers an interface to the LIBSVM library and
#             package <pkg>kernlab</pkg> implements a flexible framework 
#             for kernel learning (including SVMs, RVMs and other kernel
#	     learning algorithms). An interface to the SVMlight implementation
#	     (only for one-against-all classification) is provided in package
#	     <pkg>klaR</pkg>.</li>
#      <li><i>Model selection and validation</i>: Package <pkg>e1071</pkg>
#             has function <tt>tune()</tt> for hyper parameter tuning and 
#             function <tt>errorest()</tt> (<pkg>ipred</pkg>) can be used for
#             error rate estimation. The cost parameter C for support vector
#             machines can be chosen utilizing the functionality of package
#             <pkg>svmpath</pkg>.</li>
#    </ul>
#  </info>
#
#  <packagelist>
#    <pkg>boost</pkg>
#    <pkg priority="core">e1071</pkg>
#    <pkg priority="core">gbm</pkg>
#    <pkg>ipred</pkg>
#    <pkg priority="core">kernlab</pkg>
#    <pkg>klaR</pkg>
#    <pkg>lars</pkg>
#    <pkg>lasso2</pkg>
#    <pkg>mvpart</pkg>
#    <pkg>pamr</pkg>
#    <pkg>rpart.permutation</pkg>
#    <pkg priority="core">randomForest</pkg>
#    <pkg priority="core">rpart</pkg>
#    <pkg>svmpath</pkg>
#    <pkg>tree</pkg>
#    <pkg priority="core">VR</pkg>
#  </packagelist>
#
#  <links>
#    <a href="http://www.boosting.org/">Boosting Research Site</a>
#  </links>
#
#</CRANTaskView>
