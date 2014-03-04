##-------------helper functions
rm(list=ls())

biocViewMap <- function()
{
  webmap <- c(
    
    AssayDomains=NA_character_, 
    AssayTechnologies="Technology",   
    Bioinformatics=NA_character_,
    BiologicalDomains=NA_character_,
    ConnectTools="ThirdPartyClient",
    Enrichment=NA_character_,
    GraphsAndNetworks="GraphAndNetwork",
    HighThroughputSequencing="Sequencing",
    Methylseq="MethylSeq",
    MultipleComparisons="MultipleComparison",
    NetworkAnalysis="Network", 
    Networks="Network",
    NetworkVisualization="Visualization", 
    Regulation=NA_character_,
    RNAseq="RNASeq",
    Sequences=NA_character_, 
    Signaling= NA_character_
  )
  
  usermap <- c(    
    AffymetrixChip="OneChannel", 
    Affymetrix="OneChannel",
    BatchEffectAssessment="BatchEffect",
    ChiPseq="ChIPSeq", ChIPseq="ChIPSeq",
    ClusterValidation="Clustering",
    CopyNumberVariants="CopyNumberVariation",
    CNV="CopyNumberVariation",
    DataPreprocessing="Preprocessing",
    Design="ExperimentalDesign",
    DNAmethylation="DifferentialMethylation",
    DualChannel="TwoChannel",
    Flowcytometry="FlowCytometry", 
    FlowCytData="FlowCytometry",
    `Flow cytometry`="FlowCytometry",
    `High Throughput Sequencing`="Sequencing",
    genetics="Genetics",
    HighTroughputSequencingData="Sequencing",
    HighThroughputSequencingData="Sequencing",
    Microarrays="Microarray",
    MicroArray="Microarray",
    microRNA="miRNA",
    MRNAMicroarray="mRNAMicroarray",
    `Multiple Comparisons`="MultipleComparison",
    RIPseq="RIPSeq",
    RNAExpressionData="DifferentialExpression",
    SequenceAnnotation="GenomeAnnotation",
    SequencingMatching="SequenceMatching",
    `SNP.`="SNP",
    Statistics="StatisticalMethod",
    Technology=NA_character_,
    Visualisation="Visualization", 
    visualization="Visualization"
  )
  
  c(webmap,usermap)
}

readPathFromManifest <- function(rpacks, manifest)
{
  pkgs <- readLines(file.path(rpacks, manifest))
  
  pkgs <- sub("Package:[[:space:]]*([[:alnum:]\\.]+)[[:space:]]*$", "\\1",
              pkgs[grepl("Package:", pkgs)])
  fls <- sprintf(file.path(rpacks, "%s/DESCRIPTION"), pkgs)
  names(fls) <- pkgs
  fls <- fls[file.exists(fls)]
} 

readbiocViewsFromRpacks <- function(fls)
{
  otermsl <- lapply(fls, function(fl) {
    term <- read.dcf(fl, c("biocViews","BiocViews"))
    term <- term[!is.na(term)]
    if(length(term!=0))
      strsplit(term, "[[:space:]]*,[[:space:]]*")[[1]]
    else
      NA_character_
  })
  pkgterm <- data.frame(pkg = rep(names(otermsl), sapply(otermsl, length)),
                        term = unlist(unname(otermsl)),
                        stringsAsFactors=FALSE)
}

generatebiocViewsMap <- function(pkgterm, map)
{
  pkgterm$newterm <- pkgterm$term
  idx <- match(pkgterm$newterm, names(map))
  pkgterm$newterm[!is.na(idx)] <- unname(map[pkgterm$newterm[!is.na(idx)]])
  pkgterm
} 

readVersionFromRpacks <- function(versionPath)
{
  otermslVersion <- lapply(versionPath, function(ver) {
    dcf <- read.dcf(ver )
    v <- package_version(dcf[, "Version"])
    v0 = unclass(v)
    v0$Version[3] = v0$Version[3] +1
    class(v0) = class(v)
    c(as.character(v),as.character(v0))
  })
  
  ver <- data.frame(matrix(unlist(otermslVersion), 
                           nrow=length(otermslVersion), 
                           byrow=T,
                           dimnames=list(names(otermslVersion),c("oldVer","newVer"))))
  ver <- cbind(rownames(ver),ver )
  names(ver)<- c("pkg","oldVer","newVer")
  rownames(ver) <- NULL
  ver
}

readDot <- function(fl) 
{
  dot <- readLines(fl)
  dot <- dot[seq(grep("BiocViews -> Software", dot),
                 grep("BiocViews -> AnnotationData", dot) - 1)]
  sub(" *; *$", "", dot[grepl("^[[:space:][:alpha:]]+->", dot)])
}

getPathfromPkgName<- function(fls, pkglist)
{
  fls[which(names(fls) %in% pkglist)]
}

suggestbiocViews <- function(pkgterm, mer, biocViewdotfile, flag=TRUE,fls)
{
  
  ##read in dot file to get new terms
  dot <- readDot(biocViewdotfile)
  dotterms <- unique(unlist(strsplit(dot, " *-> *")))
  
  ##no biocViews?
  xx = sapply(split(is.na(pkgterm$newterm), pkgterm$pkg), 
              function(elt) sum(elt) == length(elt))
  any(xx)
  nobiocView <- xx[xx]
  names(nobiocView)
  pkgterm[which(pkgterm$pkg %in% names(xx[xx])),]
  
  #get the path for packages that do not have biocViews
  nobiocViewPath <- getPathfromPkgName(fls, names(nobiocView))
  
  sugbiocView <- lapply(nobiocViewPath, function(x){ 
    words <- unique(unlist(strsplit(read.dcf(x,c("Description","Title","Package"))," ")))
    idx <- which(tolower(dotterms) %in% tolower(words))
    dotterms[idx]
  })
  
  if(flag)
  {
    ##packages that have biocViews now!
    found <- sugbiocView[lapply(sugbiocView,length)>0]
    
    found <- lapply(found, function(x) paste(unlist(x),collapse=", " ))
    #add the suggested biocViews to mer. 
    idx <- match(names(found), mer$pkg)
    mer[idx,3]<- as.character(found)
    
    
  }else{
    #still do not have biocViews!
    realbad <- sugbiocView[lapply(sugbiocView,length)==0]
    
    #these files have no biocViews - manually add biocViews for them.
    mer <- mer[which(mer[,1] %in% names(realbad)),]
  }
  
  mer
  
}


##--------main function


newBiocViews <- 
    function(manifest,rpacks,biocViewdotfile, makeChanges=FALSE, resfilename)
{
  #The manifest file contains all the packages list. 
  
  # Read in all package names from here.
  fls <- readPathFromManifest(rpacks, manifest)
  cat("Total no of packages :",length(fls) )
  
  #get  the biocViews from all packages in the repository
  pkgterm <- readbiocViewsFromRpacks(fls)
  
  ##read in changes
  map <- biocViewMap()
  
  ##map the new/suggested biocViews to existing biocViews
  pkgterm <- generatebiocViewsMap(pkgterm, map)
  
  
  ## comma sepearated biocViews
  yy = lapply(split(pkgterm, pkgterm$pkg), 
              function(elt) {
                elt$term <- paste(elt$term,collapse=", ")
                elt$newterm <-  paste(na.omit(elt$newterm),collapse=", ") 
                unique(elt)
              })
  
  #represnt as a data.frame
  yes <- do.call(rbind.data.frame,yy)
  
  ## which packages had no change in their biocViews?
  nochange2 <- yes[which(yes$term==yes$newterm),]
  cat("no of packages notchanges at all :",length(nochange2[,1]) )
  
  
  ## which package had changes in their biocViews
  modified2 <- yes[which(yes$term!=yes$newterm),]
  cat("no of packages changed :",length(modified2[,2])  )
  
  #get packages whose version has to be bumped
  versionfls<- modified2[,1]
  
  #get the path for each of these packages
  versionPath <- getPathfromPkgName(fls, versionfls)
  
  # data.frame with package name, old followed by new version number.
  versiondf <- readVersionFromRpacks(versionPath)
  
  #merging 
  mer <- merge(modified2,versiondf, by="pkg")
  
  ##suggest biocViews for packages with no biocViews
  ## returns a data.frame for modified 
  mer <- suggestbiocViews(pkgterm, mer, biocViewdotfile,flag=TRUE,fls)
  
  ## which packages are realbad? still do not have biocViews - just write to file 
  badmer <- suggestbiocViews(pkgterm,mer, biocViewdotfile,flag=FALSE,fls)
  write.table(badmer,"badbiocViews.txt",sep="\t",quote=FALSE,row.names=FALSE)
  
  if(makeChanges)
  {
    ##how do we make the changes here?
  }else{
    write.table(mer, resfilename, sep="\t",quote=FALSE,row.names=FALSE)
  }  
}

makechanges<- function(filename)
{
    #filename <- "revisebiocViews.txt"
    revisemat <- read.table(filename, sep="\t",header=TRUE,
                            stringsAsFactors=FALSE)
    # no of packages to be changes
    pkglist <- nrow(revisemat)
    
    
    # first get the path for each package in file
    pkgpath <- file.path(rpacks,revisemat[,1],"DESCRIPTION")
    
    
    for (x in 1:nrow(revisemat)){
        cat(x,"\n")
        # open the description file
        data <- read.dcf(pkgpath[x])
        
        #bump the version number
        data[,"Version"] <- revisemat[x,"newVer"]
        
        ## four cases possible
        #1 - no biocViews eg:which(revisemat[,1]=="vtpnet") -476
        #2 - BiocViews eg: which(revisemat[,1]=="PSICQUIC") -348
        #3 - biocViews eg: which(revisemat[,1]=="a4") - 1
        #4- bioViews eg: which(revisemat[,1]=="EBSeq") -139
        
        wrongidx <- which(colnames(data) %in% c("BiocViews","bioViews","biocViews"))
        
        ## contains BiocViews or bioViews ( remove it!)
        if(length(wrongidx) != 0){
            cat("I am in !")
            data <- data[1, -wrongidx,drop=FALSE]
        }
        
        ## add biocViews to pkg
        data <- cbind(data,"biocViews"=revisemat[x,"newterm"])
        
        ##write to package
        write.dcf(data,file=pkgpath[x])
    }
    
}


# usage
# ## on rhino01
# ## devel
# 
# rpacks <- file.path("~/biosrc/Rpacks")
# manifest <- "bioc_2.14.manifest"
# biocViewdotfile <- "biocViewsVocab.dot"
# newBiocViews(manifest, rpacks, biocViewdotfile, 
#               makeChanges=FALSE,"revisebiocViews-devel.txt")
# 
# makechanges("revisebiocViews-devel.txt")
# 
# ## on rhino01
# ## release
# 
# rpacks <- file.path("~/Rpacks")
# manifest <- "bioc_2.14.manifest"
# biocViewdotfile <- "biocViewsVocab.dot"
# newBiocViews(manifest, rpacks, biocViewdotfile, 
#                 makeChanges=FALSE,"revisebiocViews-release.txt")
# 
# makechanges("revisebiocViews-release.txt")
