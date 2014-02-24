
## ----style, eval=TRUE, echo=FALSE, results="asis"------------------------
BiocStyle::latex()


## ----preliminaries, echo=FALSE-------------------------------------------
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

getPathfromPkgName<- function(pkglist)
{
  fls[which(names(fls) %in% pkglist)]
}



## ----code-1--------------------------------------------------------------
##get the listof packages from manifest file in Rpacks
rpacks <- file.path("C:","Users","sarora.FHCRC","Documents","Rpacks")
manifest <- "bioc_2.14.manifest"

fls <- readPathFromManifest(rpacks, manifest)


## ----code2---------------------------------------------------------------
#this will read in all the biocViews from each package
pkgterm <- readbiocViewsFromRpacks(fls)


## ----code3---------------------------------------------------------------
## read in  biocViews map
map <- biocViewMap()
as.data.frame(map)


## ----code-4--------------------------------------------------------------
## revise biocViews
pkgterm <- generatebiocViewsMap(pkgterm, map)


## ----Danfile-------------------------------------------------------------
## comma sepearated biocViews
yy = lapply(split(pkgterm, pkgterm$pkg), 
            function(elt) {
              elt$term <- paste(elt$term,collapse=", ")
              elt$newterm <-  paste(na.omit(elt$newterm),collapse=", ") 
              unique(elt)
            })

#represnt as a data.frame
yes <- do.call(rbind.data.frame,yy)


## ----code-5--------------------------------------------------------------
## which packages had no change in their biocViews?
nochange2 <- yes[which(yes$term==yes$newterm),]
length(nochange2[,1])


## ----modified------------------------------------------------------------
## which package had changes in their biocViews
modified2 <- yes[which(yes$term!=yes$newterm),]
length(modified2[,2])


## ----version-------------------------------------------------------------
#get packages whose version has to be bumped
versionfls<- modified2[,1]

#get the path for each of these packages
versionPath <- getPathfromPkgName(versionfls)

# data.frame with package name, old followed by new version number.
versiondf <- readVersionFromRpacks(versionPath)

#merging 
mer <- merge(modified2,versiondf, by="pkg")


## ----nobiocViews------------------------------------------------------------------
xx = sapply(split(is.na(pkgterm$newterm), pkgterm$pkg), 
            function(elt) sum(elt) == length(elt))
any(xx)
nobiocView <- xx[xx]
names(nobiocView)
pkgterm[which(pkgterm$pkg %in% names(xx[xx])),]


## ----suggest-------------------------------------------------------------
##read in the biocViews from dot file.
dirpath <- file.path("C:","Users","sarora.FHCRC","Documents","sandbox",
                     "project biocviews","19feb2014")
dot <- readDot(file.path(dirpath, "biocViewsVocab.dot"))
dotterms <- unique(unlist(strsplit(dot, " *-> *")))


nobiocViewPath <- getPathfromPkgName(names(nobiocView))

getDescription <- function(package)
{
  lapply(package, function(x) read.dcf(x,"Description"))
}

sugbiocView <- lapply(nobiocViewPath, function(x){ 
  words <- unique(unlist(strsplit(read.dcf(x,c("Description","Title","Package"))," ")))
  idx <- which(tolower(dotterms) %in% tolower(words))
  dotterms[idx]
  })


##packages that have biocViews now!
found <- sugbiocView[lapply(sugbiocView,length)>0]

found <- lapply(found, function(x) paste(unlist(x),collapse=", " ))
#add the suggested biocViews to mer. 
idx <- match(names(found), mer$pkg)
mer[idx,3]<- as.character(found)

#still do not have biocViews!
realbad <- sugbiocView[lapply(sugbiocView,length)==0]

#these files have no biocViews - manually add biocViews for them.
badmer <- mer[which(mer[,1] %in% names(realbad)),]


## ----write---------------------------------------------------------------
#write file for manual analysis
setwd(file.path("C:","Users","sarora.FHCRC","Documents","sandbox",
                     "project biocviews","19feb2014"))
write.table(nochange2[1:2], "nochangebiocViews.txt",quote=FALSE,
            sep="\t",row.names=FALSE)
write.table(badmer, "badbiocViews.txt",quote=FALSE,sep="\t",row.names=FALSE)
write.table(mer, "revisebiocViews.txt",quote=FALSE,sep="\t",row.names=FALSE)


## ----sessionInfo---------------------------------------------------------
sessionInfo()


