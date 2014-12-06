## Rscript for updating Experiment Data biocViews
## Dec 5th 2014.
library(biocViews)

rm(list=ls())
dirname <- "pkgs"
## read in all files and recommend new biocViews.
pkgnames <- list.files(dirname)
pkgnames <- pkgnames[!grepl("add_data.py",pkgnames)]
pkgnames <- pkgnames[!grepl(".manifest$",pkgnames)]
pkgnames <- pkgnames[!grepl("README.txt",pkgnames)]
pkgnames <- pkgnames[!grepl("-meat.sh$",pkgnames)]

genome_tbl <- rtracklayer::ucscGenomes(organism=TRUE)  

result <- lapply(pkgnames, function(x) {
    pkgdir <- file.path(dirname,x)
    message(x)
    ## add tryCatch!
    tryCatch({
        a <- recommendBiocViews(pkgdir, branch = "ExperimentData")
        message(a)
        a
    }, error=function(err) {
        warning(x, ": ", conditionMessage(err))
    })
})

current <- sapply(result, "[[", "current")
recommended <- sapply(result, "[[", "recommended")
remove <- sapply(result, "[[", "remove")

df<- data.frame(pkgnames=pkgnames, current=current, recommended=recommended,
           remove=remove, stringsAsFactors =FALSE)

webmap <- c(
    FlowCytometry="FlowCytometryData",   
    RNAExpressionData="RNASeqData",
    miRNAoverexpression="miRNAData",
    NormalTissue="Tissue"
)

final <- apply(df,1, function(z){
    c1 <- unlist(strsplit(as.character(z[2]),", "))
    c1 <- c(c1, as.character(webmap[c1][complete.cases(webmap[c1])]))
    rec <- unlist(strsplit(as.character(z[3]),", "))
    rem <- unlist(strsplit(as.character(z[4]),", "))
    fi <- unique(setdiff(c(c1,rec),rem))
    paste(fi, collapse=", ")
})


df2 <- data.frame(df, final=final, stringsAsFactors = FALSE )


terms <- getCurrentbiocViews()
expt <- terms$ExperimentData
nf <- lapply(as.character(df2$final), function(z) unlist(strsplit(z,", ")))
qr <- table(unlist(nf))
mat <- data.frame(names(qr), as.integer(qr))
mat <- mat[order(mat[,2]), ]
colnames(mat) <- c("Expt_biocViews", "occurence_in_pkgs")
write.table(mat, "count_of_biocViews_dec5.txt", sep="\t", quote=FALSE, 
            row.names=FALSE)

### update in svn

rm(list=ls())
df2 <- read.table("df2_dec5.txt", sep="\t", header=TRUE, 
                  stringsAsFactors = FALSE)
df2[64,5]<-"GEO"
dirname <- file.path(getwd(),"pkgs")
pkgnames <- list.files(dirname)
pkgnames <- pkgnames[!grepl("add_data.py",pkgnames)]
pkgnames <- pkgnames[!grepl(".manifest$",pkgnames)]
pkgnames <- pkgnames[!grepl("README.txt",pkgnames)]
pkgnames <- pkgnames[!grepl("-meat.sh$",pkgnames)]
changemat <- matrix(ncol=5, nrow=length(pkgnames))

reviseVersions <- function(v)
{
    vsp <- strsplit(v,"[.]")
    vsp$Version[3] <- as.integer(vsp$Version[3])+1
    paste(vsp$Version,collapse=".")
}

change=TRUE
for (i in 1:length(pkgnames)){
    pkg <- pkgnames[i]
    pkgdir <- file.path(dirname, pkg,"DESCRIPTION")
    data <- read.dcf(pkgdir, keep.white = TRUE)
    fi <- colnames(data)
    rm(data)
    data <- read.dcf(pkgdir, keep.white = fi)
    b_ind <- which(colnames(data)=="biocViews")
    if(length(b_ind)==0){
        oldbiocView <-""
        message(pkg)
        message("No biocViews in this package!!")
        mat=matrix(df2[i,"final"],nrow=1,ncol=1)
        newbiocView <- mat
        colnames(mat)<-"biocViews"
        data <- cbind(data,mat)
        oldVersion  <- data[,"Version"]
        newVersion <- reviseVersions(oldVersion)
        data[,"Version"] <- newVersion
    } else{
        oldbiocView <- gsub("\n","",data[,"biocViews"])
        newbiocView <- df2[i,"final"]
        if(oldbiocView!=newbiocView){
            data[,"biocViews"] <- newbiocView
        }
        oldVersion  <- data[,"Version"]
        newVersion <- reviseVersions(oldVersion)
        data[,"Version"] <- newVersion
    } 
    
    changemat[i,1] <- pkg
    changemat[i,2] <- oldbiocView
    changemat[i,3] <- newbiocView
    changemat[i,4] <- oldVersion
    changemat[i,5] <- newVersion
    if(change)
        write.dcf(data, pkgdir, keep.white=fi)  
}

write.table(changemat, "changemat_dec5.txt",sep="\t", 
            col.names=c("pkg","oldbiocView","newbiocView","oldVer","newVer"), 
            quote=FALSE, row.names=FALSE)



