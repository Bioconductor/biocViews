rm(list=ls())
readPathFromManifest <- function(rpacks, manifest)
{
    pkgs <- readLines(file.path(rpacks, manifest))
    
    pkgs <- sub("Package:[[:space:]]*([[:alnum:]\\.]+)[[:space:]]*$", "\\1",
                pkgs[grepl("Package:", pkgs)])
    fls <- sprintf(file.path(rpacks, "%s/DESCRIPTION"), pkgs)
    names(fls) <- pkgs
    fls <- fls[file.exists(fls)]
} 

readDot <- function(fl) 
{
    dot <- readLines(fl)
    dot <- dot[seq(grep("BiocViews -> Software", dot),
                   grep("BiocViews -> AnnotationData", dot) - 1)]
    sub(" *; *$", "", dot[grepl("^[[:space:][:alpha:]]+->", dot)])
}

findbiocViews<- function(file, dotterms, terms)
{   
    ## strategy 1- parse the words in the DESCRIPTION file to get
    ## biocViews
    dcf <- read.dcf(file, c("Description", "Title", "Package"))
    words1 <- unique(unlist(strsplit(dcf, " ")))
    
    ## strategy 2- get biocViews of packages in depends field.
    pkgs <- read.dcf(file, "Depends")
    pkgs <- unlist(strsplit(gsub("[0-9.()>= ]", "", pkgs), ",")) 
    
    x <- readLines(url("http://bioconductor.org/js/versions.js"))
    dv <- x[grep("develVersion", x)]
    devel_version <- strsplit(dv, '"')[[1]][2]
    repos <- c("bioc", "data/annotation", "data/experiment")
    urls <- paste0("http://bioconductor.org/packages/", devel_version,
                   "/bioc/VIEWS")
    
    words2 <- character()
    con <- url(urls) 
    biocpkgs <-  read.dcf(con,"Package")
    idx <- which(biocpkgs %in% pkgs)
    if (length(idx)!=0) {
        wrd <- read.dcf(con, "biocViews")[idx]
        wrd <- unique(unlist(strsplit(wrd, ", ")))
        words2 <- c(words2,wrd)
    }
    close(con)
    
    ##stragegy -3 man pages parsing.
    manfls <- list.files(file.path(gsub("/DESCRIPTION","",file),"man"), 
                         full.names=TRUE,pattern="\\.Rd$")
        
    ##stragegy -4 vignette pages parsing.
    vinfls <- list.files(file.path(gsub("/DESCRIPTION","",file),"vignettes"), 
                         full.names=TRUE, pattern="\\.Rnw$")
    
    allfls <- c(manfls,vinfls)
    
    if(length(allfls)==0){
        all_words <- NA
    }else{
        q <- lapply(allfls, readLines)
        temp <- unlist(strsplit(q[[1]], "[[:punct:]]", perl = TRUE)) 
        temp <- unlist(strsplit(temp, "[[:space:]]", perl = TRUE)) 
        all_words <- unique(temp[temp != ""])
    }
   
    # combine words from all sources and map
    if (length(words2)!=0) {
        words <- c(words1, words2,all_words)
    } else {
        words <- c(words1,all_words)
    }
    words <- unique(unlist(strsplit(words,"\n")))
    
    ## match against biocViews. 
    idx <- which(tolower(dotterms) %in% tolower(words))
    temp <- dotterms[idx]
    
    ## only if both "decision" and "tree" are found add biocView "DecisionTree" 
    split_word <- mapply(FUN= function(x,y){
        i <- which(tolower(x) %in% tolower(words))
        ifelse(length(i)==length(x), y, NA)
    }, terms, names(terms), USE.NAMES=FALSE)
    
    suggest_bioc <- unique(c(split_word[complete.cases(split_word)], temp))

    commonbiocViews <- c("Infrastructure","Software",
                         "AssayDomain","BiologicalQuestion","Infrastructure",
                         "ResearchField","StatisticalMethod","Technology",
                         "Annotation","Visualization","DataRepresentation",
                         "miRNA","SNP","qPCR","SAGE","Genetics" )
    
    suggest_bioc <- setdiff(suggest_bioc,commonbiocViews)
    
    ## existing biocView in test package?
    current <- read.dcf(file, c("biocViews","BiocViews"))
    current <- current[!is.na(current)]
        
    ## setdiff between current and suggested biocViews. 
    if(length(current)!=0){
        current  <- strsplit(current, "[[:space:]]*,[[:space:]]*")[[1]]
        new_bioc <- setdiff(suggest_bioc, current)
    }else{
        new_bioc <- NA_character_
    }
    
    ## some pkgs have terms which do not belong to software branch. 
    remove <- setdiff(current, dotterms)
    
    ##maintainer - email
    email <- read.dcf(file, "Maintainer")
    
    list(current = paste(current, collapse=", "), 
         new = paste(new_bioc, collapse=", "),
         remove = paste(remove, collapse=", "),
         email = paste(unlist(strsplit(email,"\n")),collapse=" ") )
}


library(BiocParallel)
rpacks <- file.path("~/Rpacks")
#rpacks <- file.path("home","sarora","Rpacks") # on rhino
manifest <- "bioc_3.0.manifest"
biocViewdotfile <- system.file("dot","biocViewsVocab.dot", package="biocViews")

# read path from Rpacks
fls <- readPathFromManifest(rpacks, manifest)
cat("Total no of packages :",length(fls) )

#read biocViews from dot file. 
dot <- readDot(biocViewdotfile)
dotterms <- unique(unlist(strsplit(dot, " *-> *")))

### split "DecsisionTree" to "decision" , "tree" 
terms <- sapply(dotterms, function(x){
    m <- gregexpr(pattern= "[[:upper:]]", text = x, ignore.case=FALSE)
    s1 <- unlist(regmatches(x,m))
    s2 <- unlist(strsplit(x, "[[:upper:]]"))
    s2 <- s2[-1]
    word<-function(s1,s2) paste0(s1,s2)
    mapply(word, s1,s2, USE.NAMES=FALSE) 
}, simplify = TRUE)

## suggest biocViews. 
system.time(result <- lapply(fls[1:20], findbiocViews, dotterms, terms))

newbioc2 <- do.call(rbind.data.frame,result)
write.table(newbioc2,"suggestedbiocViews_rpacks.txt",sep="\t", quote=FALSE)


