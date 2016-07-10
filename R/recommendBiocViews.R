.cleanupDependency <- function(input)
{
    if (is.null(input)) return(character(0))
    output <- gsub("\\s", "", input)
    output <- gsub("\\([^)]*\\)", "", output)
    res <- strsplit(output, ",")[[1]]
    unique(res[which(res != "R")])
}

.parseDot <- function(dot)
{
    dot <- sub(" *; *$", "", dot[grepl("^[[:space:][:alpha:]]+->", dot)])
    unique(unlist(strsplit(dot, " *-> *")))
}

getCurrentbiocViews <- function()
{
    #read biocViews from dot file. 
    biocViewdotfile <- system.file("dot","biocViewsVocab.dot", 
                                   package="biocViews")
    
    if(!file.exists(biocViewdotfile))
        stop("Package biocViews not found.")
    
    dot <- readLines(biocViewdotfile)
    
    Software <- dot[seq(grep("BiocViews -> Software", dot),
                        grep("BiocViews -> AnnotationData", dot) - 1)]
    AnnotationData <- dot[seq(grep("BiocViews -> AnnotationData", dot),
                              grep("BiocViews -> ExperimentData", dot) - 1)]
    ExperimentData <- dot[seq(grep("BiocViews -> ExperimentData", dot), 
                              length(dot),1)]
    
    Software <- .parseDot(Software)
    ExperimentData <- .parseDot(ExperimentData)
    AnnotationData <- .parseDot(AnnotationData)
    list(Software= Software ,ExperimentData= ExperimentData, AnnotationData= 
             AnnotationData)
}


.findBranchReadDot <- function(current, branch)
{
    ans <- getCurrentbiocViews()
    
    Software <- ans$Software
    ExperimentData <- ans$ExperimentData
    AnnotationData <- ans$AnnotationDat
    
    find_branch <- NULL
    if(length(current) != 0){
        idx<- list(Software = match(current, Software), 
                   AnnotationData = match(current, AnnotationData),
                   ExperimentData = match(current, ExperimentData))
        atrue <- sapply(idx, function(x) any(!is.na(x))) #which branch has hit 
        find_branch <- names(which(atrue==TRUE))
        if(length(find_branch)>1)
            message("You have biocViews from multiple branches.")
    }
    
    if(length(find_branch)==0 & length(branch)==3){
            txt <- paste0("Incorrect biocViews in file & no branch specified. 
                          Cant recommend biocViews")
            message(paste(strwrap(txt,exdent=2), collapse="\n"))
    }
        
    if(length(branch)==3 & length(find_branch)==1)
    {
        branch <- find_branch
    }
        
    if( length(branch)==1 & length(find_branch)==1)
    {
        if( length(branch)!=3 & (tolower(branch)!=tolower(find_branch))){
            txt <- paste0("You have specified ",branch," branch but your 
                           package contains biocViews from ",find_branch, 
                          " branch.")
            message(paste(strwrap(txt,exdent=2), collapse="\n"))
        }
    }
    # return appropriate dot terms based on branch. 
    if (tolower(branch)=="software")
            returndot <- Software
    else if(tolower(branch)=="experimentdata")
        returndot <- ExperimentData
    else
        returndot <- AnnotationData
    
    
    returndot
}

.wordsfromDESCRIPTION <- function(pkgdir)
{
    ## strategy 1- parse the words in the DESCRIPTION file to get
    ## biocViews
    descr_file <- file.path(pkgdir,"DESCRIPTION")
    dcf <- read.dcf(descr_file, c("Description", "Title", "Package","biocViews"))
    words1 <- unique(unlist(strsplit(dcf, " ")))
    
    ## strategy 2- get biocViews of packages in depends field.
    pkgs <- read.dcf(descr_file, "Depends")
    pkgs <- unlist(strsplit(gsub("[0-9.()>= ]", "", pkgs), ",")) 
    
    urls <- .getBioCDevelUrl(devel=TRUE, branch="software")
    
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
    
    if (length(words2)!=0) {
        words <- c(words1, words2)
    } else {
        words <- c(words1)
    }
    words
}

.wordsfromMANVIN <- function(pkgdir, man, vig)
{
    manfls <- character(0)
    vinfls <- character(0)
    
    ##strategy -3 man pages parsing.
    if(man)
        manfls <- list.files(file.path(pkgdir,"man"), full.names=TRUE, 
                         pattern="\\.Rd$")
    
    ##stragegy -4 vignette pages parsing.
    if(vig)
        vinfls <- list.files(file.path(pkgdir,"vignettes"), full.names=TRUE, 
                         pattern="\\.Rnw$")
    
    allfls <- c(manfls,vinfls)
    
    if(length(allfls)==0){
        all_words <- NA
    }else{
        q <- lapply(allfls, readLines)
        temp <- unlist(strsplit(q[[1]], "[[:punct:]]", perl = TRUE)) 
        temp <- unlist(strsplit(temp, "[[:space:]]", perl = TRUE)) 
        all_words <- unique(temp[temp != ""])
    }
    all_words
}

recommendBiocViews <- 
    function(pkgdir, branch= c("Software", "AnnotationData", "ExperimentData"))
{   
    if(!file.exists(pkgdir))
        stop("Package Directory not found.")
    
    if(!file.exists(file.path(pkgdir,"DESCRIPTION")))
        stop("No DESCRIPTION file found.")
    
    ## existing biocView in test package?
    current <- read.dcf(file.path(pkgdir,"DESCRIPTION"), c("biocViews",
                                                           "BiocViews"))
    current <- .cleanupDependency(current)   
   
    if(length(current)==0 & missing(branch)){
        txt <- "No existing biocViews found in this package and cannot determine
             the branch of package to recommend biocViews"
        stop(paste(strwrap(txt,exdent=2), collapse="\n"))
    }
    
    words1 <- .wordsfromDESCRIPTION(pkgdir)
    
    m <- file.exists(file.path(pkgdir,"man"))
    v <- file.exists(file.path(pkgdir,"vignettes"))
    
    man <- character(0)
    vig <- character(0)
    
    if(all(m,v)){
        all_words<- .wordsfromMANVIN(pkgdir, man=TRUE, vig=TRUE)
    } else{
        if(!m){
            message("No man pages found.") 
            all_words<- .wordsfromMANVIN(pkgdir, man=FALSE, vig=TRUE)
        }
        if(!v){
            message("No vignettes found.")
            all_words<- .wordsfromMANVIN(pkgdir, man=TRUE, vig=FALSE)
        }
    }
    words1 <- c(words1,all_words)
    words1 <- unlist(sapply(words1,.cleanupDependency, USE.NAMES = FALSE) )
    dotterms <- .findBranchReadDot(current, branch)
    
    ### split "DecsisionTree" to "decision" , "tree" 
    terms <- sapply(dotterms, function(x){
        m <- gregexpr(pattern= "[[:upper:]]", text = x, ignore.case=FALSE)
        s1 <- unlist(regmatches(x,m))
        s2 <- unlist(strsplit(x, "[[:upper:]]"))
        if(length(s2)!=length(s1))
            s2 <- s2[-1]
        word<-function(s1,s2) paste0(s1,s2)
        ans <- mapply(word, s1,s2, USE.NAMES=FALSE)
        if(length(ans)==0)
            ans <- x
        ans
    }, simplify = TRUE)
    
    terms <- lapply(terms, function(z){
        z<- setdiff(z,"Data")
        unlist(strsplit(z,"_")) 
    })

    if(branch=="ExperimentData")
    {
        terms$CpGIslandData <- c("cpg", "island")
        terms$GEO <- "GEO"
        terms$HapMap <- "HapMap"
        terms$SNPData <- "SNP"
        terms$DNASeqData <- c("DNA","Seq")
        terms$RNASeqData <- c("RNA","Seq")
        terms$ChIPSeqData <- c("ChIP","Seq")        
        terms$RIPSeqData <- c("RIP","Seq")
        terms$COPDData <-"COPD"
        terms$qPCRData <- "pcr"
        terms$SAGEData <-"sage"                
    }
    
    # combine words from all sources and map
    words1 <- unique(unlist(strsplit(words1,"\n")))
    words1 <- unique(unlist(strsplit(words1,"-")))
    words1 <- unique(unlist(strsplit(words1,"_")))
    words1 <- gsub("[.]","",words1)
    
     
    ## match against biocViews. 
    idx <- which(tolower(dotterms) %in% tolower(words1))
    temp <- dotterms[idx]
    
    ## only if both "decision" and "tree" are found add biocView "DecisionTree" 
    split_word <- mapply(FUN= function(x,y){
        i <- which(tolower(x) %in% tolower(words1))
        ifelse(length(i)==length(x), y, NA)
    }, terms, names(terms), USE.NAMES=FALSE)
    
       
    suggest_bioc <- unique(c(split_word[complete.cases(split_word)], temp))
    
    commonbiocViews <- c("Infrastructure","Software",
                         "AssayDomain","BiologicalQuestion","Infrastructure",
                         "ResearchField","StatisticalMethod","Technology",
                         "Annotation","Visualization","DataRepresentation",
                         "miRNA","SNP","qPCR","SAGE","Genetics",
                         "GenomeAnnotation",
                         "SpecimenSource","OrganismData",
                         "DiseaseModel","TechnologyData","AssayDomainData",
                         "RepositoryData")
    
    suggest_bioc <- setdiff(suggest_bioc,commonbiocViews)
    
     
    ## setdiff between current and suggested biocViews. 
    if(length(current)!=0){
        new_bioc <- setdiff(suggest_bioc, current)
    }else{
        new_bioc <- suggest_bioc
    }
    
    ## some pkgs have terms which do not belong to software branch. 
    remove <- c(intersect(current, commonbiocViews), setdiff(current, dotterms))
    
    
    list(current = paste(current, collapse=", "), 
         recommended = paste(new_bioc, collapse=", "),
         remove = paste(remove, collapse=", "))
}


.getBioCDevelUrl <- 
    function(devel=TRUE, branch) 
{
    con <- url("http://bioconductor.org/js/versions.js")
    x <- readLines(con)
    pattern <- ifelse(devel, "develVersion", "releaseVersion")
    dv <- x[grep(pattern, x)]
    devel_version <- strsplit(dv, '"')[[1]][2]
    repos <- switch(tolower(branch), 
           software="/bioc/", 
           experimentdata="/data/experiment/",
           annotationdata="/data/annotation/")
    close(con)
    paste0("http://bioconductor.org/packages/", devel_version, repos,
                   "VIEWS")
}


recommendPackages <-
    function(biocViews, use.release=TRUE, intersect.views=TRUE) 
{
    if(length(biocViews)==0)  # return avaialbel biocViews
        stop("Input some biocViews to get recommended packages.")
        
    toMatch <- paste(biocViews, collapse="|")
    
    ## check if the input biocViews are  defined by us. 
    existingbiocViews <- getCurrentbiocViews()
    match <- sapply(existingbiocViews, function(x){
        length(unique(grep(toMatch, x, ignore.case=TRUE)))
    }) 
      
    if(all(match==0L))
        stop("See: http://bioconductor.org/packages for valid biocViews")
    
    ## which branch do these biocViews belong to ?
    branch <- names(match)[match != 0L]
    if (length(branch) != 1L)
        stop("Input biocViews belong to branches ", 
             paste(sQuote(branch), collapse=", "),
             "; choose from 1 branch only")
        
    ## recommed packages based on branch 
    url <- .getBioCDevelUrl(devel=!use.release, branch)

    con <- url(url)
    tbl <- read.dcf(con, fields=c("Package", "biocViews"))
    close(con)
    
    ## get child biocViews of input biocView
    ## eg: if biocView is 'Alignment' then we should get packages tagged 
    ## with 'MultipleSequenceAlignment' also! 
    biocViews <- c(biocViews, .getChildEdgeFromDot(biocViews))
    
    idx0 <- sapply(tbl[,"biocViews"], function(table, x) {
        y <- gsub("\n", " ", table)
        y <- unlist(strsplit(y, ","))
        y <- gsub("^\\s+|\\s+$", "", y) # remove trailing/leading white spaces
        tolower(x) %in% tolower(y)
    } , biocViews) 
    
    if(length(biocViews)==1L){
        ## a list is returned. No operation needs to be done 
        return(tbl[idx0, "Package"])
    } 
    
    ## if intersect.views = TRUE then 'and' operation is carried out.
    ## eg: Packages tagged with both biocView 'a' and 'b' will be resturned.
    
    colnames(idx0) <- tbl[,"Package"]
    if (intersect.views) 
        pkg <- colnames(idx0)[colSums(idx0)==length(biocViews)] # and operation 
    else{
        pkg <- colnames(idx0)[colSums(idx0)!=0]   # or operation
    }
    
    pkg            
}

.getChildEdgeFromDot <- function(biocView) {
    ans <- .getChildren(biocView)
    ans <- unlist(ans)
    names(ans) <- NULL
    ans[!(ans %in% "1")]
}


.getChildren <- function(biocView) {
    data(biocViewsVocab)
    ans <- unlist(edges(biocViewsVocab, biocView))
    if(length(ans)==0)
        return("1")
    else
        return(c(ans, .getChildren(ans)))
}




