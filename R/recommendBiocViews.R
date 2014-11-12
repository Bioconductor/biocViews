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
            stop("You have biocViews from multiple branches.")
    }
    
    if(length(find_branch)==0 & length(branch)==3){
            txt <- paste0("Incorrect biocViews in file & no branch specified. 
                          Cant recommend biocViews")
            stop(paste(strwrap(txt,exdent=2), collapse="\n"))
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
            stop(paste(strwrap(txt,exdent=2), collapse="\n"))
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
    dcf <- read.dcf(descr_file, c("Description", "Title", "Package"))
    words1 <- unique(unlist(strsplit(dcf, " ")))
    
    ## strategy 2- get biocViews of packages in depends field.
    pkgs <- read.dcf(descr_file, "Depends")
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
    
    if (length(words2)!=0) {
        words <- c(words1, words2)
    } else {
        words <- c(words1)
    }
    words
}

.wordsfromMANVIN <- function(pkgdir)
{
    ##strategy -3 man pages parsing.
    manfls <- list.files(file.path(pkgdir,"man"), full.names=TRUE, 
                         pattern="\\.Rd$")
    
    ##stragegy -4 vignette pages parsing.
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
#     current <- current[!is.na(current)]
#     current  <- unlist(strsplit(current, ", "))
#     current  <- unlist(strsplit(current, "\n"))
#     current  <- unlist(strsplit(current, ","))
    current <- .cleanupDependency(current)   
   
    if(length(current)==0 & missing(branch)){
        txt <- "No existing biocViews found in this package and cannot determine
             the branch of package to recommend biocViews"
        stop(paste(strwrap(txt,exdent=2), collapse="\n"))
    }
     
    if(!file.exists(file.path(pkgdir,"man")))
        stop("No man pages found.")
    
    if(!file.exists(file.path(pkgdir,"vignettes")))
        stop("No vignettes found.")
    
    dotterms <- .findBranchReadDot(current, branch)
    
    ### split "DecsisionTree" to "decision" , "tree" 
    terms <- sapply(dotterms, function(x){
        m <- gregexpr(pattern= "[[:upper:]]", text = x, ignore.case=FALSE)
        s1 <- unlist(regmatches(x,m))
        s2 <- unlist(strsplit(x, "[[:upper:]]"))
        if(length(s2)!=length(s1))
            s2 <- s2[-1]
        word<-function(s1,s2) paste0(s1,s2)
        mapply(word, s1,s2, USE.NAMES=FALSE) 
    }, simplify = TRUE)
    
    words1 <- .wordsfromDESCRIPTION(pkgdir)
    
    all_words<- .wordsfromMANVIN(pkgdir)
    
    # combine words from all sources and map
    words <- c(words1,all_words)
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
                         "miRNA","SNP","qPCR","SAGE","Genetics",
                         "GenomeAnnotation" )
    
    suggest_bioc <- setdiff(suggest_bioc,commonbiocViews)
    
     
    ## setdiff between current and suggested biocViews. 
    if(length(current)!=0){
        new_bioc <- setdiff(suggest_bioc, current)
    }else{
        new_bioc <- suggest_bioc
    }
    
    ## some pkgs have terms which do not belong to software branch. 
    remove <- setdiff(current, dotterms)
    
    list(current = paste(current, collapse=", "), 
         recommended = paste(new_bioc, collapse=", "),
         remove = paste(remove, collapse=", "))
}
