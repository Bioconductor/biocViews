
#setClass("annotatedPackageRef",
#	representation(
#		packagename="character",
#		title="character",
#		maintainer="character",
#		vocEnv="environment",
#		terms="character")) 
		
packAssoc <- function(packlist, vocGraph, root="vocRoot") {
 topl <- adj(vocGraph, root)[[1]]
 out <- list()
 for (i in packlist) {
   cat("top level concept for", i, "\n")
   out[[i]] <- list()
   while (length(out[[i]]$top) == 0)  {
       out[[i]]$top <- pickItems( topl )
       if (length(out[[i]]$top) == 0) warning("you must pick one top level term")
       }
   sec <- unlist ( adj( vocGraph, out[[i]]$top ) )
   if (length(sec) > 0) {
       cat("second level concept for", i, "\n")
       out[[i]]$second <- pickItems( as.character(unlist(sec)) ) #sec[ menu(sec, graphics=TRUE) ]
       }
   else out[[i]]$second <- NA
   if (!any(is.na(out[[i]]$second))) thir <- unlist( adj( vocGraph, out[[i]]$second ) )
   else thir <- NULL
   if (length(thir) > 0) {
       cat("third level concept for", i, "\n")
       out[[i]]$third <- pickItems( as.character(unlist(thir)) ) #thir[ menu( athir, graphics=TRUE) ]
       }
   else out[[i]]$third <- NA
   pdstuff <- packageDescription( i, fields=c("Package", "Title", "Maintainer", "Description") )
   out[[i]]$maintainer <- pdstuff$Maintainer
   out[[i]]$packagename <- pdstuff$Package
   killnl <- function(x) gsub("\\\n", " ", x)
   out[[i]]$desc <- killnl(pdstuff$Description)
   out[[i]]$title <- pdstuff$Title
   cat("Continue? Type 1 if yes, 0 otherwise:")
   xx <- scan(n=1)
   if (xx != 1) return(out)
 }
 out
}

