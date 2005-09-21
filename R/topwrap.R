.topwrap.OLD <- function (gra) 
{
    topt <- adj(gra, "vocRoot")[[1]]
    sapply(topt, function(x) paste("<li><a href=",x, ".html>", x, "</a>\n", sep = ""))
}

topwrap <- function (gra) 
{
    topt <- adj(gra, "vocRoot")[[1]]
      ostr <- list()
    j <- 0
    for (i in 1:length(topt)) {
      j <- j+1
      ostr[[j]] <- paste("<li><a href=",topt[i], ".html>", topt[i], "</a>\n", sep = "")
      nn <- adj(gra, topt[i])[[1]]
      if (length(nn) > 0) {
         j <- j+1
ostr[[j]] <- paste("<ul><li> subviews:", paste("<a href=", nn, ".html>", nn, "</a>", sep="", collapse=", "), "</ul>\n")
         }
    }
    unlist(ostr)
}
