topwrap <- function (gra) 
{
    topt <- adj(gra, "vocRoot")[[1]]
    sapply(topt, function(x) paste("<li><a href=",x, ".html>", x, "</a>", sep = ""))
}
