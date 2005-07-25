packAssoc2viewlist <- function (pael) 
{
    allconc <- sapply(pael, function(x) na.omit(c(x$top, x$second, 
        x$third)))
    ns <- sapply(allconc, length)
    pks <- rep(names(allconc), ns)
    split(pks, unlist(allconc))
}
