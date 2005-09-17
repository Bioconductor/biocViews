getCTVs <- function (vpal, vocab) 
{
    vn <- names(vpal)
    nv <- length(vn)
    out <- list()
    for (i in 1:length(vn)) {
        tmp <- makeCTV(vn[i], vn[i], "None", vpal[[i]], "None", 
            vocab)
        tf <- tempfile()
        saveXML(tmp, file = tf)
        out[[vn[i]]] <- read.ctv(tf)
        unlink(tf)
    }
    out
}
