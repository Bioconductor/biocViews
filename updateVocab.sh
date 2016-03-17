#!/usr/bin/env bash

if test -z "${R_HOME}"; then
    echo "usage:"
    echo "    R CMD ./updateVocab.sh"
    exit 1
fi

DOT2GXL=dot2gxl

DOT=inst/dot/biocViewsVocab.dot
GXL=inst/dot/biocViewsVocab.gxl
RDA=data/biocViewsVocab.rda
SQLITE=inst/extdata/biocViewsVocab.sqlite

rm -f $RDA $SQLITE

$DOT2GXL $DOT > $GXL

echo "library('graph')
    con <- file('$GXL', open='r')
    biocViewsVocab <- fromGXL(con)
    save(biocViewsVocab, compress=TRUE, file='$RDA')
    close(con)
    edges <- t(sapply(strsplit(edgeNames(biocViewsVocab), '~'), c))
    colnames(edges) <- c('edgeFrom', 'edgeTo')
    if(!require(RSQLite)) {
        warning('DBI and RSQLite are required to dump onthology to database')
    } else {
        m <- dbDriver('SQLite')
        con <- dbConnect(m, dbname='$SQLITE')
        res <- dbWriteTable(con, 'biocViews',
            as.data.frame(edges, stringsAsFactors=FALSE),
            row.names=FALSE, overwrite=TRUE)
        if(!res) warning('Failed writing data to database')
        res <- dbDisconnect(con)
    }" | "${R_HOME}/bin/R" --slave

rm -f $GXL

echo "DONE"
