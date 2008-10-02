#!/usr/bin/env bash

DOT2GXL=dot2gxl
R_EXE=R

DOT=inst/dot/biocViewsVocab.dot
GXL=inst/dot/biocViewsVocab.gxl
RDA=data/biocViewsVocab.rda
SQLITE=data/biocViewsVocab.sqlite

$DOT2GXL $DOT > $GXL


R_SCRIPT="library('graph')"
R_SCRIPT="$R_SCRIPT; con <- file('$GXL', open='r')"
R_SCRIPT="$R_SCRIPT; biocViewsVocab <- fromGXL(con)"
R_SCRIPT="$R_SCRIPT; save(biocViewsVocab, compress=TRUE, file='$RDA')"
R_SCRIPT="$R_SCRIPT; close(con)"
R_SCRIPT="$R_SCRIPT; edges <- t(sapply(strsplit(edgeNames(biocViewsVocab), '~'), c))"
R_SCRIPT="$R_SCRIPT; colnames(edges) <- c('edgeFrom', 'edgeTo')"
R_SCRIPT="$R_SCRIPT; if(!require(RSQLite)) warning('DBI and RSQLite are required to dump onthology to database') else{"
R_SCRIPT="$R_SCRIPT; m <- dbDriver('SQLite')"
R_SCRIPT="$R_SCRIPT; con <- dbConnect(m, dbname='$SQLITE')" 
R_SCRIPT="$R_SCRIPT; res <- dbWriteTable(con, 'biocViews', as.data.frame(edges, stringsAsFactors=FALSE), row.names=FALSE, overwrite=TRUE)"
R_SCRIPT="$R_SCRIPT; if(!res) warning('Failed writing data to database')"
R_SCRIPT="$R_SCRIPT; res <- dbDisconnect(con)}"

echo "$R_SCRIPT" | $R_EXE --slave

echo "DONE"
