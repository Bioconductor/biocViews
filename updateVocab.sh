#!/usr/bin/env bash

DOT2GXL=dot2gxl
R_EXE=R

DOT=inst/dot/biocViewsVocab.dot
GXL=inst/dot/biocViewsVocab.gxl
RDA=data/biocViewsVocab.rda

$DOT2GXL $DOT > $GXL


R_SCRIPT="library('graph')"
R_SCRIPT="$R_SCRIPT; con <- file('$GXL', open='r')"
R_SCRIPT="$R_SCRIPT; biocViewsVocab <- fromGXL(con)"
R_SCRIPT="$R_SCRIPT; save(biocViewsVocab, compress=TRUE, file='$RDA')"
R_SCRIPT="$R_SCRIPT; close(con)"

echo "$R_SCRIPT" | $R_EXE --slave
echo "DONE"
