ROOT=biocViews

$(ROOT).dvi: $(ROOT).tex
	latex $(ROOT)
	latex $(ROOT)

$(ROOT).tex: $(ROOT).Rnw
	echo "library(tools); Sweave('$(ROOT).Rnw')" | R --no-save

edit:
	vi $(ROOT).Rnw
# add dvips option then
#psnup -4 -W128mm -H96mm -pletter -b1cm whh.ps whh4.ps

