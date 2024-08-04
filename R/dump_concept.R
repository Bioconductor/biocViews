#' produce character stream for use with Protege entities/class hierarchy ingestion 
#' based on biocViews subgraphs
#' @param x character(1) name of a node in the biocViewsVocab graph
#' @param edob a list as produced by graph::edges
#' @return a character atom with x abutting left and edge targets tab-indented
#' @examples
#' cat(substring(dump_concept("ResearchField"), 1, 152), "\n")
#' @export
dump_concept = function (x, edob = edges(biocViewsVocab)) 
{
    data("biocViewsVocab", package = "biocViews")
    paste(x, "\n\t", paste(edob[[x]], collapse = "\n\t"), collapse = "\n\t", 
        "\n", sep = "")
}
