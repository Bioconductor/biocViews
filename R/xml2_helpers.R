## xml2_helpers.R
##
## Compatibility wrappers that replicate the XML package's stateful DOM builder
## (xmlOutputDOM / xmlTree) and xmlNode / saveXML on top of xml2.
##
## The original code used three XML idioms:
##
##   1. xmlOutputDOM(tag, attrs) / xmlTree(tag)   -- stateful builder
##      Builder methods: $addTag(), $addNode(), $closeTag(), $value()
##
##   2. xmlNode(tag, ..., attrs)  -- create a standalone xml2 node
##
##   3. saveXML(node, file, prefix="")  -- serialize to file or string
##
## All three are re-implemented below so that the rest of the source code
## needs only minimal mechanical edits.

## ---------------------------------------------------------------------------
## 1.  xmlNode() replacement
## ---------------------------------------------------------------------------
## XML::xmlNode(name, ..., attrs = NULL)
##   children / text content are passed as un-named or named '...' arguments.
##   'attrs' is a named character vector.
##
## We return an xml2 node (xml_node).  Because xml2 builds nodes as part of a
## document, we keep a lightweight parent document.

xmlNode <- function(name, ..., attrs = NULL) {
    doc  <- xml2::xml_new_document()
    root <- xml2::xml_add_child(doc, name)

    if (!is.null(attrs) && length(attrs) > 0) {
        for (nm in names(attrs))
            xml2::xml_attr(root, nm) <- attrs[[nm]]
    }

    args <- list(...)
    ## If there is only a single character child and no xml_node children,
    ## use xml_set_text (produces a plain text node).  When the args are mixed
    ## (character + xml_node), wrap bare character strings in <span> so that
    ## inline separators (e.g. ", ") are preserved in the HTML output.
    node_children <- vapply(args, inherits, logical(1), "xml_node")
    mixed <- any(node_children) && any(!node_children)

    txt_acc <- character()
    for (child in args) {
        if (inherits(child, "xml_node")) {
            xml2::xml_add_child(root, child)
        } else if (is.character(child)) {
            txt <- paste0(child, collapse = "")
            if (nzchar(txt)) {
                if (mixed) {
                    sp <- xml2::xml_add_child(root, "span")
                    xml2::xml_set_text(sp, txt)
                } else {
                    txt_acc <- c(txt_acc, txt)
                }
            }
        }
    }
    if (!mixed && length(txt_acc))
        xml2::xml_set_text(root, paste0(txt_acc, collapse = ""))
    }
    root
}

## ---------------------------------------------------------------------------
## 2.  xmlOutputDOM() / xmlTree() replacement
## ---------------------------------------------------------------------------
## Both functions return an environment that exposes:
##   $addTag(name, ..., attrs, close)  -- open (and optionally close) a tag
##   $addNode(node)                    -- append an xml_node child
##   $closeTag()                       -- close the most-recently opened tag
##   $value()                          -- return the root xml_node
##
## The stateful part is a stack of open nodes held in the environment.

.makeXmlDomBuilder <- function(rootTag, attrs = NULL) {
    doc  <- xml2::xml_new_document()
    root <- xml2::xml_add_child(doc, rootTag)

    if (!is.null(attrs) && length(attrs)) {
        for (nm in names(attrs))
            xml2::xml_attr(root, nm) <- attrs[[nm]]
    }

    ## stack: top of stack is the currently open node
    stack <- list(root)

    current <- function() stack[[length(stack)]]
    push    <- function(n) stack[[length(stack) + 1L]] <<- n
    pop     <- function() stack[[length(stack)]] <<- NULL

    ## addTag(name, text?, attrs = NULL, close = TRUE)
    addTag <- function(name, ..., attrs = NULL, close = TRUE) {
        node <- xml2::xml_add_child(current(), name)

        if (!is.null(attrs) && length(attrs) > 0) {
            for (nm in names(attrs))
                xml2::xml_attr(node, nm) <- attrs[[nm]]
        }

        args <- list(...)
        node_children <- vapply(args, inherits, logical(1), "xml_node")
        mixed <- any(node_children) && any(!node_children)

        txt_acc <- character()
        for (child in args) {
            if (inherits(child, "xml_node")) {
                xml2::xml_add_child(node, child)
            } else if (is.character(child)) {
                txt <- paste0(child, collapse = "")
                if (nzchar(txt)) {
                    if (mixed) {
                        sp <- xml2::xml_add_child(node, "span")
                        xml2::xml_set_text(sp, txt)
                    } else {
                        txt_acc <- c(txt_acc, txt)
                    }
                }
            }
        }
        if (!mixed && length(txt_acc))
            xml2::xml_set_text(node, paste0(txt_acc, collapse = ""))

        if (!close) {
            push(node)
        }
        invisible(NULL)
    }

    ## addNode(node)  -- append a pre-built xml_node
    addNode <- function(node) {
        if (inherits(node, "xml_node")) {
            xml2::xml_add_child(current(), node)
        }
        invisible(NULL)
    }

    ## closeTag() -- pop the stack (close the most-recently opened tag)
    closeTag <- function() {
        if (length(stack) > 1L)
            pop()
        invisible(NULL)
    }

    ## value() -- return the root node
    value <- function() root

    list(
        addTag   = addTag,
        addNode  = addNode,
        closeTag = closeTag,
        value    = value
    )
}

xmlOutputDOM <- function(tag = "doc", attrs = NULL, ...) {
    .makeXmlDomBuilder(tag, attrs)
}

## xmlTree is used identically to xmlOutputDOM in biocViews
xmlTree <- function(tag = "doc", attrs = NULL, ...) {
    .makeXmlDomBuilder(tag, attrs)
}

## ---------------------------------------------------------------------------
## 3.  saveXML() replacement
## ---------------------------------------------------------------------------
## XML::saveXML(doc, file = NULL, prefix = "<?xml...>", ...)
##   * When 'file' is NULL/missing, returns the serialised string.
##   * When 'file' is a connection or path, writes to it.
##   * 'prefix' is prepended to the output (used for DOCTYPE in biocViews).
##
## xml2::as_xml_document() / xml2::write_html() / xml2::write_xml() are used.

saveXML <- function(doc, file = NULL, prefix = "", ...) {
    if (!inherits(doc, "xml_node"))
        stop("saveXML: 'doc' must be an xml_node")

    txt <- as.character(doc)

    ## Prepend any requested prefix (e.g. DOCTYPE declaration)
    if (nzchar(prefix))
        txt <- paste0(prefix, "\n", txt)

    if (is.null(file)) {
        return(txt)
    } else {
        writeLines(txt, con = file, sep = "")
        invisible(txt)
    }
}
