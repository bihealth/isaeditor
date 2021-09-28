
## reads an assay or study file
.read_isa_assay <- function(file_name, type) {

    x <- read_delim(file_name, col_names = FALSE, delim = "\t", col_types = cols())

    isa_stru <- tibble(col_name = as.matrix(x)[1, ]) %>%
        mutate(col_id = paste0("ID", 1:n())) %>%
        mutate(is_node = grepl("( Name|Protocol REF)$", .data[["col_name"]])) %>%
        mutate(node_name = ifelse(.data[["is_node"]], .data[["col_name"]], NA)) %>%
        mutate(node_id = ifelse(.data[["is_node"]], .data[["col_id"]], NA)) %>%
        fill(.data[["node_name"]]) %>%
        fill(.data[["node_id"]])


    x <- read_delim(file_name, col_names = FALSE, delim = "\t", skip = 1, col_types = cols())
    x <- as.colorDF(x)
    colnames(x) <- isa_stru[["col_id"]]

    ret <- list(isa_stru = isa_stru, contents = x, n = nrow(x), type = type)

    class(ret) <- c("isatab")  #, class(ret))

    .check_integrity(ret)
}


#' Read or write an isatab file
#'
#' @param x isatab object
#' @param file file name to read / write
#' @param type Either 'auto', or 'investigation', 'study', 'assay' (can
#' be abbreviated)
#' @importFrom readr read_delim cols read_tsv
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom colorDF print_colorDF summary_colorDF as.colorDF
#' @importFrom rlang .data
#' @return `read_isa()` returns either an object of class `isatab` (for study / assay files) or
#' an object of class `isa_i` (for investigation files).
#' @seealso [`isatab-class`]
#' @examples
#' file <- system.file('extdata', 'i_Investigation.txt', package='isaeditor')
#' isa_i <- read_isa(file)
#' print(isa_i)
#'
#' file <- system.file('extdata', 's_isatab.txt', package='isaeditor')
#' isa_s <- read_isa(file)
#' print(isa_s)
#'
#' @export
read_isa <- function(file, type = "auto") {

    type <- match.arg(type, c("auto", "investigation", "study", "assay"))
    types_all <- c(a = "assay", s = "study", i = "investigation")

    if (type == "auto") {

        type <- substr(basename(file), 1, 1)
        if (!type %in% names(types_all)) {
            stop("unknown type")
        }

        type <- switch(type, a = "assay", s = "study", i = "investigation")
    }

    # message(glue('Type is {type}'))

    if (type == "investigation") {
        ret <- .read_investigation(file)
    } else {
        ret <- .read_isa_assay(file, type)
    }

    ret
}


isa_get_nodes <- function(x) {
    stopifnot(is(x, "isatab"))

    nodes <- which(x$isa_stru$is_node)

    ret <- x$isa_stru$node_name
    names(ret) <- x$isa_stru$node_id
    ret <- ret[!duplicated(names(ret))]
    ret
}



#' @rdname read_isa
#' @importFrom methods is
#' @importFrom readr write_delim
#' @export
write_isa <- function(x, file) {
    stopifnot(is(x, "isatab"))

    tmp <- rbind(x$isa_stru[["col_name"]], x$contents)
    tmp[is.na(tmp)] <- ""
    write_delim(tmp, file, delim = "\t", col_names = FALSE)
}


#' Show properties associated with a node ID
#'
#' Show properties associated with a node ID
#'
#' Note: IDs are a thing internal to this R package. They are not imported
#' from or exported to actual ISA-tab files. However, given that the node
#' 'identifiers' (e.g. 'Sample Name') can be ambiguous, IDs are necessary
#' to unambiguously identify a node.
#' @param x object of class isatab
#' @param node_id ID of a node
#' @return Returns a named character vector. Names are the IDs of
#' properties associated with a given node, and values are the property
#' names.
#' @seealso [`isatab-class`], [isa_nodes()]
#' @examples
#' file <- system.file('extdata', 's_isatab.txt', package='isaeditor')
#' isa_s <- read_isa(file)
#' isa_properties(isa_s, 'ID1')
#' @export
isa_properties <- function(x, node_id) {
    stopifnot(is(x, "isatab"))
    stopifnot(node_id %in% x$isa_stru$node_id)

    sel <- x$isa_stru$node_id == node_id

    ret <- x$isa_stru$col_name[sel]
    names(ret) <- x$isa_stru$col_id[sel]
    ret

}


#' Show nodes in an isatab
#'
#' Show nodes in an isatab
#'
#' Note: IDs are a thing internal to this R package. They are not imported
#' from or exported to actual ISA-tab files. However, given that the node
#' 'identifiers' (e.g. 'Sample Name') can be ambiguous, IDs are necessary
#' to unambiguously identify a node.
#' @param x object of class `isatab`
#' @seealso [`isatab-class`]
#' @importFrom dplyr group_by summarise ungroup arrange
#' @return Returns a data frame (tibble) containing columns with node ID,
#' node identifier (name), number of properties associated with that
#' node and a summary of the values for that node.
#' @seealso [`isatab-class`], [isa_properties()]
#' @examples
#' file <- system.file('extdata', 's_isatab.txt', package='isaeditor')
#' isa_s <- read_isa(file)
#' isa_nodes(isa_s)
#' @export
isa_nodes <- function(x) {
    stopifnot(is(x, "isatab"))

    nodes <- which(x$isa_stru$is_node)

    ret <- x$isa_stru %>%
        group_by(.data[["node_id"]], .data[["node_name"]]) %>%
        summarise(n_properties = n()) %>%
        ungroup() %>%
        mutate(value_summary = unlist(lapply(x$contents[, .data[["node_id"]]], .val_summary))) %>%
        arrange(as.numeric(gsub("ID", "", .data[["node_id"]])))

    ret
}


#' Add sample rows to an isatab
#'
#' Add sample rows to an isatab
#'
#' Expand the isatab by adding rows. If `total` parameter is `TRUE`, the
#' number of rows to be added will be such that the final number of rows is
#' `n`. However, if `n` is smaller than current number of rows, no rows
#' will be removed.
#' @param x an isatab object
#' @param n number of rows to add
#' @param total if TRUE, the resulting isatab object will have `n` rows.
#' @param replicate If true (default), the values in the last row of the isatabs will be replicated.
#'        Otherwise, empty rows will be added.
#' @return An object of class isatab with expanded rows
#' @importFrom tidyr uncount fill
#' @seealso [`isatab-class`]
#' @examples
#' file <- system.file('extdata', 's_isatab.txt', package='isaeditor')
#' isa_s <- read_isa(file)
#' isa_new <- isa_rows_add(isa_s, 10, total=TRUE)
#' n_row(isa_new)
#' @export 
isa_rows_add <- function(x, n, total = FALSE, replicate = TRUE) {

    stopifnot(is(x, "isatab"))
    stopifnot(n >= 0)

    if (total) {
        if (n < nrow(x$contents)) {
            warning(glue("{n} < {nrow(x$contents)}. Cowardly refusing to remove rows."))
            n <- 0
        } else {
            n <- n - nrow(x$contents)
        }
    }

    cont <- x$contents

    if (!replicate) {
        cont <- rbind(cont, rep(NA, ncol(cont)))
        cont.nnn <- c(rep(1, nrow(cont) - 1), n)
    } else {
        cont$.nnn <- c(rep(1, nrow(cont) - 1), n + 1)
    }

    cont <- uncount(cont, .data[[".nnn"]])
    cont[[".nnn"]] <- NULL

    x$contents <- as.colorDF(cont)
    x$n <- nrow(x$contents)
    # x
    .check_integrity(x)
}


## internal implementation returns the ID of the created node
.isa_node_add <- function(x, node, columns = NULL, after_node = NULL) {

    isa_stru <- x$isa_stru
    stopifnot(all(!duplicated(columns)))
    stopifnot(length(node) == 1)

    newcols <- c(node, columns)
    newnode <- matrix(NA, nrow = x$n, ncol = length(newcols))
    newnode <- as_tibble(matrix(NA, nrow = x$n, ncol = length(newcols)), .name_repair = "unique")

    newcol_ids <- .new_id(x, n = length(newcols))
    newnode_id <- newcol_ids[1]
    message(glue("New ID {newnode_id}"))

    newnode_stru <- tibble(col_name = newcols, col_id = newcol_ids, is_node = c(TRUE, rep(FALSE, length(columns))), node_name = rep(node, length(newcols)), node_id = rep(newnode_id,
        length(newcols)))

    pos <- nrow(isa_stru)

    if (!is.null(after_node)) {
        stopifnot(after_node %in% isa_stru$node_id)
        pos <- last(which(isa_stru$node_id == after_node))
    }

    newcont <- cbind(x$contents[, 1:pos], newnode)
    newstru <- rbind(isa_stru[1:pos, ], newnode_stru)

    if (pos < nrow(isa_stru)) {
        nn <- nrow(isa_stru)
        newcont <- cbind(newcont, x$contents[, (pos + 1):nn])
        newstru <- rbind(newstru, isa_stru[(pos + 1):nn, ])
    }

    x$isa_stru <- newstru
    x$contents <- newcont
    colnames(x$contents) <- x$isa_stru$col_id

    list(x = .check_integrity(x), node_id = newnode_id)
}


#' Add or remove nodes and properties
#' 
#' Add or remove nodes and properties
#'
#' These functions manipulate the structure of an isatab. `isa_node_add`
#' and `isa_node_rm` add or remove whole nodes. 
#'
#' To add or remove properties (individual columns which are not nodes) belonging to a given 
#' node, use `isa_property_add` and `isa_property_rm`.
#'
#' Adding and removing nodes is easier using brackets / subscripts. Read
#' the documentation for [`isatab-class`] for details.
#' 
#' Note: IDs are a thing internal to this R package. They are not imported
#' from or exported to actual ISA-tab files. However, given that the node
#' 'identifiers' (e.g. 'Sample Name') can be ambiguous, IDs are necessary
#' to unambiguously identify a node.
#' @param x isatab object
#' @param node new node identifier (e.g. 'Sample Name')
#' @param columns (optional) character vector with columns to add
#' @param after_node ID of the node after which the current node should be
#'        inserted
#' @importFrom tibble tibble as_tibble
#' @seealso [`isatab-class`]
#' @examples
#' file <- system.file('extdata', 's_isatab.txt', package='isaeditor')
#' isa_s <- read_isa(file)
#' isa_s <- isa_node_add(isa_s, 'Library Name', columns='Comment[Raw File]')
#' isa_nodes(isa_s)
#' isa_s <- isa_property_add(isa_s, 'Characteristics[Age]', values=c(75, 38, 43), node_id='ID1')
#' @return These functions return an object of isatab-class.
#' @export
isa_node_add <- function(x, node, columns = NULL, after_node = NULL) {

    ret <- .isa_node_add(x, node, columns, after_node)
    ret$x
}


#' @rdname isa_node_add
#' @export
isa_node_rm <- function(x, node_id) {

    stopifnot(is(x, "isatab"))
    stopifnot(length(node_id) > 0)

    if (!all(node_id %in% x$isa_stru$node_id)) {
        miss <- node_id[!node_id %in% x$isa_stru$node_id]
        miss <- paste(miss, collapse = ", ")
        stop(glue("No such node ID(s): {miss}"))
    }

    sel <- which(x$isa_stru$node_id %in% node_id)
    x$isa_stru <- x$isa_stru[-sel, ]
    x$contents <- x$contents[, -sel]
    .check_integrity(x)
}


## this internal version returns a list containing the new isatab and the ID of the newly created property
.isa_property_add <- function(x, property, values = NA, node_id = NULL, after_id = NULL) {
    stopifnot(is(x, "isatab"))

    if (length(property) > 1) {
        stopifnot(is(values, "data.frame"))
    } else {
        stopifnot(is(values, "vector"))
        values <- data.frame(values)
        names(values) <- property
    }

    # add property after the last node
    if (is.null(node_id)) {
        node_id <- last(x$isa_stru$node_id)
    }

    stopifnot(node_id %in% x$isa_stru$node_id)

    node_sel <- which(x$isa_stru$node_id == node_id)

    # add property after the last property in node
    if (is.null(after_id)) {
        after_id <- last(x$isa_stru[node_sel, ][["col_id"]])
    }

    stopifnot(after_id %in% x$isa_stru$col_id[node_sel])

    # name of the property
    node <- which(x$isa_stru$node_id == node_id & x$isa_stru$is_node)
    node_name <- x$isa_stru$node_name[node]

    # exact position at which to insert the property/properties
    pos <- which(x$isa_stru$col_id == after_id)

    # generate new id(s) for the property
    prop_ids <- .new_id(x, n = length(property))

    ret <- x

    new_prop <- tibble(col_name = property, col_id = prop_ids, is_node = FALSE, node_name = node_name, node_id = node_id)

    ret$isa_stru <- rbind(ret$isa_stru[1:pos, ], new_prop)
    ## add remainder
    if (pos < nrow(x$isa_stru)) {
        ret$isa_stru <- rbind(ret$isa_stru, x$isa_stru[(pos + 1):nrow(x$isa_stru), ])
    }

    ret$isa_stru <- as_tibble(ret$isa_stru)

    ret$contents <- cbind(x$contents[, 1:pos], values)

    ## add remainder
    if (pos < ncol(x$contents)) {
        ret$contents <- cbind(ret$contents, x$contents[, (pos + 1):ncol(x$contents)])
    }

    ret$contents <- as_tibble(ret$contents)
    ret$contents <- as.colorDF(ret$contents)
    colnames(ret$contents) <- ret$isa_stru$col_id

    ret$n <- nrow(ret$contents)

    attr(ret, "class") <- "isatab"

    .check_integrity(ret)
    list(x = ret, id = prop_ids)
}


#' @param property Character vector with identifiers (such as 'Comment\[Important\]') of the properties to be inserted
#' @param node_id For `isa_node_rm`: character vector of node
#' IDs to be removed.
#' @param node_id ID of the node in which to add the property (default:
#' last node in the isatab).
#' @param after_id ID of the property after which the parameter should
#' be inserted (deault: last property)
#' @param values vector (if only one property is added) or data frame (if
#' multiple properties are added) of values used to initialize the node /
#' parameter. If multiple properties are added with one call (the length of
#' the `property` vector is greater than one), and values is
#' a data frame, than it has to have sufficient number of columns
#' corresponding to the `property` vector.
#' @rdname isa_node_add
#' @export
isa_property_add <- function(x, property, values = NA, node_id = NULL, after_id = NULL) {
    tmp <- .isa_property_add(x, property, values = values, node_id = node_id, after_id = after_id)

    tmp$x
}


#' @param prop_ids IDs of the properties to be removed
#' @rdname isa_node_add
#' @export
isa_property_rm <- function(x, prop_ids = NULL) {

    stopifnot(is(x, "isatab"))
    stopifnot(all(prop_ids %in% x$isa_stru$col_id))

    sel <- x$isa_stru$col_id %in% prop_ids
    stopifnot(all(!x$isa_stru$is_node[sel]))

    x$isa_stru <- x$isa_stru[!sel, ]
    x$contents <- x$contents[, !sel]
    x$n <- nrow(x$contents)

    .check_integrity(x)
}


#' Find IDs of nodes or properties  
#'
#' Find IDs of nodes or properties fullfilling specified criteria
#'
#' Note: IDs are a thing internal to this R package. They are not imported
#' from or exported to actual ISA-tab files. However, given that the node
#' 'identifiers' (e.g. 'Sample Name') can be ambiguous, IDs are necessary
#' to unambiguously identify a node.
#' @param x object of class isatab
#' @param node_pattern return only nodes which match the given pattern
#' @param value_pattern return only nodes which match one of the values
#' @param prop_pattern return only nodes which match one of the properties
#' @return Character vector of IDs
#' @seealso [`isatab-class`]
#' @examples
#' file <- system.file('extdata', 
#'    'a_isatab.txt',
#'    package='isaeditor')
#' isa_a <- read_isa(file)
#' isa_ID_find(isa_a, node_pattern='.* Name')
#' isa_a[['ID34']]
#' @export
isa_ID_find <- function(x, node_pattern = NULL, value_pattern = NULL, prop_pattern = NULL) {

    stopifnot(is(x, "isatab"))
    sel_node <- sel_prop <- sel_val <- TRUE

    if (!is.null(node_pattern)) {
        sel_node <- grepl(node_pattern, x$isa_stru$node_name) & x$isa_stru$is_node
    }

    if (!is.null(prop_pattern)) {
        sel_prop <- grepl(prop_pattern, x$isa_stru$col_name) & !x$isa_stru$is_node
    }

    if (!is.null(value_pattern)) {
        sel_val <- apply(x$contents, 2, function(xx) {
            any(grepl(value_pattern, xx))
        })
    }

    x$isa_stru$col_id[sel_node & sel_prop & sel_val]

}

