#' Class for assay and study objects
#'
#' Class for isatab assay and study objects 
#'
#' Objects of this class are generated usually by reading a file with
#' [read_isa()]. 
#'
#' Internally, it is a list containing as elements a data frame (tibble)
#' describing the structure of the isa tab (`isa_stru`) and a data frame
#' (tibble) containing the actual data.
#'
#' ## Terminology
#'
#' ISA-tab *nodes* (such as 'Source Name', 'Sample Name', 'Protocol REF',
#' 'Extract Name' or 'Library Name') can have *properties*. Both are represented as
#' *columns*. In the ISA-tab specificiation, node designators such as
#' 'Sample Name' are called *identifiers*, although they need not be unique.
#' IDs are internal identifiers of the package `isaeditor`; they are unique
#' to a column. Some functions in `isaeditor` can access ISA-tab columns
#' using node / property combination; some other require the internal ID.
#'
#' ## Accessing columns (nodes and properties) of an isa tab
#'
#' Note: IDs are a thing internal to this R package. They are not imported
#' from or exported to actual ISA-tab files. However, given that the node
#' 'identifiers' (e.g. 'Sample Name') can be ambiguous, IDs are necessary
#' to unambiguously identify a node.
#'
#' There are two ways of accessing a column: by using the `[` function to
#' select a node identifier
#' (e.g. 'Protocol REF') and, optionally, a property identifier (e.g.
#' 'Performer'), or by using the `[[` function to select column IDs. The former has the disadvantage that
#' multiple identical node / property identifier combinations may exist,
#' and it may be necessary to specify which node is meant:
#'
#' ```
#' isa_a <- read_isa('a_isatab.txt')
#' isa_a[ 'Sample Name' ]
#' isa_a[ 'Protocol REF', 'Performer' ]
#' ## 3rd instance of the combination Protocol REF / Performer
#' isa_a[ 'Protocol REF', 'Performer', n=3 ]
#' isa_a[ 'Protocol REF', 'Performer', n=3 ] <- 'Rosalind Franklin'
#' ```
#' 
#' Assigning a NULL value to a selected node is equivalent to removing this
#' node and all its properties.
#'
#' Assigning a NULL value to a property is equivalent with removing this
#' property.
#'
#' Using column IDs with the `[[` function is not ambiguous, but column IDs are a trick
#' used by the package `isaeditor` and are not exported or read from an
#' actual ISA-tab. To view the column IDs, simply print the isatab object to the
#' screen or inspect the `isa_stru` element of the object:
#'
#' ```
#' isa_s <- read_isa('s_isatab.txt')
#' isa_s
#' isa_s$isa_stru
#' isa_s[['ID21']]
#' isa_s[['ID21']] <- 'Rosalind Franklin'
#' ```
#'
#' Both `[` and `[[` return a vector if a single column is specified and a data
#' frame if more than one column is selected.
#'
#' ## Creating and removing nodes and properties
#'
#' Nodes and properties can either be created with [isa_node_add()] and
#' [isa_property_add()] or with assigning a value to a new node with `[<-`:
#'
#' ```
#' isa_a['Test Node'] <- c(1, 2, 3)
#' isa_a['Test Node', 'Test Property'] <- 5:7
#' ```
#'
#' In the above code, first the node `Test Node` was created and filled
#' with values 1:3, and then the property `Test Property` was created and
#' filled with 5:7. This can be shortened by assigning a data frame in one
#' step:
#'
#' ```
#' isa_a['Test Node', 'Test Property'] <- data.frame(1:3, 5:7)
#' ```
#'
#' A column ID can be specified to insert the node at a position relative
#' to another node, or the property at a position relative to another
#' property:
#'
#' ```
#' isa_a[ 'Test Node', after_id='ID1' ] <- 1:3
#' ```
#'
#' Removing nodes and properties works by assigning `NULL` to either a node
#' (in which case all node properties will be removed as well) or a
#' property.
#'
#' ```
#' # remove only one property
#' isa_a['Test Node', 'Test Property'] <- NULL
#' # remove node and its properties
#' isa_a['Test Node'] <- NULL
#' ```
#' @seealso [read_isa()] [isa_ID_find()]
#' @name isatab-class
NULL

#' @rdname isatab-class
#' @param x object of class isatab
#' @export
dim.isatab <- function(x) {
  return(dim(x$contents))
}

#' @rdname isatab-class
#' @param x object of class isatab
#' @importFrom colorDF df_style df_style<- col_type<-
#' @export
print.isatab <- function(x, ...) {

    tmp <- x$contents
    tmp <- as.colorDF(tmp)
    colnames(tmp) <- sprintf("%s [%s]", x$isa_stru[["col_name"]], x$isa_stru[["col_id"]])

    df_style(tmp, "type.styles") <- c(df_style(tmp), list(node = list(fg = "red", bg = "white")))
    col_type(tmp, colnames(tmp)[x$isa_stru[["is_node"]]]) <- "node"

    print(tmp)
}

#' @rdname isatab-class
#' @export
as.data.frame.isatab <- function(x, ...) {

    tmp <- x$contents
    colnames(tmp) <- sprintf("%s [%s]", x$isa_stru[["col_name"]], x$isa_stru[["col_id"]])
    as.data.frame(tmp)

}

#' @rdname isatab-class
#' @export
as_tibble.isatab <- function(x, ...) {

    tmp <- x$contents
    colnames(tmp) <- sprintf("%s [%s]", x$isa_stru[["col_name"]], x$isa_stru[["col_id"]])
    as_tibble(tmp)

}

#' Generic replacement for `nrow()`
#'
#' Generic replacement for `nrow()`
#' @param x an array-like object
#' @export
n_row <- function(x) {
    UseMethod("n_row", x)
}



#' @export
n_row.default <- function(x) {
    nrow(x)
}

#' @rdname isatab-class
#' @export
n_row.isatab <- function(x) {
    stopifnot(is(x, "isatab"))

    x$n
}


#' Print generic summary
#' @param object object of class isatab
#' @param ... any further arguments are ignored
#' @importFrom dplyr filter n last mutate 
#' @importFrom crayon style
#' @rdname isatab-class
#' @export 
summary.isatab <- function(object, ...) {
    x <- object
    nodes <- isa_get_nodes(x)
    nodes_n <- length(nodes)

    cat(style(glue("Isatab of type {x$type} with {x$n} samples and {nodes_n} nodes."), "italic"))
    cat("\n")

    for (n in names(nodes)) {
        node_name <- gsub(" Name$", "", nodes[n])
        node_pos <- which(x$isa_stru$node_id == n & x$isa_stru$is_node)
        node_val <- x$contents[[node_pos]][1]
        node_d <- filter(x$isa_stru, .data[["node_id"]] == n & !.data[["is_node"]])
        cat(style(glue("Node {node_name} [{n}] ({node_val}...)"), "bold"))
        cat("\n")
        if (nrow(node_d) > 0) {

            for (i in 1:nrow(node_d)) {

                val <- .val_summary(x$contents[[node_pos + i]])
                cn <- node_d[["col_name"]][i]
                cid <- node_d[["col_id"]][i]
                ws <- "  "

                if (grepl("^(Unit|Term)", cn)) {
                  ws <- "    "
                }
                cat(glue("{ws}{cn} [{cid}] ({val})\n", .trim = FALSE))

            }
        }
    }
}

## remove selected properties – helper function for [<-
.props_remove <- function(x, property, node, node_id) {

    node_df <- x$isa_stru[x$isa_stru$node_id == node_id, ]
    pp <- paste(property, collapse = ", ")
    message(glue("Removing properties '{pp}' from node '{node}' [{node_id}]"))
    for (prop in property) {
        if (!prop %in% node_df$col_name) {
            message(glue("Property '{prop}' does not exist, skipping"))
        } else {
            message(glue("Removing property '{prop}' from node '{node}' [{node_id}]"))
            .col_id <- x$isa_stru$col_id[match(prop, x$isa_stru$col_name)]
            x <- isa_property_rm(x, .col_id)
        }
    }

    x
}

## fill selected props of node node_id with values helper function for [<-
.props_modify <- function(x, property, node, node_id, after_id, value) {
    # isa_stru of the node_id only
    node_df <- x$isa_stru[x$isa_stru$node_id == node_id, ]

    for (prop in property) {
        if (!prop %in% node_df$col_name) {
            if (is.null(after_id)) {
                after_id <- last(node_df$col_id)
            }
            stopifnot(after_id %in% node_df$col_id)
            message(glue("Creating property {prop} for node {node} [{node_id}] after column [{after_id}]"))
            if (is(value, "data.frame")) {
                stopifnot(length(value) > 0)
                .val <- value[[1]]
                value[[1]] <- NULL
            } else {
                .val <- value
            }
            tmp <- .isa_property_add(x, prop, .val, node_id = node_id, after_id = after_id)
            x <- tmp$x
            after_id <- tmp$id
            stopifnot(length(after_id) == 1)
            node_df <- x$isa_stru[x$isa_stru$node_id == node_id, ]
        } else {
            message(glue("Modyfying property {prop} for node {node} [{node_id}]"))
            .col_id <- node_df$col_id[match(prop, node_df$col_name)]
            if (is(value, "data.frame")) {
                .val <- value[[1]]
                value[[1]] <- NULL
            } else {
                .val <- value
            }
            stopifnot(!is.null(.val))
            x[[.col_id]] <- .val
        }

    }
    x
}

## helper function for [<- for when a node needs to be created
.node_new <- function(x, node, after_id, value) {
    sel <- x$isa_stru$is_node & x$isa_stru$node_name == node
    # create a new node but after which node?
    if (is.null(after_id)) {
        after_id <- last(x$isa_stru$node_id)
    } else {
        after_id <- x$isa_stru$node_id[x$isa_stru$col_id == after_id]
    }
    after_name <- .col_id_to_name(x, after_id)
    message(glue("Adding node {node} after node {after_name} [{after_id}]"))

    # the internal version gives us the new node id
    tmp <- .isa_node_add(x, node, after_node = after_id)
    x <- tmp$x
    node_id <- tmp$node_id

    tmp2 <- .col_insert_value(x, node_id, value)

    c(tmp2, list(node_id = node_id))
}

## helper function for [<- and [[<- inserts value into a column, *using up* columns if it is a data frame
.col_insert_value <- function(x, col_id, value) {

    if (is(value, "data.frame")) {
        .val <- value[[1]]
        value[[1]] <- NULL
    } else {
        .val <- value
    }

    ## populate the new node with values
    stopifnot(!is.null(.val))
    x[[col_id]] <- .val

    list(x = x, value = value)
}

## checks node selection for ambiguity, that is multiple nodes with the same name
.check_sel <- function(x, sel, node, n) {

    if (sum(sel) > 1) {
        if (is.na(n)) {
            stop(glue("Node name {node} is ambiguous (there are {sum(sel)} nodes called {node}).\nPlease use `[[ID]]` or the `n` parameter."))
        } else {
            if (n > sum(sel)) {
                n <- sum(sel)
            }
            sel <- which(sel)[n]
            message(glue("Selecting node {node} [{x$isa_stru$node_id[sel]}], {n} out of {sum(sel)}"))
        }
    }

    return(sel)
}

#' @param value vector or data frame with values which will be inserted into the isatab
#'        at the specified column. 
#' @param node node column (e.g. 'Sample Name')
#' @param new force creating a new node even if there is already a node
#'        with such an identifier
#' @param property property column (e.g. 'Performer')
#' @param n instance of the node identifier (if there are multiple
#' identical node identifiers in the isatab, for example multiple 'Extract
#' Name' nodes).
#' @param after_id ID of an existing column. If a column (node or property) needs to be created,
#' after_id can be used to specify after which node / column the new column
#' will be inserted.
#' @rdname isatab-class
#' @export
`[<-.isatab` <- function(x, node, property = NULL, new = FALSE, n = NA, after_id = NULL, value) {

    stopifnot(is(x, "isatab"))
    ## first, find the node_id
    sel <- x$isa_stru$is_node & x$isa_stru$node_name == node

    ## instead of a convoluted if/else structure, we have here multiple mutually exclusive if statemets for verbosity

    if ((new || !any(sel)) && is.null(value)) {
        ## NULL nodes are to be deleted, so just return the object
        return(x)
    }

    if (new || !any(sel)) {
        # create a new node, adding values
        tmp <- .node_new(x, node, after_id, value)
        node_id <- tmp$node_id
        value <- tmp$value
        x <- tmp$x
        after_id <- node_id
    }

    if (!(new || !any(sel))) {
        # selecting an existing node
        sel     <- .check_sel(x, sel, node, n)
        node_id <- x$isa_stru$node_id[sel]

        if (is.null(property) && !is.null(value)) {
            tmp <- .col_insert_value(x, node_id, value)
            value <- tmp$value
            x <- tmp$x
        }
    }

    if (is.null(property) && is.null(value)) {
        message(glue("Removing node {node} [{node_id}]"))
        x <- isa_node_rm(x, node_id)
    }


    if (!is.null(property) && is.null(value)) {
        x <- .props_remove(x, property, node, node_id)
    }

    if (!is.null(property) && !is.null(value)) {
        if (!is.null(after_id) && !after_id %in% x$isa_stru$col_id[x$isa_stru$col_id == node_id]) {
            stop(glue("after_id=[{after_id}] is not a property of node '{node}' [{node_id}]"))
        }

        pp <- paste(property, collapse = ", ")
        message(glue("Modifying / creating properties '{pp}'..."))

        x <- .props_modify(x, property, node, node_id, after_id, value)
    }

    .check_integrity(x)
}


#' @rdname isatab-class
#' @export
`[.isatab` <- function(x, node, property = NULL, n = NA) {

    stopifnot(is(x, "isatab"))

    if (!node %in% x$isa_stru$node_name) {
        stop(glue("No such node: \"{node}\""))
    }

    if (is.null(property)) {
        property <- node
    }

    sel <- x$isa_stru$is_node & x$isa_stru$node_name == node

    if (sum(sel) > 1L) {
        if (is.na(n)) {
            stop(glue("Node name {node} is ambiguous (there are {sum(sel)} nodes called {node}).\nPlease use `[[ID]]` or the `n` parameter."))
        }
        if (n > sum(sel)) {
            n <- sum(sel)
        }
        sel <- which(sel)[n]
    }

    node_id <- x$isa_stru$node_id[sel]
    node_df <- x$isa_stru[x$isa_stru$node_id == node_id, ]

    if (!all(property %in% node_df$col_name)) {
        miss <- property[!property %in% node_df$col_name]
        stop(glue("Properties not in node '{node}' [{node_id}]:\n{paste(miss, collapse='; ')}"))
    }

    prop_ids <- node_df$col_id[match(property, node_df$col_name)]

    ret <- x[[prop_ids]]
    if (is(ret, "data.frame")) {
        colnames(ret) <- property
    }
    ret
}

## return only node ids from col_id
.filter_nodes <- function(x, col_id) {
    cid <- col_id
    x$isa_stru$col_id[x$isa_stru$col_id %in% cid & x$isa_stru$is_node]
}

## return only property ids from col_id
.filter_props <- function(x, col_id) {
    cid <- col_id
    x$isa_stru$col_id[x$isa_stru$col_id %in% cid & !x$isa_stru$is_node]

}

#' @param col_id Column ID (e.g. 'ID34')
#' @rdname isatab-class
#' @export
`[[<-.isatab` <- function(x, col_id, value) {
    stopifnot(is(x, "isatab"))
    stopifnot(all(col_id %in% x$isa_stru[["col_id"]]))

    if (is.null(value)) {
        ## removing col_id / node
        cid <- col_id
        x$isa_stru <- x$isa_stru %>%
            filter(!.data[["node_id"]] %in% cid) %>%
            filter(!.data[["col_id"]] %in% cid)
        x$contents <- x$contents[, x$isa_stru$col_id]
    } else {
        x$contents[, col_id] <- value
    }
    .check_integrity(x)
}

#' @rdname isatab-class
#' @export
`[[.isatab` <- function(x, col_id) {

    stopifnot(length(col_id) > 0)
    stopifnot(all(col_id %in% x$isa_stru[["col_id"]]))
    sel <- match(col_id, x$isa_stru[["col_id"]])

    if (length(sel) > 1) {
        return(x$contents[, col_id])
    } else {
        return(x$contents[[col_id]])
    }
}



