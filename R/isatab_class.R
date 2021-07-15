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
#' ## Accessing columns of an isa tab
#'
#' There are two ways of accessing a column: by using the `[` function to
#' select a node identifier
#' (e.g. "Protocol REF") and, optionally, a parameter identifier (e.g.
#' "Performer"), or by using the `[[` function to select column IDs. The former has the disadvantage that
#' multiple identical node / parameter identifier combinations may exist,
#' and it may be necessary to specify which node is meant:
#'
#' ```
#' isa_s <- read_isa("s_isatab.txt")
#' isa_s[ "Sample Name" ]
#' isa_s[ "Protocol REF", "Performer" ]
#' ## 3rd instance of the combination Protocol REF / Performer
#' isa_s[ "Protocol REF", "Performer", 3 ]
#' isa_s[ "Protocol REF", "Performer", 3 ] <- "Rosalind Franklin"
#' ```
#'
#' The latter (column IDs) is not ambiguous, but column IDs are a trick
#' used by the package `isaeditor` and are not exported or read from an
#' actual ISA-tab. To view the column IDs, simply print the isatab object to the
#' screen or inspect the `isa_stru` element of the object:
#'
#' ```
#' isa_s <- read_isa("s_isatab.txt")
#' isa_s
#' isa_s$isa_stru
#' isa_s[["ID34"]]
#' isa_s[["ID34"]] <- "Rosalind Franklin"
#' ```
#' @seealso [read_isa]
#' @name isatab-class
NULL

#' @rdname isatab-class
#' @param x object of class isatab
#' @importFrom colorDF df_style df_style<- col_type<-
#' @export
print.isatab <- function(x, ...) {

  tmp <- x$contents
  colnames(tmp) <- sprintf("%s [%s]", x$isa_stru[["col_name"]], x$isa_stru[["col_id"]])

  df_style(tmp, "type.styles") <- c(df_style(tmp), list(node=list(fg="red", bg="white")))
  col_type(tmp, colnames(tmp)[ x$isa_stru[["is_node"]] ]) <- "node"

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

## provide a summary of values
.val_summary <- function(x) {
  if(all(is.na(x))) {
    return("All missing")
  }

  n_uniq <- length(unique(x))
  if(n_uniq == 1) {
    return(glue("One value: {x[1]}"))
  }

  if(n_uniq == length(x)) {
    return(glue("All unique; {x[1]}..."))
  }

  n <- length(unique(x))
  glue("{n} unique: {x[1]}...")
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
  
  cat(style(
            glue("Isatab of type {x$type} with {x$n} samples and {nodes_n} nodes."),
            "italic"
  ))
  cat("\n")

  for(n in names(nodes)) {
    node_name <- gsub(" Name$", "", nodes[n])
    node_pos  <- which(x$isa_stru$node_id == n & x$isa_stru$is_node)
    node_val  <- x$contents[[node_pos]][1]
    node_d <- filter(x$isa_stru, .data[["node_id"]] == n & !.data[["is_node"]])
    cat(style(glue("Node {node_name} [{n}] ({node_val}...)"), "bold"))
    cat("\n")
    if(nrow(node_d) > 0) {

      for(i in 1:nrow(node_d)) {

        val <- .val_summary(x$contents[[node_pos + i]])
        cn <- node_d[["col_name"]][i]
        cid <- node_d[["col_id"]][i]
        ws <- '  '

        if(grepl("^(Unit|Term)", cn)) {
          ws <- '    '
        }
        cat(glue('{ws}{cn} [{cid}] ({val})\n', .trim=FALSE))

      }
    }
  }
}


#' @param value vector with values which will be inserted into the isatab
#'        at the specified column 
#' @param node node identifier (e.g. "Sample Name")
#' @param col column identifier (e.g. "Performer")
#' @param n instance of the matching node/column pairs
#' @rdname isatab-class
#' @export
`[<-.isatab` <- function(x, node, col=NULL, n=1, value) {

  stopifnot(is(x, "isatab"))

  if(is.null(col)) {
    col <- node
  }

  if(!node %in% x$isa_stru$node_name) {
    stop(glue('No such node: "{node}"'))
  }

  sel <- which(x$isa_stru$node_name == node & x$isa_stru$col_name == col)

  if(length(sel) < 1) {
    stop(glue("No column {col} in node {node}"))
  }

  tot_n <- length(sel)
  if(tot_n > 1) {
    if(n > length(sel)) {
      n <- length(sel)
    }
    sel <- sel[n]
  }

  col_id <- x$isa_stru$col_id[sel]

  message(glue("Replacing values in node {node}, column {col}, instance {n}/{tot_n} ({col_id})"))

  x$contents[[col_id]] <- value
  x
}


#' @rdname isatab-class
#' @export
`[.isatab` <- function(x, node, col=NULL, n=1) {

  stopifnot(is(x, "isatab"))

  if(is.null(col)) {
    col <- node
  }

  if(!node %in% x$isa_stru$node_name) {
    stop(glue('No such node: "{node}"'))
  }

  sel <- which(x$isa_stru$node_name == node & x$isa_stru$col_name == col)

  if(length(sel) < 1) {
    stop(glue("No column {col} in node {node}"))
  }

  tot_n <- length(sel)
  if(tot_n > 1) {
    if(n > length(sel)) {
      n <- length(sel)
    }
    sel <- sel[n]
  }

  col_id <- x$isa_stru$col_id[sel]

  message(glue("Showing values in node {node}, column {col}, instance {n}/{tot_n} ({col_id})"))

  x$contents[[col_id]]
}

#' @param col_id Column ID (e.g. "ID34")
#' @rdname isatab-class
#' @export
`[[<-.isatab` <- function(x, col_id, value) {
  stopifnot(is(x, "isatab"))
  stopifnot(col_id %in% x$isa_stru[["col_id"]])
  sel <- which(x$isa_stru[["col_id"]] == col_id)
  message(glue("Replacing values in node {x$isa_stru$node_name[sel]}, column {x$isa_stru$col_name[sel]}"))

  x$contents[[col_id]] <- value
  x
}

#' @rdname isatab-class
#' @export
`[[.isatab` <- function(x, col_id) {

  stopifnot(col_id %in% x$isa_stru[["col_id"]])
  sel <- match(col_id, x$isa_stru[["col_id"]])

  x$contents[[sel]]
}
