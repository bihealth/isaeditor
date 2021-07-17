#' Lists all the nodes in an isatab object
#'
#' Lists all the nodes in an isatab object
#' 
#' `node_list` returns a data frame with one row per node, showing the
#' number of properties associated with a given node and a summary of
#' values for that node.
#'
#' `node_show` returns a data frame for a given `node_id` listing all
#' properties associated with that node and a summary of values for each of
#' the properties.
#'
#' `node_select` returns a new object of class isatab containing only the
#' selected nodes.
#'
#' `prop_select` returns a new object of class isatab containing only the
#' selected property IDs (which may not be node IDs!).
#'
#' @examples 
#' file <- system.file("extdata", "s_isatab.txt", package="isaeditor")
#' isa_s <- read_isa(file)
#' node_list(isa_s)
#' @return a data.frame like object
#' @param x object of class isatab
#' @export
node_list <- function(x) {
  stopifnot(is(x, "isatab"))

  ret <- x$isa_stru %>% 
    group_by(.data[["node_id"]], .data[["node_name"]]) %>%
    summarise(n_prop=n()) %>% ungroup()
  val_sum <- unlist(lapply(ret$node_id, function(id) .val_summary(x$contents[[id]])))
  ret[["value_summary"]] <- val_sum
  ret
}


#' @param node_id ID of a node to show
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#' @rdname node_list
#' @export
node_show <- function(x, node_id) {
  stopifnot(is(x, "isatab"))
  stopifnot(all(node_id %in% x$isa_stru$node_id))
  nid <- node_id
  ret <- x$isa_stru %>% filter(.data[["node_id"]] %in% nid) %>%
    select(all_of(c("col_id", "col_name", "is_node")))
  val_sum <- unlist(lapply(ret$col_id, function(id) .val_summary(x$contents[[id]])))
  ret[["value_summary"]] <- val_sum
  ret

}


#' @rdname node_list
#' @export
node_select <- function(x, node_id, inverse=FALSE) {
  stopifnot(is(x, "isatab"))
  stopifnot(all(node_id %in% x$isa_stru$node_id))

  if(inverse) {
    node_id <- x$isa_stru$node_id[ 
                                  x$isa_stru$is_node &
                                  !x$isa_stru$node_id %in% node_id ]
  }

  nid <- node_id
  x$isa_stru <- x$isa_stru %>% filter(.data[["node_id"]] %in% nid)
  x$contents <- x$contents[ , x$isa_stru$col_id ]
  .check_integrity(x)
}

#' @param prop_id property IDs to be selected
#' @param inverse if TRUE, inverses the selection
#' @rdname node_list
#' @export
prop_select <- function(x, prop_id, inverse=FALSE) {
  stopifnot(is(x, "isatab"))
  stopifnot(all(prop_id %in% x$isa_stru$col_id))

  stopifnot(all(!x$isa_stru$is_node[ x$isa_stru$col_id %in% prop_id ]))

  if(inverse) {
    prop_id <- x$isa_stru$col_id[ (!x$isa_stru$is_node) & (!x$isa_stru$col_id %in% prop_id) ]
  }

  x$isa_stru <- x$isa_stru %>% filter(.data[["col_id"]] %in% prop_id | .data[["is_node"]])
  x$contents <- x$contents[ , x$isa_stru$col_id ]
  .check_integrity(x)
}



