.summary_r6 <- function(x) {

  nodes <- x$nodes
  nodes_n <- length(nodes)

  cat(style(glue("Isatab of type {x$type} with {x$n} samples and {nodes_n} nodes."), "italic"))
  cat("\n")

  for (n in names(nodes)) {
    node_name <- gsub(" Name$", "", nodes[n])
    node_pos <- which(x$isa_stru$node_id == n & x$isa_stru$is_node)
    node_val <- x$isa_data[[node_pos]][1]
    node_d <- filter(x$isa_stru, .data[["node_id"]] == n & !.data[["is_node"]])
    cat(style(glue("Node {node_name} [{n}] ({node_val}...)"), "bold"))
    cat("\n")
    if (nrow(node_d) > 0) {

      for (i in 1:nrow(node_d)) {

          val <- .val_summary(x$isa_data[[node_pos + i]])
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

#' R6 Class Representing ISA-Tab Study or Assay data
#'
#' @description
#' blabla
#'
#' @details
#' bla bla
IsaTab <- R6::R6Class("IsaTab",
  public = list(

    #' @field isa_stru A data frame with the ISA-Tab structure
    isa_stru = NULL,

    #' @field isa_data A list with the ISA-Tab data
    isa_data = NULL,

    #' @field type The type of ISA-Tab object (study or assay)
    type = NULL,

    # Constructor
    #' @param type The type of ISA-Tab object (study or assay)
    initialize = function(type = "study") {
      message("IsaTab object created")
    },

    #' @param id The id of the node or property
    #' @param value The value to set
    #' @description
    #' Set a value for a node or property in the ISA-Tab data
    #' using the internal ID.
    set = function(id, value) {
      stopifnot(!is.null(self$isa_stru))
      stopifnot(all(id %in% self$isa_stru$col_id))

      if(length(id) > 1 || is(value, "data.frame")) {
        stopifnot(is(value, "data.frame"))
        stopifnot(ncol(value) == length(id))
        stopifnot(nrow(value) == self$n)
        for(i in 1:length(id)) {
          self$set(id[i], value[[i]])
        }
        return(invisible(self))
      } 

      stopifnot(is(value, "vector"))
      stopifnot(length(value) == self$n)
      message("Setting value for ", id)

      self$isa_data[[id]] <- value
      invisible(self)
    },

    #' @description
    #' Remove a node and its properties from the ISA-Tab data
    #' using the internal ID.
    #' @param node_id The id of the node to remove
    node_rm = function(node_id) {
      stopifnot(!is.null(self$isa_stru))
      stopifnot(all(node_id %in% self$isa_stru$col_id))

      sel <- self$isa_stru$col_id %in% node_id

      # check that all ids are nodes
      stopifnot(all(self$isa_stru$is_node[sel]))

      # select all nodes and their properties
      sel <- self$isa_stru$node_id %in% node_id

      self$isa_stru <- self$isa_stru[!sel, ]
      self$isa_data <- self$isa_data[ , !sel, drop = F]
      invisible(self)
    },

    #' @description
    #' Remove a property from the ISA-Tab data
    #' using the internal ID.
    #' @param prop_id The id of the property to remove
    prop_rm = function(prop_id) {
      stopifnot(!is.null(self$isa_stru))
      stopifnot(all(prop_id %in% self$isa_stru$col_id))

      sel <- self$isa_stru$col_id %in% prop_id

      # check that all ids are props
      stopifnot(all(!self$isa_stru$is_node[sel]))

      self$isa_stru <- self$isa_stru[!sel, ]
      self$isa_data <- self$isa_data[ , !sel, drop = F]
      invisible(self)
    },

    #' @description
    #' Convert IsaTab to a data frame.
    #' @return A data frame with the ISA-Tab data
    as_data_frame = function() {
      if(is.null(self$isa_stru)) {
        return(NULL)
      }
      tmp <- self$isa_data
      colnames(tmp) <- sprintf("%s [%s]", 
                               self$isa_stru[["col_name"]], 
                               self$isa_stru[["col_id"]])
      tmp
    },

    #' @description
    #' Load an assay or study from a file
    #' @param file The file to load
    load_file = function(file) {
      type <- self$type
      message("Loading ISA file, type: ", type)
      tmp <- .read_isa_assay_from_file(file, type)
      self$isa_stru <- tmp$isa_stru
      self$isa_data <- tmp$contents
      invisible(self)
    },

    #' @description
    #' Shows summary of the ISA-Tab data
    summary = function() {
      .summary_r6(self)
    },

    #' @description
    #' Print the ISA-Tab object
    print = function() {
      message("IsaTab object")
      if(is.null(self$isa_stru)) {
        cat("Empty ISA structure")
      } else {
        cat("ISA structure loaded")
      }

    }
  ),

  active = list(
    #' @field n A numeric vector with the number of samples in the ISA-Tab data
    n = function() {
      if(is.null(self$isa_stru)) {
        return(NULL)
      }
      nrow(self$isa_data)
    },

    #' @field dim A numeric vector with the dimensions of the ISA-Tab data
    dim = function() {
      if(is.null(self$isa_stru)) {
        return(NULL)
      }
      dim(self$isa_data)
    },

    #' @field nodes A character vector with the ISA-Tab nodes
    nodes = function() {
      if(is.null(self$isa_stru)) {
        return(NULL)
      }
      nodes <- self$isa_stru$is_node
      ret <- self$isa_stru$col_name[ nodes ]
      names(ret) <- self$isa_stru$node_id[ nodes ]
      ret
    },

    #' @field node_df A data frame with the ISA-Tab nodes
    node_df = function() {
      if(is.null(self$isa_stru)) {
        return(NULL)
      }
      nodes <- self$isa_stru$is_node
      df <- self$isa_data[ , nodes, drop = F ]
      colnames(df) <- self$isa_stru$col_name[ nodes ]
      df
    }
  ),

  private = list(
    data = NULL
  )
)



#' @rdname IsaTab
#' @export
`[.IsaTab` <- function(x, node, property = NULL, n = NA) {

  stopifnot(length(node) == 1L)

  if (!node %in% x$isa_stru$node_name) {
    stop(glue("No such node: \"{node}\""))
  }

  if (is.null(property)) {
    property <- node
  }

  stopifnot(length(property) == 1L)

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

  prop_id <- node_df$col_id[match(property, node_df$col_name)]

  x$isa_data[[prop_id]]
}



