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
#' @seealso [<- for IsaTab: \code{\link{[<-.IsaTab}} to modify elements.
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
    #' @description
    #' Get a value for a node or property in the ISA-Tab data
    #' using the internal ID.
    get = function(id) {
      stopifnot(!is.null(self$isa_stru))
      stopifnot(all(id %in% self$isa_stru$col_id))
      stopifnot(length(id) == 1)

      self$isa_data[[id]]
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
    #' Find the internal ID of a property given the node ID and property name
    #' @param node_id The id of the node
    #' @param prop_name The name of the property
    find_prop_id = function(node_id, prop_name) {
      stopifnot(!is.null(self$isa_stru))
      stopifnot(all(node_id %in% self$isa_stru$node_id))

      sel <- self$isa_stru$node_id == node_id & !self$isa_stru$is_node
      df <- self$isa_stru[sel, ]

      if(!all(prop_name %in% df$col_name)) {
        stop(glue("Properties not in node '{node_id}':\n{paste(prop_name[!prop_name %in% df$col_name], collapse='; ')}"))
      }

      ret <- df$col_id[ match(prop_name, df$col_name) ]
      names(ret) <- prop_name
      ret
    },

    #' @description
    #' Find the internal ID of a node based on its name
    #' @param node_name The name of the node
    find_node_id = function(node_name) {
      stopifnot(!is.null(self$isa_stru))
      stopifnot(node_name %in% self$isa_stru$col_name)

      self$isa_stru$node_id[ self$isa_stru$col_name == node_name ]
    },


    #' @description
    #' Find the internal ID of a node or property. If the node name is ambiguous,
    #' an error is thrown. Use the n parameter to select the node by its relative 
    #' position. For example, if there are 3 nodes called "Protocol REF", use
    #' n = 2 to select the second one.
    #' @param node_name The name of the node
    #' @param prop_name The name of the property
    #' @param n The position of the node to select (if there are multiple nodes with the same name)
    find_id = function(node_name, prop_name = NULL, n = NULL) {
      stopifnot(!is.null(self$isa_stru))
      ret <- list()

      node_ids <- self$find_node_id(node_name)

      if(length(node_ids) == 0) {
        stop(glue("No such node: {node_name}"))
      }

      if(length(node_ids) > 1) {
        if(is.null(n)) {
          stop(glue("Node name {node_name} is ambiguous (there are {length(node_ids)} nodes called {node_name}).\nPlease use `[[ID]]` or the `n` parameter."))
        }

        stopifnot(is.numeric(n) && length(n) == 1 && n > 0)

        if(n > length(node_ids)) {
          stop(glue("There are only {length(node_ids)} nodes called {node_name}."))
        }

        node_ids <- node_ids[n]
      }

      if(is.null(prop_name)) {
        ret <- node_ids
      } else {
        ret <- self$find_prop_id(node_ids, prop_name)
      }

      ret
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
summary.IsaTab <- function(object, ...) {
  object$summary()
}


#' Subset Methods for IsaTab Objects
#'
#' This page provides an overview of the subset and subset-assignment methods for objects of class `IsaTab`.
#'
#' The following methods are implemented:
#' \itemize{
#'   \item \code{\link{[.IsaTab}}: Subset elements using `[`.
#'   \item \code{\link{[[.IsaTab}}: Extract elements using `[[`.
#'   \item \code{\link{[<-.IsaTab}}: Modify elements using `[<-`.
#'   \item \code{\link{[[<-.IsaTab}}: Modify nested elements using `[[<-`.
#' }
#'
#' @section Subsetting:
#' The `[`, `[[`, and `[<-` methods allow for flexible subsetting and modification of `IsaTab` objects.
#'
#' @seealso \code{\link{IsaTab}}
#' @name IsaTab-subset-methods
#' @aliases [ IsaTab-subset [[ [<- [[<-

#' Subset ISA-Tab data by column ID
#'
#' Subset ISA-Tab data by column ID
#'
#' Extract a node or property from the ISA-Tab data by its ID. Note that in the
#' ISAtab standard, there can be multiple nodes with the same name. The
#' ISAtab class assigns unique IDs to each node and property. However,
#' these IDs are not a part of the ISAtab standard and are only used
#' internally in the R package.
#' @param x An IsaTab object
#' @param i The ID of the node or property to extract
#' @method [[ IsaTab
#' @return A data frame with the extracted node or property
#' @seealso \code{\link{IsaTab-subset-methods}}
#' @export
`[[.IsaTab` <- function(x, i) {
  x$get(i)
}

#' Modify ISA-Tab data by column ID
#'
#' Modify ISA-Tab data by column ID
#' @param x An IsaTab object
#' @param i The ID of the node or property to modify
#' @param value The value to set
#' @method [[<- IsaTab
#' @export
`[[<-.IsaTab` <- function(x, i, value) {
  x$set(i, value)
}

#' Subset ISA-Tab data by node and property name
#'
#' Subset ISA-Tab data by node and property name
#'
#' Extract a node or property from the ISA-Tab data by its name. Note that in the
#' ISAtab standard, there can be multiple nodes with the same name.
#' Therefore, if there are multiple nodes with the same name, the `n` parameter
#' can be used to select the node by its position in the ISA-Tab structure.
#' @param x An IsaTab object
#' @param node The name of the node to extract
#' @param property The name of the property to extract
#' @param n Which node to extract (if there are multiple nodes with the same name)
#' @method [ IsaTab
#' @return A data frame with the extracted node or property
#' @seealso \code{\link{IsaTab-subset-methods}}
#' @export
`[.IsaTab` <- function(x, node, property = NULL, n = NA) {

  stopifnot(length(node) == 1L)

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

  prop_id <- node_df$col_id[match(property, node_df$col_name)]

  x$isa_data[ , prop_id]
}



