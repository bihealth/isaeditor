

#' @importFrom R6 R6Class

IsaTab <- R6Class("IsaTab",
  public = list(

    isa_stru = NULL,
    isa_data = NULL,
    type = NULL,

    # Constructor
    initialize = function() {
      message("IsaTab object created")

    },

    # set value of a node or property
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

    # remove a node
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

    # remove a property
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

    # convert to a data frame
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

    # load from a file
    load_file = function(file, type = "study") {
      message("Loading ISA file, type: ", type)
      tmp <- .read_isa_assay_from_file(file, type)
      self$isa_stru <- tmp$isa_stru
      self$isa_data <- tmp$contents
    },

    # summary function
    summary = function() {

      x <- self
      nodes <- self$nodes
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
    },

    # print function
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
    # number of samples
    n = function() {
      if(is.null(self$isa_stru)) {
        return(NULL)
      }
      nrow(self$isa_data)
    },

    # dimensions of the data
    dim = function() {
      if(is.null(self$isa_stru)) {
        return(NULL)
      }
      dim(self$isa_data)
    },

    nodes = function() {
      if(is.null(self$isa_stru)) {
        return(NULL)
      }
      nodes <- self$isa_stru$is_node
      ret <- self$isa_stru$col_name[ nodes ]
      names(ret) <- self$isa_stru$node_id[ nodes ]
      ret
    },

    # isa data, but just the nodes
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





