

#' Read or write an isatab file
#'
#' @param x isatab object
#' @param file file name to read / write
#' @param type Either "auto", or "investigation", "study", "assay" (can
#' be abbreviated)
#' @importFrom readr read_delim
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom colorDF print_colorDF summary_colorDF as.colorDF
#' @export
read_isa <- function(file, type="auto") {

  type <- match.arg(type, c("auto", "investigation", "study", "assay"))
  types_all <- c(a="assay", s="study", i="investigation")

  if(type == "auto") {

    type <- substr(basename(file), 1, 1)
    if(!type %in% names(types_all)) {
      stop("unknown type")
    }

    type <- c(a="assay", s="study", i="investigation")[ type ]
  }

  message(glue("Type is {type}"))

  x <- read_delim(file, col_names=FALSE, delim="\t", col_type=cols())

  isa_stru <- tibble(col_name=as.matrix(x)[1, ]) %>%
    mutate(is_node=grepl("( Name|Protocol REF)$", .data[["col_name"]])) %>%
    mutate(node_name=ifelse(.data[["is_node"]], .data[["col_name"]], NA)) %>%
    mutate(node_id=ifelse(.data[["is_node"]], paste0("ID", 1:n()), NA)) %>%
    fill(.data[["node_name"]]) %>%
    fill(.data[["node_id"]])


  x <- read_delim(file, col_names=FALSE, delim="\t", skip=1, col_type=cols())

  ret <- list(
              isa_stru=isa_stru,
              contents=x,
              n=nrow(x),
              type=type
  )

  class(ret) <- c("isatab") #, class(ret))

  return(ret)
}


#' @export
print.isatab <- function(x, ...) {

  tmp <- as.colorDF(x$contents)
  colnames(tmp) <- x$isa_stru[[1]]
  print(tmp)
}

isa_get_nodes <- function(x) {
  stopifnot(is(x, "isatab"))

  nodes <- which(x$isa_stru$is_node)

  ret <- x$isa_stru$node_name
  names(ret) <- x$isa_stru$node_id
  ret <- ret[ !duplicated(names(ret)) ]
  ret
}

## provide a summary of values
val_summary <- function(x) {
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
#' @importFrom dplyr filter n
#' @export 
summary.isatab <- function(x, ...) {
  nodes <- isa_get_nodes(x)
  nodes_n <- length(nodes)
  
  cat(glue("Isatab of type {x$type} with {x$n} samples and {nodes_n} nodes.\n", .trim=FALSE))

  for(n in names(nodes)) {
    node_name <- gsub(" Name$", "", nodes[n])
    node_pos  <- which(x$isa_stru$node_id == n & x$isa_stru$is_node)
    node_val  <- x$contents[[node_pos]][1]
    node_d <- filter(x$isa_stru, node_id == n & !is_node)
    cat(glue("Node {node_name} ID {n} ({node_val}...)\n", .trim=FALSE))
    if(nrow(node_d) > 0) {

      for(i in 1:nrow(node_d)) {

        val <- val_summary(x$contents[[node_pos + i]])
        cn <- node_d[["col_name"]][i]
        ws <- '  '

        if(grepl("^(Unit|Term)", cn)) {
          ws <- '    '
        }
        cat(glue('{ws}{cn} ({val})\n', .trim=FALSE))

      }
    }
  }
}


isa_mod <- function(x, replacement, node, col=NULL) {
  stopifnot(is(x, "isatab"))

  stopifnot(length(replacement) == x$n)

  if(is.null(col)) {
    col <- node
  }

  stopifnot(sum(x$isa_stru$col_name == col & x$isa_stru$node_name == node) == 1)

  col_pos <- which(x$isa_stru$col_name == col & x$isa_stru$node_name == node)

  x$contents[[col_pos]] <- replacement
  return(x)
}

#' @rdname read_isa
#' @importFrom methods is
#' @export
write_isa <- function(x, file) {
  stopifnot(is(x, "isatab"))

  tmp <- rbind(x$isa_stru[["col_name"]], x$contents)   
  tmp[is.na(tmp)] <- ""
  write_delim(tmp, file, delim="\t", col_names=FALSE)
}




#' @export
`[.isatab` <- function(x, node, col=NULL, n=1) {

  if(is.null(col)) {
    col <- node
  }

  sel <- which(x$isa_stru$node_name == node & x$isa_stru$is_node)

  if(length(sel) < 1) {
    stop(glue('No such node: "{node}"'))
  }

  if(length(sel) > 1) {
    message(glue("Showing {n} out of {length(sel)} [{sel[n]}]"))
    sel <- sel[n]
  }

  node_id <- x$isa_stru$node_id[sel]
  message(node_id)

  sel <- which(x$isa_stru$node_id == node_id & x$isa_stru$col_name == col)

  if(length(sel) < 1) {
    stop(glue('No column "{col}" in node: "{node}"'))
  }

  x$contents[[sel]]
}


#' Add sample rows to an isatab
#'
#' Add sample rows to an isatab
#' @param x an isatab object
#' @param n number of rows to add
#' @param replicate If true (default), the values in the last row of the isatabs will be replicated.
#'        Otherwise, empty rows will be added.
#' @return isatab with expanded rows
#' @importFrom tidyr uncount
#' @export 
isa_add_rows <- function(x, n, replicate=TRUE) {

  stopifnot(is(x, "isatab"))

  cont <- x$contents

  if(!replicate) {
    cont <- rbind(cont, rep(NA, ncol(cont)))
    cont.nnn <- c(rep(1, nrow(cont) - 1), n)
  } else {
    cont$.nnn <- c(rep(1, nrow(cont) - 1), n + 1)
  }

  cont <- uncount(cont, .data[[".nnn"]])

  x$contents <- cont
  x$n <- nrow(x$contents)
  x
}


#' Insert a node to the isatab
#' 
#' @param x isatab object
#' @param node new node identifier (e.g. "Sample Name")
#' @param columns (optional) character vector with columns to add
#' @param after ID of the node after which the current node should be
#'        inserted
#' @importFrom dplyr last
#' @importFrom tibble tibble
#' @export
isa_add_node <- function(x, node, columns=NULL, after=NULL)  {

  isa_stru <- x$isa_stru

  newcols <- c(node, columns)
  newnode <- as_tibble(matrix(NA, nrow=x$n, ncol=length(newcols)), .name_repair="unique")

  newnode_id <- max(as.numeric(gsub("ID", "", isa_stru$node_id))) + 1
  newnode_id <- paste0("ID", newnode_id)
  message(glue("New ID {newnode_id}"))

  newnode_stru <- tibble(
                         col_name=newcols,
                         is_node=c(TRUE, rep(FALSE, length(columns))),
                         node_name=rep(node, length(newcols)),
                         node_id=rep(newnode_id, length(newcols)))

  pos <- nrow(isa_stru)

  if(!is.null(after)) {
    stopifnot(after %in% isa_stru$node_id)
    pos <- last(which(isa_stru$node_id == after))
  }

  newcont <- cbind(x$contents[, 1:pos], newnode)
  newstru <- rbind(isa_stru[1:pos, ], newnode_stru)

  if(pos < nrow(isa_stru)) {
    nn <- nrow(isa_stru)
    newcont <- cbind(newcont, x$contents[, (pos + 1):nn])
    newstru <- rbind(newstru, isa_stru[(pos + 1):nn, ])
  }

  x$isa_stru <- newstru
  x$contents <- newcont

  x

}




